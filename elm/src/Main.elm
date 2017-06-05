module Main exposing (..)

import Html exposing (programWithFlags)
import Types exposing (..)
import Ports
import Msg exposing (..)
import Dom
import Task
import View exposing (view)
import Utils exposing (..)


-- import List exposing (..)


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


initialNewWord : Word
initialNewWord =
    { text = "", soundUrl = "", id = -1 }


type alias Flags =
    { words : Words
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        words =
            flags.words
    in
        ( Model words words initialNewWord Show Nothing, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WordText id text ->
            ( { model | words = (updateText id text model.words) |> deleteEmptyWords }, Cmd.none )

        WordSoundUrl id soundUrl ->
            ( { model | words = (updateSoundUrl id soundUrl model.words) |> deleteEmptyWords }, Cmd.none )

        Play id ->
            ( { model | playingWordId = Just id }, Cmd.none )

        PlayEnded ->
            ( { model | playingWordId = Nothing }, Cmd.none )

        NewWordText text ->
            addNewWord model text

        EditMode ->
            ( { model | mode = Edit }, Task.attempt FocusResult (Dom.focus "new-word") )

        Cancel ->
            ( { model | words = model.prevWords, mode = Show }, Cmd.none )

        Save ->
            ( { model | mode = Show }, Ports.save model.words )

        FocusResult result ->
            ( model, Cmd.none )


updateText : Int -> String -> Words -> Words
updateText id text words =
    updateWord (\word -> { word | text = text }) id words


updateSoundUrl : Int -> String -> Words -> Words
updateSoundUrl id soundUrl words =
    updateWord (\word -> { word | soundUrl = soundUrl }) id words


updateWord : (Word -> Word) -> Int -> Words -> Words
updateWord func id words =
    let
        f w =
            if id == w.id then
                func w
            else
                w
    in
        List.map f words


getWordById : Words -> Int -> Maybe Word
getWordById words id =
    words |> List.filter (\w -> w.id == id) |> List.head


addNewWord : Model -> String -> ( Model, Cmd Msg )
addNewWord model text =
    let
        newId =
            (maxWordId model.words) + 1
    in
        ( { model
            | words = List.append model.words [ { initialNewWord | id = newId, text = text } ]
            , newWord = initialNewWord
          }
        , Task.attempt FocusResult (Dom.focus (wordDomId newId))
        )


deleteEmptyWords : Words -> Words
deleteEmptyWords words =
    words
        |> List.filter (\w -> w.text /= "" || w.soundUrl /= "")
        |> sortWords


sortWords : Words -> Words
sortWords words =
    words
        |> List.indexedMap (\index w -> { w | id = index })


maxWordId : Words -> Int
maxWordId words =
    case List.maximum (List.map (\w -> w.id) words) of
        Nothing ->
            0

        Just value ->
            value
