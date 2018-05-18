module Main exposing (..)

import Dom
import Html exposing (programWithFlags)
import Msg exposing (..)
import Ports
import Random
import Random.List exposing (shuffle)
import Task
import Time exposing (Time, millisecond)
import Types exposing (..)
import Utils exposing (..)
import View exposing (view)


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


type alias Flags =
    { words : Words
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
      words = wordsOrDefault flags.words
    in
      ( Model words words Show Nothing (PlayAll 0 0 0), Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Time.every (100 * millisecond) Tick
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WordText id text ->
            ( { model | words = updateText id text model.words }, Cmd.none )

        WordSoundUrl id soundUrl ->
            ( { model | words = updateSoundUrl id soundUrl model.words }, Cmd.none )

        Play id ->
            ( { model | playingWordId = Just id }, Cmd.none )

        AudioPlayEnded ->
            ( { model | playingWordId = Nothing }, Cmd.none )

        AudioError ->
            ( { model | playingWordId = Nothing }, Cmd.none )

        AddWord ->
            addNewWord model

        DeleteWord id ->
            ( { model | words = deleteWord model.words id }, Cmd.none )

        EditMode ->
            ( { model | mode = Edit }, Task.attempt FocusResult (Dom.focus "new-word") )

        HideMode ->
            ( { model | mode = Hide }, Cmd.none )

        ShowMode ->
            ( { model | mode = Show }, Cmd.none )

        Cancel ->
            ( { model | words = model.prevWords, mode = Show }, Cmd.none )

        Save ->
            ( { model | words = model.words |> deleteEmptyWords, mode = Show }, Ports.save model.words )

        FocusResult result ->
            ( model, Cmd.none )

        Shuffle ->
            ( model, Random.generate ShuffledList (shuffle model.words) )

        ShuffledList shuffledList ->
            ( { model | words = shuffledList, mode = Hide }, Cmd.none )

        PlayAllWords ->
            doNothing model



-- Tick newTime ->
--     ( { model | playAll = playAllNewTime model.playAll newTime }, Cmd.none )


playAllNewTime : PlayAll -> Time -> PlayAll
playAllNewTime playAll newTime =
    { playAll | time = newTime }


doNothing : Model -> ( Model, Cmd Msg )
doNothing model =
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


addNewWord : Model -> ( Model, Cmd Msg )
addNewWord model =
    let
        newId =
            maxWordId model.words + 1
    in
    ( { model | words = List.append model.words [ { text = "", soundUrl = "", id = newId } ] }
    , Task.attempt FocusResult (Dom.focus (wordDomId newId))
    )


deleteEmptyWords : Words -> Words
deleteEmptyWords words =
    words
        |> List.filter (\w -> w.text /= "" || w.soundUrl /= "")
        |> sortWords


deleteWord : Words -> Int -> Words
deleteWord words id =
    words |> List.filter (\w -> w.id /= id) |> sortWords


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

wordsOrDefault : Words -> Words
wordsOrDefault words =
    if List.isEmpty(words) then
        [ ( Word "word" "" 1 ) ]
    else
        words
