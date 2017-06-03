module Main exposing (..)

import Html exposing (Html, programWithFlags, ul, div, input, text, button, a, table, tr, td)
import Html.Attributes exposing (type_, placeholder, value, href, target)
import Html.Events exposing (onInput, onClick)
import Types exposing (..)
import Ports
import Msg exposing (..)
import Audio exposing (playAudio)


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
            ( addNewWord model text, Cmd.none )

        EditMode ->
            ( { model | mode = Edit }, Cmd.none )

        Cancel ->
            ( { model | words = model.prevWords, mode = Show }, Cmd.none )

        Save ->
            ( { model | mode = Show }, Ports.save model.words )


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


addNewWord : Model -> String -> Model
addNewWord model text =
    { model
        | words = List.append model.words [ { initialNewWord | id = (maxWordId model.words) + 1, text = text } ]
        , newWord = initialNewWord
    }


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



-- VIEW


view : Model -> Html Msg
view model =
    case model.mode of
        Show ->
            show model

        Edit ->
            edit model


show : Model -> Html Msg
show model =
    div []
        [ table [] (List.map (\w -> (decoratedWord model w) |> showWord) model.words)
        , button [ onClick EditMode ] [ text "Edit" ]
        ]


decoratedWord : Model -> Word -> DecoratedWord
decoratedWord model word =
    { text = word.text
    , soundUrl = word.soundUrl
    , id = word.id
    , playing = ((Just word.id) == model.playingWordId)
    , canPlay = model.playingWordId == Nothing
    }


showWord : DecoratedWord -> Html Msg
showWord word =
    tr []
        [ td [] [ text word.text ]
        , td [] [ dictionaryLink word ]
        , td [] [ playButton word ]
        ]


edit : Model -> Html Msg
edit model =
    div []
        [ table []
            (List.append (List.map (\w -> (decoratedWord model w) |> editWord) model.words) [ editNewWord model.newWord ])
        , button [ onClick Save ] [ text "Save" ]
        , button [ onClick Cancel ] [ text "Cancel" ]
        ]


editWord : DecoratedWord -> Html Msg
editWord word =
    tr []
        [ td [] [ input [ type_ "text", placeholder "text", onInput (WordText word.id), value word.text ] [] ]
        , td [] [ input [ type_ "text", placeholder "sound URL", onInput (WordSoundUrl word.id), value word.soundUrl ] [] ]
        , td [] [ dictionaryLink word ]
        , td [] [ playButton word ]
        , td [] [ text ((toString word.id) ++ " | " ++ word.text ++ " | " ++ word.soundUrl) ]
        ]


editNewWord : Word -> Html Msg
editNewWord word =
    div []
        [ input [ type_ "text", placeholder "new word", onInput NewWordText, value word.text ] []
        ]


dictionaryLink : DecoratedWord -> Html Msg
dictionaryLink word =
    a [ href ("http://www.collinsdictionary.com/dictionary/english/" ++ word.text), target "_blank" ]
        [ text "dictionary" ]


playButton : DecoratedWord -> Html Msg
playButton word =
    if word.playing then
        div []
            [ text "playing"
            , playAudio word.soundUrl PlayEnded
            ]
    else if word.canPlay then
        a [ href "#", onClick (Play word.id) ] [ text "play" ]
    else
        text "play"
