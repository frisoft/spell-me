port module Main exposing (..)

import Html exposing (Html, programWithFlags, ul, div, input, text, button)
import Html.Attributes exposing (type_, placeholder, value)
import Html.Events exposing (onInput, onClick)
import Types exposing (..)
import Ports


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


model : Model
model =
    { words =
        [ { text = "Please", soundUrl = "", id = 0 }
        , { text = "Thanks", soundUrl = "", id = 1 }
        ]
    , prevWords = []
    , newWord = initialNewWord
    , mode = Show
    }



-- INIT


type alias Flags =
    { words : Words
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        words =
            flags.words
    in
        ( Model words words initialNewWord Show, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = WordText Int String
    | WordSoundUrl Int String
    | NewWordText String
    | EditMode
    | Cancel
    | Save


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WordText id text ->
            ( { model | words = (updateText id text model.words) |> deleteEmptyWords }, Cmd.none )

        WordSoundUrl id soundUrl ->
            ( { model | words = (updateSoundUrl id soundUrl model.words) |> deleteEmptyWords }, Cmd.none )

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
        [ ul [] (List.map showWord model.words)
        , button [ onClick EditMode ] [ text "Edit" ]
        ]


showWord : Word -> Html Msg
showWord word =
    div []
        [ text word.text
        ]


edit : Model -> Html Msg
edit model =
    div []
        [ ul []
            (List.append (List.map editWord model.words) [ editNewWord model.newWord ])
        , button [ onClick Save ] [ text "Save" ]
        , button [ onClick Cancel ] [ text "Cancel" ]
        ]


editWord : Word -> Html Msg
editWord word =
    div []
        [ input [ type_ "text", placeholder "text", onInput (WordText word.id), value word.text ] []
        , input [ type_ "text", placeholder "sound URL", onInput (WordSoundUrl word.id), value word.soundUrl ] []
        , div [] [ text ((toString word.id) ++ " | " ++ word.text ++ " | " ++ word.soundUrl) ]
        ]


editNewWord : Word -> Html Msg
editNewWord word =
    div []
        [ input [ type_ "text", placeholder "new word", onInput NewWordText, value word.text ] []
        ]
