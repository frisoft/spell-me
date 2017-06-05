module View exposing (view)

import Html exposing (Html, ul, div, input, text, button, a, table, tr, td, h1)
import Html.Attributes exposing (id, type_, placeholder, value, href, target)
import Html.Events exposing (onInput, onClick)
import Types exposing (..)
import Msg exposing (..)
import Audio exposing (playAudio)
import Utils exposing (..)


view : Model -> Html Msg
view model =
    case model.mode of
        Show ->
            page (show model)

        Edit ->
            page (edit model)


page : Html Msg -> Html Msg
page body =
    div []
        [ header
        , body
        ]


header : Html Msg
header =
    h1 [] [ text "Spelling" ]


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
        [ td [] [ input [ id (wordDomId word.id), type_ "text", placeholder "text", onInput (WordText word.id), value word.text ] [] ]
        , td [] [ input [ type_ "text", placeholder "sound URL", onInput (WordSoundUrl word.id), value word.soundUrl ] [] ]
        , td [] [ dictionaryLink word ]
        , td [] [ playButton word ]
        , td [] [ text ((toString word.id) ++ " | " ++ word.text ++ " | " ++ word.soundUrl) ]
        ]


editNewWord : Word -> Html Msg
editNewWord word =
    div []
        [ input [ id "new-word", type_ "text", placeholder "new word", onInput NewWordText, value word.text ] []
        ]


dictionaryLink : DecoratedWord -> Html Msg
dictionaryLink word =
    a [ href ("http://www.collinsdictionary.com/dictionary/english/" ++ word.text), target "_blank" ] [ text "dictionary" ]


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
