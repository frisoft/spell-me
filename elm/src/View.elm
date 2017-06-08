module View exposing (view)

import Html exposing (Html, ul, div, input, text, a, table, tr, td, h1)
import Html.Attributes exposing (id, type_, placeholder, value, href, target)
import Html.Events exposing (onInput, onClick)
import Types exposing (..)
import Msg exposing (..)
import Audio exposing (playAudio)
import Utils exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input


view : Model -> Html Msg
view model =
    case model.mode of
        Show ->
            page (show model)

        Edit ->
            page (edit model)


page : Html Msg -> Html Msg
page body =
    Grid.container []
        [ Grid.simpleRow
            [ Grid.col [ Col.xs12 ]
                [ header ]
            ]
        , Grid.simpleRow
            [ Grid.col [ Col.xs12 ]
                [ body ]
            ]
        ]


header : Html Msg
header =
    h1 [] [ text "Spelling" ]


show : Model -> Html Msg
show model =
    div []
        [ table [] (List.map (\w -> (decoratedWord model w) |> showWord) model.words)
        , Button.button [ Button.primary, Button.attrs [ onClick EditMode ] ] [ text "Edit" ]
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
        , div [] [ addNewWord ]
        , Button.button [ Button.primary, Button.attrs [ onClick Save ] ] [ text "Save" ]
        , Button.button [ Button.secondary, Button.attrs [ onClick Cancel ] ] [ text "Cancel" ]
        ]


editWord : DecoratedWord -> Html Msg
editWord word =
    tr []
        [ td [] [ Input.text [ Input.id (wordDomId word.id), Input.placeholder "text", Input.onInput (WordText word.id), Input.defaultValue word.text ] ]
        , td [] [ Input.text [ Input.placeholder "sound URL", Input.onInput (WordSoundUrl word.id), Input.defaultValue word.soundUrl ] ]
        , td [] [ a [ href "#", onClick (DeleteWord word.id) ] [ text "delete" ] ]
        , td [] [ dictionaryLink word ]
        , td [] [ text "|" ]
        , td [] [ playButton word ]
        ]


editNewWord : Word -> Html Msg
editNewWord word =
    div []
        [ Input.text [ Input.id "new-word", Input.placeholder "new word", Input.onInput NewWordText, Input.defaultValue word.text ]
        ]


addNewWord : Html Msg
addNewWord =
    a [ href "#", onClick AddWord ] [ text "add word" ]


dictionaryLink : DecoratedWord -> Html Msg
dictionaryLink word =
    a [ href ("http://www.collinsdictionary.com/dictionary/english/" ++ word.text), target "_blank" ] [ text "dictionary" ]


playButton : DecoratedWord -> Html Msg
playButton word =
    if word.playing then
        div []
            [ text "playing"
            , playAudio word.soundUrl AudioPlayEnded AudioError
            ]
    else if word.canPlay then
        a [ href "#", onClick (Play word.id) ] [ text "play" ]
    else
        text "play"
