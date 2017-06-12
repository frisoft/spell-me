module View exposing (view)

import Html exposing (Html, ul, div, input, text, a, table, tr, td, h1)
import Html.Attributes exposing (id, type_, placeholder, value, href, target, class)
import Html.Events exposing (onInput, onClick)
import Types exposing (..)
import Msg exposing (..)
import Audio exposing (playAudio)
import Utils exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Table as Table


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
        , td [] [ dictionaryButton word ]
        , td [] [ playButton word ]
        ]


edit : Model -> Html Msg
edit model =
    div []
        [ Table.table
            { options = [ Table.striped, Table.attr (class "table-words") ]
            , thead = Table.thead [] []
            , tbody = Table.tbody [] (List.map (\w -> (decoratedWord model w) |> editWord) model.words)
            }
        , div [ class "add-new-word-container" ] [ addNewWord ]
        , Button.button [ Button.primary, Button.attrs [ onClick Save ] ] [ text "Save" ]
        , text " "
        , Button.button [ Button.secondary, Button.attrs [ onClick Cancel ] ] [ text "Cancel" ]
        ]


editWord : DecoratedWord -> Table.Row Msg
editWord word =
    Table.tr []
        [ Table.td [] [ Input.text [ Input.id (wordDomId word.id), Input.placeholder "text", Input.onInput (WordText word.id), Input.defaultValue word.text ] ]
        , Table.td [] [ Input.text [ Input.placeholder "sound URL", Input.onInput (WordSoundUrl word.id), Input.defaultValue word.soundUrl ] ]
        , Table.td [] [ Button.button [ Button.danger, Button.attrs [ onClick (DeleteWord word.id) ] ] [ text "-" ] ]
        , Table.td [] [ dictionaryButton word ]
        , Table.td [] [ playButton word ]
        ]


addNewWord : Html Msg
addNewWord =
    Button.button [ Button.success, Button.attrs [ onClick AddWord ] ] [ text "+" ]


dictionaryButton : DecoratedWord -> Html Msg
dictionaryButton word =
    a [ class "btn", href ("http://www.collinsdictionary.com/dictionary/english/" ++ word.text), target "_blank" ] [ text "dictionary" ]


playButton : DecoratedWord -> Html Msg
playButton word =
    div []
        [ playAudio word.soundUrl word.playing AudioPlayEnded AudioError
        , if word.playing then
            Button.button [ Button.secondary, Button.disabled True ] [ text "playing" ]
          else if word.canPlay then
            Button.button [ Button.secondary, Button.attrs [ onClick (Play word.id) ] ] [ text "play" ]
          else
            Button.button [ Button.secondary, Button.disabled True ] [ text "play" ]
        ]
