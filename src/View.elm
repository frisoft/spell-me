module View exposing (view)

import Audio exposing (playAudio)
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Table as Table
import Html exposing (Html, a, div, h1, input, table, td, text, tr, ul)
import Html.Attributes exposing (class, href, id, placeholder, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Msg exposing (..)
import Types exposing (..)
import Utils exposing (..)
import String exposing (left)

view : Model -> Html Msg
view model =
    case model.mode of
        Show ->
            page (show model)

        Hide ->
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
        ([ Table.table
            { options = [ Table.striped, Table.attr (class "t able-words") ]
            , thead = Table.thead [] []
            , tbody = Table.tbody [] (List.map (\w -> decoratedWord model w |> showWord) model.words)
            }
         ]
            ++ showButtons model
         -- ++ [ text (toString model.playAll.time) ]
        )


showButtons : Model -> List (Html Msg)
showButtons model =
    [ if model.mode == Hide then
        primaryButton ShowMode "Show words"
      else
        primaryButton HideMode "Hide words"
    , text " "
    , primaryButton EditMode "Edit"
    , text " "
    , primaryButton Shuffle "Shuffle"

    -- , text " "
    -- , primaryButton PlayAllWords "Play all"
    ]


primaryButton : Msg -> String -> Html Msg
primaryButton msg txt =
    Button.button [ Button.primary, Button.attrs [ onClick msg ] ] [ text txt ]


decoratedWord : Model -> Word -> DecoratedWord
decoratedWord model word =
    { text = word.text
    , soundUrl = word.soundUrl
    , id = word.id
    , playing = Just word.id == model.playingWordId
    , canPlay = model.playingWordId == Nothing
    , mode = model.mode
    }


showWord : DecoratedWord -> Table.Row Msg
showWord word =
    Table.tr [] (showWordCells word)


showWordCells : DecoratedWord -> List (Table.Cell Msg)
showWordCells word =
    (if word.mode == Show then
        [ Table.td [] [ text word.text ]
        , Table.td [] [ dictionaryButton word ]
        ]
     else
        []
    )
        ++ [ Table.td [] [ playButton word ] ]


edit : Model -> Html Msg
edit model =
    div []
        [ Table.table
            { options = [ Table.striped, Table.attr (class "table-words") ]
            , thead = Table.thead [] []
            , tbody = Table.tbody [] (List.map (\w -> decoratedWord model w |> editWord) model.words)
            }
        , div [ class "add-new-word-container" ] [ addNewWord ]
        , primaryButton Save "Save"
        , text " "
        , Button.button [ Button.secondary, Button.attrs [ onClick Cancel ] ] [ text "Cancel" ]
        ]


editWord : DecoratedWord -> Table.Row Msg
editWord word =
    Table.tr []
        [ Table.td [] [ Input.text [ Input.id (wordDomId word.id), Input.placeholder "text", Input.onInput (WordText word.id), Input.defaultValue word.text ] ]
        , Table.td [] [ Input.text [ Input.placeholder "alternative sound URL", Input.onInput (WordSoundUrl word.id), Input.defaultValue word.soundUrl ] ]
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
        [ playAudio (soundUrlOrDefault word) word.playing AudioPlayEnded AudioError
        , if word.playing then
            Button.button [ Button.secondary, Button.disabled True, Button.attrs [ class "button-play" ] ] [ text "playing" ]
          else if word.canPlay then
            Button.button [ Button.secondary, Button.attrs [ onClick (Play word.id), class "button-play" ] ] [ text "play" ]
          else
            Button.button [ Button.secondary, Button.disabled True, Button.attrs [ class "button-play" ] ] [ text "play" ]
        ]


soundUrlOrDefault : DecoratedWord -> String
soundUrlOrDefault word =
    if word.soundUrl /= "" then
        "https://www.collinsdictionary.com" ++ word.soundUrl
    else
        defaultSoundUrl word.text

defaultSoundUrl : String -> String
defaultSoundUrl text =
  let
    w = text ++ "__gb"
  in
    "https://www.oxfordlearnersdictionaries.com/media/english/uk_pron_ogg/" ++
      (left 1 text) ++ "/" ++
      (left 3 w) ++ "/" ++
      (left 5 w) ++ "/" ++
      w ++ "_1.ogg"
