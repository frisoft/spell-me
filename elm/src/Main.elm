import Html exposing (Html, ul, div, input, text, button)
import Html.Attributes exposing (type_, placeholder, value)
import Html.Events exposing (onInput, onClick)
-- import List exposing (..)

main : Program Never Model Msg
main =
  programWithFlags { init = init, model = model, view = view, update = update }


-- MODEL

type alias Model =
  { words: Words
  , newWord: Word
  , mode: Mode
  }

type alias Words =
  List Word

type alias Word =
  { text: String
  , soundUrl: String
  , id: Int
  }

type Mode = Show | Edit

initialNewWord : Word
initialNewWord =
  { text = "", soundUrl = "", id = -1 }

model : Model
model =
  {
    words =
    [ { text = "Please", soundUrl = "", id = 0 }
    , { text = "Thanks", soundUrl = "", id = 1 }
    ]
    , newWord = initialNewWord
    , mode = Show
  }

-- INIT
type alias Flags =
  {
    words : List Word
  }

init : Flags -> (Model, Cmd Msg)
init : flags =
  (
    {
      words: flags.words
    }
    , Cmd.none
  )

-- UPDATE

type Msg
  = WordText Int String
  | WordSoundUrl Int String
  | NewWordText String
  | EditMode

update : Msg -> Model -> Model
update msg model =
  case msg of
    WordText id text ->
      { model | words = (updateText id text model.words) |> deleteEmptyWords }
    WordSoundUrl id soundUrl ->
      { model | words = (updateSoundUrl id soundUrl model.words) |> deleteEmptyWords }
    NewWordText text ->
      addNewWord model text

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
  { model |
      words = List.append model.words [{ initialNewWord | id = (maxWordId model.words) + 1, text = text }]
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
  ul []
    (List.append (List.map editWord model.words) [editNewWord model.newWord])

editWord : Word -> Html Msg
editWord word =
  div []
    [ input [ type_ "text", placeholder "text", onInput (WordText word.id), value word.text ] []
    , input [ type_ "text", placeholder "sound URL", onInput (WordSoundUrl word.id), value word.soundUrl ] []
    , div [] [text ((toString word.id) ++ " | " ++ word.text ++ " | " ++ word.soundUrl)]
    ]

editNewWord : Word -> Html Msg
editNewWord word =
  div []
    [ input [ type_ "text", placeholder "new word", onInput NewWordText, value word.text ] []
    ]
