import Html exposing (Html, ul, div, input, text)
import Html.Attributes exposing (type_, placeholder, value)
import Html.Events exposing (onInput)
-- import List exposing (..)

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { words: Words
  }

type alias Words =
  List Word

type alias Word =
  { text: String
  , soundUrl: String
  , id: Int
  }

model : Model
model =
  {
    words =
    [ { text = "Please", soundUrl = "", id = 0 }
    , { text = "Thanks", soundUrl = "", id = 1 }
    ]
  }

-- UPDATE

type Msg
  = WordText Int String
  | WordSoundUrl Int String

update : Msg -> Model -> Model
update msg model =
  case msg of
    WordText id text ->
      { model | words = updateText id text model.words }
    WordSoundUrl id soundUrl ->
      { model | words = updateSoundUrl id soundUrl model.words }

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



-- VIEW
view : Model -> Html Msg
view model =
  ul [] (List.map viewWord model.words)

viewWord : Word -> Html Msg
viewWord word =
  div []
    [ input [ type_ "text", placeholder "text", onInput (WordText word.id), value word.text ] []
    , input [ type_ "text", placeholder "sound URL", onInput (WordSoundUrl word.id), value word.soundUrl ] []
    , div [] [text (word.text ++ " | " ++ word.soundUrl)]
    ]
