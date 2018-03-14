module Audio exposing (playAudio)

import Html exposing (Attribute, Html, audio, span, text)
import Html.Attributes exposing (src, autoplay)
import Html.Events exposing (on)
import Json.Decode as Json


onError : msg -> Attribute msg
onError message =
    on "error" (Json.succeed message)


onEnded : msg -> Attribute msg
onEnded message =
    on "ended" (Json.succeed message)


playAudio : String -> Bool -> msg -> msg -> Html msg
playAudio soundUrl autoPlay msgEnded msgError =
    span []
        [ audio [ src soundUrl ] []
        , if autoPlay then
            audio [ src soundUrl, autoplay True, onEnded msgEnded, onError msgError ] []
          else
            text ""
        ]
