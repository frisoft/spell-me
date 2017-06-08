module Audio exposing (playAudio)

import Html exposing (Attribute, Html, audio)
import Html.Attributes exposing (src, autoplay)
import Html.Events exposing (on)
import Json.Decode as Json


onError : msg -> Attribute msg
onError message =
    on "error" (Json.succeed message)


onEnded : msg -> Attribute msg
onEnded message =
    on "ended" (Json.succeed message)


playAudio : String -> msg -> msg -> Html msg
playAudio soundUrl msgEnded msgError =
    audio [ src soundUrl, autoplay True, onEnded msgEnded, onError msgError ] []
