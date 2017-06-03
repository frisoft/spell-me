module Audio exposing (playAudio)

import Html exposing (Attribute, Html, audio)
import Html.Attributes exposing (src, autoplay)
import Html.Events exposing (on)
import Json.Decode as Json


onEnded : msg -> Attribute msg
onEnded message =
    on "ended" (Json.succeed message)


playAudio : String -> msg -> Html msg
playAudio soundUrl msgEnded =
    audio [ src soundUrl, autoplay True, onEnded msgEnded ] []
