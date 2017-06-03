module Msg exposing (..)

import Dom


type Msg
    = WordText Int String
    | WordSoundUrl Int String
    | Play Int
    | PlayEnded
    | NewWordText String
    | EditMode
    | Cancel
    | Save
    | FocusResult (Result Dom.Error ())
