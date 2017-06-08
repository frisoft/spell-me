module Msg exposing (..)

import Dom


type Msg
    = WordText Int String
    | WordSoundUrl Int String
    | Play Int
    | AudioPlayEnded
    | AudioError
    | NewWordText String
    | AddWord
    | DeleteWord Int
    | EditMode
    | Cancel
    | Save
    | FocusResult (Result Dom.Error ())
