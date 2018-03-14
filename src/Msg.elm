module Msg exposing (..)

import Dom
import Time exposing (Time)
import Types exposing (Words)


type Msg
    = WordText Int String
    | WordSoundUrl Int String
    | Play Int
    | AudioPlayEnded
    | AudioError
    | AddWord
    | DeleteWord Int
    | EditMode
    | HideMode
    | ShowMode
    | Cancel
    | Save
    | FocusResult (Result Dom.Error ())
    | Shuffle
    | ShuffledList Words
    | PlayAllWords



-- | Tick Time
