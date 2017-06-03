module Msg exposing (..)


type Msg
    = WordText Int String
    | WordSoundUrl Int String
    | Play Int
    | PlayEnded
    | NewWordText String
    | EditMode
    | Cancel
    | Save
