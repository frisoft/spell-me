module Types exposing (..)

import Time exposing (Time)


type alias Words =
    List Word


type alias Word =
    { text : String
    , soundUrl : String
    , id : Int
    }


type alias DecoratedWord =
    { text : String
    , soundUrl : String
    , id : Int
    , playing : Bool
    , canPlay : Bool
    , mode : Mode
    }


type alias PlayAll =
    { playingId : Int
    , startTime : Time
    , time : Time
    }


type Mode
    = Show
    | Hide
    | Edit


type alias Model =
    { words : Words
    , prevWords : Words
    , mode : Mode
    , playingWordId : Maybe Int
    , playAll : PlayAll
    }
