module Types exposing (..)


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
    }


type Mode
    = Show
    | Edit


type alias Model =
    { words : Words
    , prevWords : Words
    , newWord : Word
    , mode : Mode
    , playingWordId : Maybe Int
    }
