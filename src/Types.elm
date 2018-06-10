module Types exposing (..)

import Dict exposing (Dict)


type alias Model =
    { value : String
    , emails : List Email
    , notes : Notes
    , settings : Settings
    }


type alias AddressValues =
    ( String, String )


type alias Id =
    String


type alias Email =
    { id : Id
    , userName : String
    , host : String
    , count : Int
    , createdAt : String
    }


type alias Note =
    String


type alias Notes =
    Dict String Note


type alias Settings =
    { autoClipboard : Bool
    , baseDomain : String
    }
