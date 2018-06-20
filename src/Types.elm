module Types exposing (..)

import Dict
import Date


type Msg
    = Input String
    | GenerateNewMail
    | GenerateAdditionalMail Email
    | SaveGeneratedEmail Email Date.Date
    | ClearEmailsList
    | RemoveEmail String
    | ReceivedEmails (List Email)
    | ReceivedNotes (List ( String, Note ))
    | Copy String
    | AutoClipboard Bool
    | UpdateNote Id Note
    | ReceivedSettings (Maybe Settings)
    | SetBaseDomain String


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
    Dict.Dict String Note


type alias Settings =
    { autoClipboard : Bool
    , baseDomain : String
    }
