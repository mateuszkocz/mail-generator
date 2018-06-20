port module Ports exposing (..)

import Types exposing (Email, Id, Note, Settings)


port storeEmail : Email -> Cmd msg


port storeNote : ( Id, Note ) -> Cmd msg


port getEmails : () -> Cmd msg


port getNotes : () -> Cmd msg


port receiveEmails : (List Email -> msg) -> Sub msg


port receiveNotes : (List ( String, Note ) -> msg) -> Sub msg


port removeEmail : String -> Cmd msg


port removeAllEmails : () -> Cmd msg


port copy : String -> Cmd msg


port storeSettings : Settings -> Cmd msg


port receiveSettings : (Maybe Settings -> msg) -> Sub msg


port getSettings : () -> Cmd msg
