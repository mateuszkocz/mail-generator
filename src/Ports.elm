port module Ports exposing (..)

import Email exposing (Email, Id)
import Notes exposing (Note)


port storeEmail : Email -> Cmd msg


port storeNote : ( Id, Note ) -> Cmd msg


port getEmails : () -> Cmd msg


port receiveEmails : (List Email -> msg) -> Sub msg


port removeEmail : String -> Cmd msg


port removeAllEmails : () -> Cmd msg


port copy : String -> Cmd msg
