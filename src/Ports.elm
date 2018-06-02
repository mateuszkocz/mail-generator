port module Ports exposing (..)

import Email exposing (Email)


port store : Email -> Cmd msg


port getEmails : () -> Cmd msg


port receiveEmails : (List Email -> msg) -> Sub msg


port removeEmail : String -> Cmd msg


port removeAllEmails : () -> Cmd msg
