port module Ports exposing (..)


port store : String -> Cmd msg


port getEmails : () -> Cmd msg


port receiveEmails : (List String -> msg) -> Sub msg


port removeEmail : String -> Cmd msg


port removeAllEmails : () -> Cmd msg
