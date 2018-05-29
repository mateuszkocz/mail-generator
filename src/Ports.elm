port module Ports exposing (..)


port read : () -> Cmd msg


port receivedReadResult : (String -> msg) -> Sub msg
