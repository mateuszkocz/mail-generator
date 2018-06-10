module Main exposing (..)

import Html exposing (..)
import Ports
import Dict exposing (..)
import Types exposing (..)
import Update
import View
import Email as E


initialModel : Model
initialModel =
    { value = ""
    , emails = []
    , settings =
        { autoClipboard = True
        , baseDomain = E.initialHost
        }
    , notes = Dict.empty
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [ Ports.getEmails (), Ports.getNotes (), Ports.getSettings () ] )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Ports.receiveEmails ReceivedEmails, Ports.receiveNotes ReceivedNotes, Ports.receiveSettings ReceivedSettings ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = View.mainView
        , update = Update.update
        , subscriptions = subscriptions
        }
