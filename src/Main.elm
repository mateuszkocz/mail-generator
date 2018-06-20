module Main exposing (main)

import Html
import Ports
import Dict
import Types exposing (Model, Msg(..))
import Update
import View
import Email


initialModel : Model
initialModel =
    { value = ""
    , emails = []
    , settings =
        { autoClipboard = True
        , baseDomain = Email.initialHost
        }
    , notes = Dict.empty
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [ Ports.getEmails (), Ports.getNotes (), Ports.getSettings () ] )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Ports.receiveEmails ReceiveEmails, Ports.receiveNotes ReceiveNotes, Ports.receiveSettings ReceiveSettings ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = View.mainView
        , update = Update.update
        , subscriptions = subscriptions
        }
