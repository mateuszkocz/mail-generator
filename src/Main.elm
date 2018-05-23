module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Model =
    { mail : String
    }


type Msg
    = Input String


initialModel : Model
initialModel =
    { mail = ""
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            ( { model | mail = value }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Mail generator" ]
        , mailInput model
        , p [] [ text model.mail ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


mailInput : Model -> Html Msg
mailInput model =
    input [ placeholder "Mail address", onInput Input ] []
