module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Email


type alias Model =
    { base : String
    , host : String
    }


type Msg
    = Input String


initialModel : Model
initialModel =
    { base = ""
    , host = Email.initialHost
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            let
                ( base, host ) =
                    Email.splitAddress value
            in
                ( { model | base = base, host = host }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Mail generator" ]
        , mailInput model
        , span [] [ text model.host ]
        , p [] [ text (Email.combineAddress ( model.base, model.host )) ]
        ]


mailInput : Model -> Html Msg
mailInput model =
    input
        [ placeholder "Mail address"
        , onInput Input
        ]
        []
