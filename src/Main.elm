module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { base : String
    , host : String
    }


type alias AddressValues =
    ( String, String )


type Msg
    = Input String


initialModel : Model
initialModel =
    { base = ""
    , host = "gmail.com"
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
                    splitAddress value
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
        , p [] [ text (combineAddress ( model.base, model.host )) ]
        ]


mailInput : Model -> Html Msg
mailInput model =
    input
        [ placeholder "Mail address"
        , onInput Input
        ]
        []


address : String -> String
address value =
    value
        |> splitAddress
        |> combineAddress


splitAddress : String -> AddressValues
splitAddress address =
    case String.split "@" address of
        [ base, "" ] ->
            ( base, initialModel.host )

        [ base ] ->
            ( base, initialModel.host )

        [ base, domain ] ->
            ( base, domain )

        _ ->
            ( "", initialModel.host )


combineAddress : AddressValues -> String
combineAddress ( base, host ) =
    base ++ "@" ++ host
