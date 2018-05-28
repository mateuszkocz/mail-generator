module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Email


type alias Model =
    { userName : String
    , host : String
    , emails : List String
    }


type Msg
    = Input String
    | GenerateMail
    | ClearEmailsList
    | RemoveEmail String


initialModel : Model
initialModel =
    { userName = ""
    , host = Email.initialHost
    , emails = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            let
                ( userName, host ) =
                    Email.splitAddress value
            in
                ( { model | userName = userName, host = host }, Cmd.none )

        GenerateMail ->
            ( { model | emails = List.append model.emails [ Email.generateEmail model.userName model.host model.emails ] }, Cmd.none )

        ClearEmailsList ->
            ( { model | emails = [] }, Cmd.none )

        RemoveEmail droppedEmail ->
            ( { model | emails = List.filter (\email -> email /= droppedEmail) model.emails }, Cmd.none )


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
        , mailForm model
        , mailsList model.emails
        ]


mailInput : Model -> Html Msg
mailInput model =
    input
        [ placeholder "Mail address"
        , onInput Input
        ]
        []


mailsList : List String -> Html Msg
mailsList emails =
    div []
        [ ul [] (List.reverse (mailItems emails))
        , if List.isEmpty emails then
            text ""
          else
            button [ type_ "button", onClick ClearEmailsList ] [ text "Clear" ]
        ]


mailItems : List String -> List (Html Msg)
mailItems emails =
    List.map mailItem emails


mailItem : String -> Html Msg
mailItem email =
    li []
        [ text email
        , button [ onClick (RemoveEmail email) ] [ text "Remove" ]
        ]


mailForm : Model -> Html Msg
mailForm model =
    Html.form
        [ onSubmit GenerateMail ]
        [ mailInput model
        , span [] [ text model.host ]
        , button [] [ text "Generate" ]
        ]
