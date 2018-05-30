module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Email
import Ports


type alias Model =
    { userName : String
    , host : String
    , emails : List String
    , at : Bool
    }


type Msg
    = Input String
    | GenerateMail
    | ClearEmailsList
    | RemoveEmail String
    | ReceivedEmails (List String)


initialModel : Model
initialModel =
    { userName = ""
    , host = Email.initialHost
    , emails = []
    , at = False
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Ports.getEmails () )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            let
                ( userName, host ) =
                    Email.splitAddress value
            in
                ( { model | userName = userName, host = host, at = String.contains "@" value }, Cmd.none )

        GenerateMail ->
            let
                email =
                    Email.generateEmail model.userName model.host model.emails
            in
                ( { model | emails = List.append model.emails [ email ] }, Ports.store email )

        ClearEmailsList ->
            ( { model | emails = [] }, Ports.removeAllEmails () )

        RemoveEmail droppedEmail ->
            ( { model | emails = List.filter (\email -> email /= droppedEmail) model.emails }, Ports.removeEmail droppedEmail )

        ReceivedEmails emails ->
            ( { model | emails = List.concat [ model.emails, emails ] }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Ports.receiveEmails ReceivedEmails ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
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



-- TODO: allow generating "another email like that", ie. generate with the same appendix like the clicked one with increased counter.


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
        , hostAddition model
        , button [] [ text "Generate" ]
        ]


hostAddition : Model -> Html Msg
hostAddition { userName, host, at } =
    let
        userNamePlaceholder =
            span [ style [ ( "color", "transparent" ) ] ] [ text userName ]

        atPlaceholder =
            if at then
                span [ style [ ( "color", "transparent" ) ] ] [ text "@" ]
            else
                span [ style [ ( "color", "gray" ) ] ] [ text "@" ]

        hostColor =
            if host == Email.initialHost then
                "gray"
            else
                "transparent"

        hostPlaceholder =
            span [ style [ ( "color", hostColor ) ] ] [ text host ]
    in
        div []
            [ userNamePlaceholder
            , atPlaceholder
            , hostPlaceholder
            ]
