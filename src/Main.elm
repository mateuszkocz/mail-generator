module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Email
import Ports


type alias Model =
    { value : String
    , emails : List Email.Email
    , autoClipboard : Bool
    }


type Msg
    = Input String
    | GenerateNewMail
    | GenerateAdditionalMail Email.Email
    | ClearEmailsList
    | RemoveEmail String
    | ReceivedEmails (List Email.Email)
    | Copy String
    | AutoClipboard Bool


initialModel : Model
initialModel =
    { value = ""
    , emails = []
    , autoClipboard = True
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Ports.getEmails () )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            ( { model | value = value }, Cmd.none )

        GenerateNewMail ->
            let
                email =
                    Email.generateEmail model.value model.emails

                effect =
                    if model.autoClipboard then
                        Cmd.batch [ Ports.store email, Ports.copy email.id ]
                    else
                        Ports.store email
            in
                ( { model | emails = List.append model.emails [ email ] }, effect )

        GenerateAdditionalMail baseEmail ->
            let
                newEmail =
                    Email.generateAdditionalEmail baseEmail model.emails

                effect =
                    if model.autoClipboard then
                        Cmd.batch [ Ports.store newEmail, Ports.copy newEmail.id ]
                    else
                        Ports.store newEmail
            in
                ( { model | emails = List.append model.emails [ newEmail ] }, effect )

        ClearEmailsList ->
            ( { model | emails = [] }, Ports.removeAllEmails () )

        RemoveEmail droppedEmail ->
            ( { model | emails = List.filter (\email -> email.id /= droppedEmail) model.emails }, Ports.removeEmail droppedEmail )

        ReceivedEmails emails ->
            ( { model | emails = List.concat [ model.emails, emails ] }, Cmd.none )

        Copy address ->
            ( model, Ports.copy address )

        AutoClipboard value ->
            ( { model | autoClipboard = value }, Cmd.none )


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


mailInput : Html Msg
mailInput =
    input
        [ placeholder "Mail address"
        , onInput Input
        ]
        []


mailsList : List Email.Email -> Html Msg
mailsList emails =
    div []
        [ ul [] (List.reverse (mailItems emails))
        , if List.isEmpty emails then
            text ""
          else
            button [ type_ "button", onClick ClearEmailsList ] [ text "Clear" ]
        ]


mailItems : List Email.Email -> List (Html Msg)
mailItems emails =
    List.map mailItem emails



-- TODO: allow generating "another email like that", ie. generate with the same appendix like the clicked one with increased counter.


mailItem : Email.Email -> Html Msg
mailItem email =
    li []
        [ text email.id
        , button [ onClick (Copy email.id) ] [ text "Copy" ]
        , button [ onClick (GenerateAdditionalMail email) ] [ text "New" ]
        , button [ onClick (RemoveEmail email.id) ] [ text "Remove" ]
        ]


mailForm : Model -> Html Msg
mailForm { value, autoClipboard } =
    Html.form
        [ onSubmit GenerateNewMail ]
        [ mailInput
        , hostAddition value
        , button [] [ text "Generate" ]
        , input
            [ onCheck AutoClipboard
            , type_ "checkbox"
            , checked autoClipboard
            ]
            [ text "Save to clipboard on generation" ]
        ]


hostAddition : String -> Html Msg
hostAddition value =
    let
        ( userName, host ) =
            Email.splitAddress value

        hostColor =
            if host == Email.initialHost && not (String.contains Email.initialHost value) then
                "gray"
            else
                "transparent"

        atColor =
            if String.contains "@" value then
                "transparent"
            else
                "gray"

        userNamePlaceholder =
            span [ style [ ( "color", "transparent" ) ] ] [ text userName ]

        atPlaceholder =
            span [ style [ ( "color", atColor ) ] ] [ text "@" ]

        hostPlaceholder =
            span [ style [ ( "color", hostColor ) ] ] [ text host ]
    in
        div []
            [ userNamePlaceholder
            , atPlaceholder
            , hostPlaceholder
            ]
