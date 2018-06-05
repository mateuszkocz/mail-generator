module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Email
import Notes exposing (Note, Notes)
import Ports
import Date exposing (..)
import Task exposing (..)
import Dict exposing (..)


type alias Model =
    { value : String
    , emails : List Email.Email
    , notes : Notes
    , settings : Settings
    }


type alias Settings =
    { autoClipboard : Bool
    }


type Msg
    = Input String
    | GenerateNewMail
    | GenerateAdditionalMail Email.Email
    | SaveGeneratedEmail Email.Email Date
    | ClearEmailsList
    | RemoveEmail String
    | ReceivedEmails (List Email.Email)
    | ReceivedNotes (List ( String, Note ))
    | Copy String
    | AutoClipboard Bool
    | UpdateNote Email.Id Note


initialModel : Model
initialModel =
    { value = ""
    , emails = []
    , settings = { autoClipboard = True }
    , notes = Dict.empty
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [ Ports.getEmails (), Ports.getNotes () ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            ( { model | value = value }, Cmd.none )

        GenerateNewMail ->
            let
                email =
                    Email.generateEmail model.value model.emails
            in
                ( model, Task.perform (SaveGeneratedEmail email) Date.now )

        GenerateAdditionalMail baseEmail ->
            let
                email =
                    Email.generateAdditionalEmail baseEmail model.emails
            in
                ( model, Task.perform (SaveGeneratedEmail email) Date.now )

        SaveGeneratedEmail email date ->
            let
                emailWithDate =
                    { email | createdAt = toString date }

                effect =
                    if model.settings.autoClipboard then
                        Cmd.batch [ Ports.storeEmail emailWithDate, Ports.copy emailWithDate.id ]
                    else
                        Ports.storeEmail emailWithDate
            in
                ( { model | emails = List.append model.emails [ emailWithDate ] }, effect )

        ClearEmailsList ->
            ( { model | emails = [] }, Ports.removeAllEmails () )

        RemoveEmail droppedEmail ->
            ( { model | emails = List.filter (\email -> email.id /= droppedEmail) model.emails }, Ports.removeEmail droppedEmail )

        ReceivedEmails emails ->
            ( { model | emails = List.concat [ model.emails, emails ] }, Cmd.none )

        Copy address ->
            ( model, Ports.copy address )

        AutoClipboard value ->
            let
                settings =
                    model.settings

                newSettings =
                    { settings | autoClipboard = value }
            in
                ( { model | settings = newSettings }, Cmd.none )

        UpdateNote emailId content ->
            let
                updatedNotes =
                    Dict.update emailId (\_ -> Just content) model.notes
            in
                ( { model | notes = updatedNotes }, Ports.storeNote ( emailId, content ) )

        ReceivedNotes notes ->
            ( { model | notes = Dict.fromList notes }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Ports.receiveEmails ReceivedEmails, Ports.receiveNotes ReceivedNotes ]


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
        , mailsList model.emails model.notes
        ]


mailInput : Html Msg
mailInput =
    input
        [ placeholder "Mail address"
        , onInput Input
        ]
        []


mailsList : List Email.Email -> Notes -> Html Msg
mailsList emails notes =
    div []
        [ ul [] (List.reverse (mailItems emails notes))
        , if List.isEmpty emails then
            text ""
          else
            button [ type_ "button", onClick ClearEmailsList ] [ text "Clear" ]
        ]


mailItems : List Email.Email -> Notes -> List (Html Msg)
mailItems emails notes =
    List.map (\email -> mailItem email (Dict.get email.id notes)) emails


mailItem : Email.Email -> Maybe Note -> Html Msg
mailItem email note =
    li []
        [ text email.id
        , displayDate email.createdAt
        , button [ onClick (Copy email.id) ] [ text "Copy" ]
        , button [ onClick (GenerateAdditionalMail email) ] [ text "New" ]
        , button [ onClick (RemoveEmail email.id) ] [ text "Remove" ]
        , noteView email.id note
        ]


noteView : String -> Maybe Note -> Html Msg
noteView id note =
    let
        noteContent =
            case note of
                Just v ->
                    v

                Nothing ->
                    ""
    in
        div []
            [ textarea
                [ value noteContent
                , onInput (UpdateNote id)
                ]
                []
            , button
                [ onClick (UpdateNote id "") ]
                [ text "Clear" ]
            ]


mailForm : Model -> Html Msg
mailForm { value, settings } =
    Html.form
        [ onSubmit GenerateNewMail ]
        [ mailInput
        , hostAddition value
        , button [] [ text "Generate" ]
        , label []
            [ input
                [ onCheck AutoClipboard
                , type_ "checkbox"
                , checked autoClipboard.autoClipboard
                ]
                []
            , text "Save to clipboard"
            ]
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


displayDate : String -> Html Msg
displayDate date =
    case Date.fromString date of
        Ok d ->
            let
                dateContent =
                    [ Date.day d, Date.hour d, Date.minute d ]
                        |> List.map (\v -> toString v)
                        |> String.join "_"
            in
                span
                    [ style [ ( "margin", ("10px") ) ] ]
                    [ text (toString (Date.month d) ++ "_" ++ dateContent) ]

        Err err ->
            text ""
