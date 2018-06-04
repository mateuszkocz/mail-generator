module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Email
import Ports
import Date exposing (..)
import Task exposing (..)
import Dict exposing (..)


type alias Model =
    { value : String
    , emails : List Email.Email
    , autoClipboard : Bool
    , notes : Notes
    }


type alias Note =
    String


type alias Notes =
    Dict String Note


type Msg
    = Input String
    | GenerateNewMail
    | GenerateAdditionalMail Email.Email
    | SaveGeneratedEmail Email.Email Date
    | ClearEmailsList
    | RemoveEmail String
    | ReceivedEmails (List Email.Email)
    | Copy String
    | AutoClipboard Bool
    | UpdateNote Email.Id Note


initialModel : Model
initialModel =
    { value = ""
    , emails = []
    , autoClipboard = True
    , notes = Dict.empty
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
                    if model.autoClipboard then
                        Cmd.batch [ Ports.store emailWithDate, Ports.copy emailWithDate.id ]
                    else
                        Ports.store emailWithDate
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
            ( { model | autoClipboard = value }, Cmd.none )

        UpdateNote emailId content ->
            let
                updatedNotes =
                    Dict.update emailId (\_ -> Just content) model.notes
            in
                ( { model | notes = updatedNotes }, Cmd.none )


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



-- TODO: allow generating "another email like that", ie. generate with the same appendix like the clicked one with increased counter.


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
