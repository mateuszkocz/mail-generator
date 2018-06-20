module Update exposing (update)

import Types exposing (Model, Msg(..))
import Email
import Task
import Ports
import Date
import Dict


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input value ->
            ( { model | value = value }, Cmd.none )

        GenerateNewMail ->
            let
                email =
                    Email.generateEmail model.value model.emails model.settings.baseDomain
            in
                if Email.empty email then
                    ( model, Cmd.none )
                else
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
                ( { model | settings = newSettings }, Ports.storeSettings newSettings )

        UpdateNote emailId content ->
            let
                updatedNotes =
                    Dict.update emailId (\_ -> Just content) model.notes
            in
                ( { model | notes = updatedNotes }, Ports.storeNote ( emailId, content ) )

        ReceivedNotes notes ->
            ( { model | notes = Dict.fromList notes }, Cmd.none )

        ReceivedSettings settings ->
            case settings of
                Just s ->
                    ( { model | settings = s }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetBaseDomain domain ->
            let
                settings =
                    model.settings

                newSettings =
                    { settings | baseDomain = domain }
            in
                ( { model | settings = newSettings }, Ports.storeSettings newSettings )
