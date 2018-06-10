module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)
import Dict exposing (Dict)
import Email as E
import Date


mainView : Model -> Html Msg
mainView model =
    div [ style [ ( "font-size", "1.5rem" ), ( "font-family", "sans-serif" ) ] ]
        [ h1 [] [ text "Mail generator" ]
        , mailForm model
        , domainSaver model.value model.settings.baseDomain
        , mailsList model.emails model.notes
        ]


mailInput : Html Msg
mailInput =
    input
        [ onInput Input
        , style
            [ ( "border", "none" )
            , ( "border-bottom", "1px solid gray" )
            , ( "font-size", "inherit" )
            , ( "padding", ".5rem" )
            , ( "font-family", "inherit" )
            , ( "width", "50%" )
            ]
        ]
        []


mailsList : List Email -> Notes -> Html Msg
mailsList emails notes =
    div []
        [ ul [] (List.reverse (mailItems emails notes))
        , if List.isEmpty emails then
            text ""
          else
            button [ type_ "button", onClick ClearEmailsList ] [ text "Clear" ]
        ]


mailItems : List Email -> Notes -> List (Html Msg)
mailItems emails notes =
    List.map (\email -> mailItem email (Dict.get email.id notes)) emails


mailItem : Email -> Maybe Note -> Html Msg
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
        [ onSubmit GenerateNewMail
        , style [ ( "position", "relative" ) ]
        ]
        [ mailInput
        , hostAddition value settings.baseDomain
        , button [] [ text "Generate" ]
        , label []
            [ input
                [ onCheck AutoClipboard
                , type_ "checkbox"
                , checked settings.autoClipboard
                ]
                []
            , text "Save to clipboard"
            ]
        ]


hostAddition : String -> String -> Html Msg
hostAddition value baseDomain =
    let
        ( userName, host ) =
            E.splitAddress value baseDomain

        hostColor =
            if host == baseDomain && not (String.contains baseDomain value) then
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
        div
            [ style
                [ ( "position", "absolute" )
                , ( "top", ".5rem" )
                , ( "left", ".5rem" )
                , ( "pointer-events", "none" )
                ]
            ]
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


domainSaver : String -> String -> Html Msg
domainSaver value baseDomain =
    let
        ( _, host ) =
            E.splitAddress value baseDomain

        textContent =
            "Save " ++ host ++ " domain as a default."
    in
        button
            [ disabled (host == baseDomain)
            , onClick (SetBaseDomain host)
            ]
            [ text textContent ]
