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
    div [ style [ ( "font-size", "1rem" ), ( "font-family", "sans-serif" ) ] ]
        [ title
        , div
            [ style
                [ ( "padding", "1rem" ) ]
            ]
            [ mailForm model
            , domainSaver model.value model.settings.baseDomain
            , mailsList model.emails model.notes
            ]
        ]


title : Html Msg
title =
    h1
        [ style
            [ ( "margin-top", "0" )
            , ( "margin-bottom", "1rem" )
            , ( "padding", "1rem" )
            , ( "background", "hotpink" )
            , ( "text-align", "center" )
            , ( "color", "white" )
            , ( "font-size", "1.1rem" )
            , ( "font-weight", "400" )
            , ( "text-transform", "uppercase" )
            , ( "letter-spacing", ".1rem" )
            ]
        ]
        [ text "Mail generator" ]


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
        [ ul
            [ style
                [ ( "padding-left", "0" )
                , ( "margin-top", "1rem" )
                , ( "list-style", "none" )
                ]
            ]
            (List.reverse (mailItems emails notes))
        , if List.isEmpty emails then
            text ""
          else
            button
                [ type_ "button"
                , onClick ClearEmailsList
                , style [ ( "width", "100%" ) ]
                ]
                [ text "Remove all emails" ]
        ]


mailItems : List Email -> Notes -> List (Html Msg)
mailItems emails notes =
    List.indexedMap (\index email -> mailItem email (Dict.get email.id notes) index) emails


mailItem : Email -> Maybe Note -> Int -> Html Msg
mailItem email note index =
    li
        [ style
            [ ( "padding", "1rem 0" ) ]
        ]
        [ div
            [ style
                [ ( "display", "flex" )
                , ( "justify-content", "space-between" )
                , ( "width", "100%" )
                ]
            ]
            [ span
                [ style [ ( "color", "hotpink" ) ] ]
                [ text email.id ]
            , div
                []
                [ displayDate email.createdAt
                , button [ onClick (Copy email.id) ] [ text "Copy to clipboard" ]
                , button [ onClick (GenerateAdditionalMail email) ] [ text "New" ]
                , button [ onClick (RemoveEmail email.id) ] [ text "Remove" ]
                ]
            ]
        , noteView email.id note
        , div
            [ style
                [ ( "width", "10%" )
                , ( "margin", "0 auto" )
                ]
            ]
            []
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
        div
            [ style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "align-items", "flex-end" )
                ]
            ]
            [ resizableTextArea id noteContent
            , button
                [ onClick (UpdateNote id "") ]
                [ text "Clear" ]
            ]


resizableTextArea : Id -> String -> Html Msg
resizableTextArea id content =
    let
        -- This will make sure the last empty line will still be visible.
        holderAppendix =
            if (String.right 1 content) == "\n" || content == "" then
                " "
            else
                ""

        commonStyles =
            [ ( "font-size", "70%" )
            , ( "padding", ".2rem" )
            , ( "line-height", "1" )
            , ( "font-family", "sans-serif" )
            ]
    in
        div
            [ style
                [ ( "position", "relative" )
                , ( "width", "100%" )
                , ( "margin", ".5rem 0" )
                ]
            ]
            [ div
                [ style
                    (List.append
                        [ ( "white-space", "pre" )
                        , ( "border", "1px solid transparent" )
                        ]
                        commonStyles
                    )
                ]
                [ text (content ++ holderAppendix) ]
            , textarea
                [ value content
                , onInput (UpdateNote id)
                , style
                    (List.append
                        [ ( "position", "absolute" )
                        , ( "top", "0" )
                        , ( "overflow", "hidden" )
                        , ( "resize", "none" )
                        , ( "width", "100%" )
                        , ( "outline", "none" )
                        , ( "height", "100%" )
                        , ( "border", "1px solid whitesmoke" )
                        ]
                        commonStyles
                    )
                , placeholder "Add a note…"
                ]
                []
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
                hour =
                    (toString (Date.hour d)) ++ ":" ++ (toString (Date.minute d))
            in
                span
                    [ style
                        [ ( "font-size", "70%" )
                        , ( "color", "gray" )
                        , ( "margin-right", ".5rem" )
                        ]
                    ]
                    [ text (toString (Date.month d) ++ " " ++ (toString (Date.day d)) ++ ", " ++ hour) ]

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
