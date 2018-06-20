module View exposing (mainView)

import Html exposing (Html, main_, p, div, footer, header, h1, text, label, input, ul, li, button, span, textarea, a)
import Html.Attributes exposing (style, placeholder, type_, checked, autofocus, disabled, value, href)
import Html.Events exposing (onClick, onCheck, onSubmit, onInput)
import Types exposing (Msg(..), Model, Email, Notes, Note, Id)
import Dict
import Email as E
import Date


colors : { primary : String, secondary : String, background : String }
colors =
    { primary = "hotpink"
    , secondary = "#555"
    , background = "#f5f5f5"
    }


boxShadow : String
boxShadow =
    "0 6px 8px rgba(102,119,136,.03), 0 1px 2px rgba(102,119,136,.3)"


type ButtonStyle
    = Primary
    | Secondary


mainView : Model -> Html Msg
mainView model =
    main_
        [ style
            [ ( "font-size", "1rem" )
            , ( "font-family", "sans-serif" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            , ( "height", "100%" )
            ]
        ]
        [ top model
        , bodySection model
        , bottom
        ]


top : Model -> Html Msg
top model =
    header
        [ style
            [ ( "width", "100%" )
            , ( "display", "flex" )
            , ( "justify-content", "space-between" )
            , ( "align-items", "center" )
            , ( "border-bottom", "1px solid " ++ colors.primary )
            , ( "padding", ".5rem 1rem" )
            , ( "background", colors.primary )
            , ( "color", "white" )
            , ( "box-shadow", boxShadow )
            ]
        ]
        [ h1
            [ style
                [ ( "display", "inline-block" )
                , ( "font-size", "1.1rem" )
                , ( "font-weight", "400" )
                , ( "text-transform", "uppercase" )
                , ( "letter-spacing", ".1rem" )
                ]
            ]
            [ text "Mail generator" ]
        , mailForm model
        , actionSection model
        ]


actionSection : Model -> Html Msg
actionSection model =
    div
        [ style
            [ ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "justify-content", "right" )
            ]
        ]
        [ domainSaver model.value model.settings.baseDomain
        , label
            [ style
                [ ( "font-size", "70%" )
                , ( "margin-left", "5px" )
                ]
            ]
            [ input
                [ onCheck AutoClipboard
                , type_ "checkbox"
                , checked model.settings.autoClipboard
                ]
                []
            , text "Copy to clipboard"
            ]
        ]


bodySection : Model -> Html Msg
bodySection model =
    div
        [ style
            [ ( "padding", "0 1rem 1rem" )
            , ( "width", "100%" )
            , ( "max-width", "800px" )
            , ( "margin", "1rem auto 0" )
            ]
        ]
        [ if List.isEmpty model.emails then
            manual
          else
            mailsList model.emails model.notes
        ]


manual : Html Msg
manual =
    div
        [ style
            [ ( "margin", "2rem" )
            , ( "padding", "1.5rem 2rem" )
            , ( "color", colors.secondary )
            , ( "line-height", "2" )
            , ( "font-size", "90%" )
            , ( "background-color", colors.background )
            , ( "box-shadow", boxShadow )
            , ( "border-radius", "2px" )
            ]
        ]
        [ p [ style [ ( "margin-top", "0" ) ] ]
            [ text """
            Generate and keep track of email addresses you're using to test your app with different users accounts.
            Add notes and easily manage the generated emails to make your QA job easy. Works with
            Gmail, custom domains and anything that can handle a + suffix.
        """
            ]
        , p [] [ text """
            You can start the counter from an arbitrary number by passing the number you want after the last + (eg. hello+10@gmail.com).
            The last generated email is automatically saved in the clipboard and is ready to paste in your app.
        """ ]
        , p [] [ text """
            Emails and notes are stored locally in your browser. You have full control of your data.
        """ ]
        , p [ style [ ( "margin-bottom", "0" ), ( "color", colors.primary ) ] ] [ text "Have fun!" ]
        ]


mailInput : Html Msg
mailInput =
    input
        [ onInput Input
        , autofocus True
        , style
            [ ( "border", "none" )
            , ( "font-size", "inherit" )
            , ( "padding", ".5rem" )
            , ( "font-family", "inherit" )
            , ( "width", "100%" )
            , ( "background", "white" )
            , ( "border-radius", "2px" )
            , ( "color", colors.secondary )
            , ( "box-shadow", boxShadow )
            ]
        ]
        []


mailsList : List Email -> Notes -> Html Msg
mailsList emails notes =
    div []
        [ ul
            [ style
                [ ( "padding-left", "0" )
                , ( "margin", "0" )
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
                , style (withButtonStyles Primary [])
                ]
                [ text "Remove all emails" ]
        ]


mailItems : List Email -> Notes -> List (Html Msg)
mailItems emails notes =
    List.indexedMap (\index email -> mailItem email (Dict.get email.id notes) index) emails


withButtonStyles : ButtonStyle -> List ( String, String ) -> List ( String, String )
withButtonStyles buttonStyle styles =
    let
        borderColor =
            case buttonStyle of
                Primary ->
                    colors.primary

                Secondary ->
                    "gray"
    in
        List.append
            styles
            [ ( "background", "none" )
            , ( "border", "none" )
            , ( "border-bottom", "1px solid" )
            , ( "border-bottom-color", borderColor )
            , ( "padding", ".3rem" )
            , ( "cursor", "pointer" )
            ]


mailItem : Email -> Maybe Note -> Int -> Html Msg
mailItem email note _ =
    li
        [ style
            [ ( "padding", "1rem" )
            , ( "background", "#fff" )
            , ( "box-shadow", boxShadow )
            , ( "border-radius", "2px" )
            , ( "margin-bottom", "1rem" )
            ]
        ]
        [ div
            [ style
                [ ( "display", "flex" )
                , ( "justify-content", "space-between" )
                , ( "width", "100%" )
                ]
            ]
            [ span
                [ style
                    [ ( "color", colors.primary )
                    , ( "cursor", "pointer" )
                    ]
                , onClick (CopyToClipboard email.id)
                ]
                [ text email.id ]
            , div
                []
                [ displayDate email.createdAt
                , button
                    [ style (withButtonStyles Primary [ ( "margin", "0 .2rem" ) ])
                    , onClick (CopyToClipboard email.id)
                    ]
                    [ text "Copy to clipboard" ]
                , button
                    [ style (withButtonStyles Primary [ ( "margin", "0 .2rem" ) ])
                    , onClick (GenerateAdditionalMail email)
                    ]
                    [ text "New like this" ]
                , button
                    [ style (withButtonStyles Primary [ ( "margin", "0 .2rem" ) ])
                    , onClick (RemoveEmail email.id)
                    ]
                    [ text "Remove" ]
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
        content =
            Maybe.withDefault "" note
    in
        div
            [ style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "align-items", "flex-end" )
                ]
            ]
            [ resizableTextArea id content
            , button
                [ onClick (UpdateNote id "")
                , style (withButtonStyles Secondary [])
                , disabled (content == "")
                ]
                [ text "Clear the note" ]
            ]


resizableTextArea : Id -> Note -> Html Msg
resizableTextArea id content =
    let
        -- This will make sure the last empty line will still be visible.
        holderAppendix =
            if String.right 1 content == "\n" || content == "" then
                " "
            else
                ""

        commonStyles =
            [ ( "font-size", "80%" )
            , ( "padding", ".2rem .5rem" )
            , ( "line-height", "1.3" )
            , ( "font-family", "sans-serif" )
            , ( "border", "1px solid transparent" )
            ]
    in
        div
            [ style
                [ ( "position", "relative" )
                , ( "width", "100%" )
                , ( "margin-top", ".5rem" )
                ]
            ]
            [ div
                [ style
                    (List.append
                        commonStyles
                        [ ( "white-space", "pre-wrap" ) ]
                    )
                ]
                [ text (content ++ holderAppendix) ]
            , textarea
                [ value content
                , onInput (UpdateNote id)
                , style
                    (List.append
                        commonStyles
                        [ ( "position", "absolute" )
                        , ( "top", "0" )
                        , ( "overflow", "hidden" )
                        , ( "resize", "none" )
                        , ( "width", "100%" )
                        , ( "outline", "none" )
                        , ( "height", "100%" )
                        , ( "border-left", "1px solid #ddd" )
                        , ( "background", "#fff" )
                        , ( "color", colors.secondary )
                        ]
                    )
                , placeholder "Add a note…"
                ]
                []
            ]


mailForm : Model -> Html Msg
mailForm model =
    Html.form
        [ onSubmit GenerateNewMail
        , style
            [ ( "position", "relative" )
            , ( "display", "flex" )
            , ( "width", "300px" )
            ]
        ]
        [ div
            [ style [ ( "width", "100%" ) ] ]
            [ mailInput
            , hostAddition model.value model.settings.baseDomain
            ]
        , button
            [ style
                (withButtonStyles Primary [ ( "font-size", "1rem" ), ( "color", "#fff" ) ])
            ]
            [ text "→" ]
        ]


hostAddition : String -> String -> Html Msg
hostAddition email baseDomain =
    let
        ( userName, host ) =
            E.splitAddress email baseDomain

        baseColor =
            "#bbb"

        hostColor =
            if host == baseDomain && not (String.contains baseDomain email) then
                baseColor
            else
                "transparent"

        atColor =
            if String.contains "@" email then
                "transparent"
            else
                baseColor

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
                    String.padLeft 2 '0' (toString (Date.hour d)) ++ ":" ++ String.padLeft 2 '0' (toString (Date.minute d))
            in
                span
                    [ style
                        [ ( "font-size", "70%" )
                        , ( "color", "gray" )
                        , ( "margin-right", ".5rem" )
                        ]
                    ]
                    [ text (toString (Date.month d) ++ " " ++ toString (Date.day d) ++ ", " ++ hour) ]

        Err _ ->
            text ""


domainSaver : String -> String -> Html Msg
domainSaver email baseDomain =
    let
        ( _, host ) =
            E.splitAddress email baseDomain
    in
        button
            [ disabled (host == baseDomain)
            , onClick (SetBaseDomain host)
            , style (withButtonStyles Secondary [ ( "color", "#fff" ), ( "border-bottom-color", "currentcolor" ) ])
            ]
            [ text "Save domain" ]


bottom : Html Msg
bottom =
    footer
        [ style
            [ ( "text-align", "center" )
            , ( "color", "#bbb" )
            , ( "font-size", "90%" )
            ]
        ]
        [ p [] [ text "★" ]
        , p [ style [ ( "padding-left", "6px" ) ] ]
            [ text "made by "
            , a [ style [ ( "color", "inherit" ) ], href "https://github.com/mateuszkocz" ] [ text "MK" ]
            , text " — src @ "
            , a [ style [ ( "color", "inherit" ) ], href "https://github.com/mateuszkocz/TODO" ] [ text "GitHub" ]
            ]
        ]
