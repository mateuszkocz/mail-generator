module Email exposing (initialHost, generateEmail, generateAdditionalEmail, splitAddress, empty)

import Regex exposing (Regex)
import Types exposing (Email, AddressValues)


initialHost : String
initialHost =
    "gmail.com"


digitRegex : Regex
digitRegex =
    Regex.regex "\\+\\d+$"


splitAddress : String -> String -> AddressValues
splitAddress address defaultDomain =
    case String.split "@" address of
        [ userName, "" ] ->
            ( userName, defaultDomain )

        [ userName ] ->
            ( userName, defaultDomain )

        [ userName, domain ] ->
            ( userName, domain )

        _ ->
            ( "", defaultDomain )


combineAddress : String -> String -> String
combineAddress userName host =
    userName ++ "@" ++ host


extractCount : String -> Int
extractCount userName =
    userName
        |> String.split "+"
        |> List.drop 1
        |> last
        |> Maybe.withDefault ""
        |> String.toInt
        |> Result.withDefault -1
        |> max 0


last : List a -> Maybe a
last list =
    list
        |> List.reverse
        |> List.head


generateEmail : String -> List Email -> String -> Email
generateEmail email emails baseDomain =
    let
        ( fullUserName, host ) =
            splitAddress email baseDomain

        startingCount =
            extractCount fullUserName

        userName =
            Regex.replace Regex.All digitRegex (always "") fullUserName

        previousEmail =
            emails
                |> List.filter (\emailItem -> userName == emailItem.userName && String.contains host emailItem.host)
                |> last

        count =
            case previousEmail of
                Just address ->
                    address
                        |> .count
                        |> (+) 1
                        |> max startingCount

                Nothing ->
                    startingCount
                        |> max 1

        id =
            generateEmailId userName host count
    in
        { id = id
        , userName = userName
        , host = host
        , count = count
        , createdAt = ""
        }


generateEmailId : String -> String -> Int -> String
generateEmailId userName host count =
    combineAddress (userName ++ "+" ++ toString count) host


generateAdditionalEmail : Email -> List Email -> Email
generateAdditionalEmail email emails =
    let
        count =
            emails
                |> List.filter (\emailItem -> emailItem.userName == email.userName && emailItem.host == email.host)
                |> List.map .count
                |> List.maximum
                |> Maybe.withDefault 1
                |> (+) 1
    in
        { email
            | id = generateEmailId email.userName email.host count
            , count = count
        }


empty : Email -> Bool
empty email =
    email.userName == ""
