module Email exposing (..)

import Regex exposing (..)


type alias AddressValues =
    ( String, String )


type alias Id =
    String


type alias Email =
    { id : Id
    , userName : String
    , host : String
    , count : Int
    , createdAt : String
    }


initialHost : String
initialHost =
    "gmail.com"


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


extractCount : String -> ( String, Int )
extractCount userName =
    userName
        |> String.split "+"
        |> List.drop 1
        |> List.reverse
        |> List.head
        |> Maybe.withDefault ""
        |> String.toInt
        |> Result.withDefault -1
        |> max 0
        |> (,) (Regex.replace All (regex "\\+\\d+$") (always "") userName)


generateEmail : String -> List Email -> String -> Email
generateEmail email emails baseDomain =
    let
        ( fulllUserName, host ) =
            splitAddress email baseDomain

        ( userName, startingCount ) =
            extractCount fulllUserName

        count =
            emails
                |> List.filter (\emailItem -> String.contains userName emailItem.userName && String.contains host emailItem.host)
                |> List.length
                |> (+) 1
                |> (+) startingCount

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
    combineAddress (userName ++ "+" ++ (toString count)) host


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
