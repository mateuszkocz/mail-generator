module Email exposing (..)


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


generateEmail : String -> List Email -> String -> Email
generateEmail email emails baseDomain =
    let
        ( userName, host ) =
            splitAddress email baseDomain

        count =
            emails
                |> List.filter (\emailItem -> String.contains userName emailItem.userName && String.contains host emailItem.host)
                |> List.length
                |> (+) 1

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
