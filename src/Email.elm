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



-- TODO
-- - when some address is removed, duplicates will be generated. Need to use the last value at least. It's still not perfect
--   since the last value also might get removed. Maybe the future plan to store state in localstoage will help.
-- - allow user to star numeration at defined point, eg. "asd+10" or "asd+yolo+10" will start generating at 10


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
