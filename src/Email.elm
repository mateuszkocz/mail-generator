module Email exposing (..)


type alias AddressValues =
    ( String, String )


initialHost : String
initialHost =
    "gmail.com"


splitAddress : String -> AddressValues
splitAddress address =
    case String.split "@" address of
        [ userName, "" ] ->
            ( userName, initialHost )

        [ userName ] ->
            ( userName, initialHost )

        [ userName, domain ] ->
            ( userName, domain )

        _ ->
            ( "", initialHost )


combineAddress : String -> String -> String
combineAddress userName host =
    userName ++ "@" ++ host



-- TODO
-- - when some address was removed, duplicates will be generated. Need to use the last value at least. It's still not perfect
--   since the last value also might get removed. Maybe the future plan to store state in localstoage will help.
-- - allow user to star numeration at defined point, eg. "asd+yolo+10" will start generating at 10


generateEmail : String -> List String -> String
generateEmail email emails =
    let
        ( userName, host ) =
            splitAddress email

        count =
            emails
                |> List.filter (\address -> String.contains userName address && String.contains host address)
                |> List.length
                |> (+) 1
    in
        combineAddress (userName ++ "+" ++ (toString count)) host
