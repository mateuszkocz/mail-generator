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


generateEmail : String -> String -> List String -> String
generateEmail userName host emails =
    let
        sameBases =
            List.filter (\address -> String.contains userName address && String.contains host address) emails

        count =
            (List.length sameBases) + 1
    in
        combineAddress (userName ++ "+" ++ (toString count)) host
