module TimeOfDay exposing (TimeOfDay, timeOfDayFromString, timeOfDayToString)


type alias TimeOfDay =
    { hour : Int
    , minute : Int
    }


timeOfDayToString : TimeOfDay -> String
timeOfDayToString time =
    let
        hour =
            toString time.hour |> String.pad 2 '0'

        minute =
            toString time.minute |> String.pad 2 '0'
    in
        hour ++ ":" ++ minute


timeOfDayFromString : String -> Result String TimeOfDay
timeOfDayFromString time =
    let
        timeParts =
            String.split ":" time

        hour : Result String Int
        hour =
            timeParts
                |> List.head
                |> Result.fromMaybe "No hour part present"
                |> Result.andThen String.toInt

        minute : Result String Int
        minute =
            timeParts
                |> List.tail
                |> Maybe.andThen List.head
                |> Result.fromMaybe "No minute part exists"
                |> Result.andThen String.toInt
    in
        Result.map2 TimeOfDay hour minute
