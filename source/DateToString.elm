module DateToString exposing (dateToString)

import Date exposing (Date, year, month, day)


dateToString : Date.Date -> Date.Date -> String
dateToString currentDate date =
    if (isTheSameDay currentDate date) then
        toTimeString date
    else
        (toDateString date) ++ " " ++ (toTimeString date)


toDateString : Date -> String
toDateString date =
    (toString (Date.day date))
        ++ " "
        ++ (monthToString (Date.month date))


toTimeString : Date -> String
toTimeString date =
    (pad2 (Date.hour date))
        ++ ":"
        ++ (pad2 (Date.minute date))


pad2 : Int -> String
pad2 i =
    String.pad 2 '0' (toString i)


isTheSameDay : Date -> Date -> Bool
isTheSameDay date1 date2 =
    (year date1)
        == (year date2)
        && (month date1)
        == (month date2)
        && (day date1)
        == (day date2)


monthToString : Date.Month -> String
monthToString month =
    case month of
        Date.Jan ->
            "jan"

        Date.Feb ->
            "feb"

        Date.Mar ->
            "maa"

        Date.Apr ->
            "apr"

        Date.May ->
            "mei"

        Date.Jun ->
            "jun"

        Date.Jul ->
            "jul"

        Date.Aug ->
            "aug"

        Date.Sep ->
            "sep"

        Date.Oct ->
            "okt"

        Date.Nov ->
            "nov"

        Date.Dec ->
            "dec"
