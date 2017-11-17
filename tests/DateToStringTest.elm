module DateToStringTest exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Date exposing (Date)
import DateToString exposing (dateToString)


now : Date
now =
    date 2017 11 17 22 0


all : Test
all =
    describe "DateToString"
        [ test "in the past" <|
            \() ->
                Expect.equal (dateToString now (date 2017 2 17 15 0)) "17 feb 15:00"
        , test "today" <|
            \() ->
                Expect.equal (dateToString now (date 2017 11 17 13 12)) "13:12"
        ]


date : Int -> Int -> Int -> Int -> Int -> Date
date year month date hour minute =
    let
        dateString =
            (toString year)
                ++ "-"
                ++ (toString month)
                ++ "-"
                ++ (toString date)
                ++ " "
                ++ (toString hour)
                ++ ":"
                ++ (toString minute)
    in
        Date.fromString dateString |> Result.withDefault (Date.fromTime 0)
