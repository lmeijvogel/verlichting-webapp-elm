module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import DateToStringTest exposing (all)


suite : Test
suite =
    DateToStringTest.all
