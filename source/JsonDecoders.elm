module JsonDecoders exposing (availableProgrammes, currentProgramme, activationResponse, vacationMode, liveState, mainSwitchState, PostProgrammeResult, VacationModeResult)

import Json.Decode as Decode
import Json.Decode exposing (..)
import Programme exposing (Programme)
import LiveState exposing (LiveState)
import MainSwitchState exposing (MainSwitchState)
import TimeOfDay exposing (TimeOfDay)


type alias PostProgrammeResult =
    { success : Bool
    , programme : String
    }


type alias VacationModeResult =
    { state : String
    , start_time : TimeOfDay
    , end_time : TimeOfDay
    }


availableProgrammes : Decoder (List Programme)
availableProgrammes =
    let
        tupleToProgramme : ( String, String ) -> Programme
        tupleToProgramme ( id, name ) =
            Programme id name

        convert : List ( String, String ) -> Decoder (List Programme)
        convert tupleList =
            Decode.succeed (List.map tupleToProgramme tupleList)
    in
        field "availableProgrammes" (keyValuePairs (string))
            |> Decode.andThen convert
            |> Decode.map List.reverse


currentProgramme : Decoder String
currentProgramme =
    field "programme" string


activationResponse : Decoder PostProgrammeResult
activationResponse =
    map2 PostProgrammeResult
        (field "success" bool)
        (field "programme" string)


vacationMode : Decoder VacationModeResult
vacationMode =
    let
        convert : String -> Decoder TimeOfDay
        convert input =
            let
                timeOfDayResult =
                    TimeOfDay.timeOfDayFromString input
            in
                case timeOfDayResult of
                    Ok timeOfDay ->
                        Decode.succeed timeOfDay

                    _ ->
                        Decode.fail input
    in
        map3 VacationModeResult
            (field "state" string)
            ((field "start_time" string) |> Decode.andThen convert)
            ((field "end_time" string) |> Decode.andThen convert)


liveState : Decoder LiveState
liveState =
    let
        convert : Bool -> Decoder LiveState
        convert state =
            let
                newState =
                    if state then
                        LiveState.Live
                    else
                        LiveState.Simulation
            in
                Decode.succeed newState
    in
        field "state" bool
            |> Decode.andThen convert


mainSwitchState : Decoder MainSwitchState
mainSwitchState =
    let
        convert : Bool -> Decoder MainSwitchState
        convert state =
            let
                newState =
                    if state then
                        MainSwitchState.Enabled
                    else
                        MainSwitchState.Disabled
            in
                Decode.succeed newState
    in
        field "state" bool
            |> Decode.andThen convert
