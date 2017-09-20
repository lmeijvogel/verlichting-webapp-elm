module JsonDecoders exposing (availableProgrammes, currentProgramme, activationResponse, vacationMode, liveState, mainSwitchState, PostProgrammeResult(..), VacationModeResult)

import Json.Decode as Decode
import Json.Decode exposing (..)
import Programmes.Model exposing (Programme)
import LiveState exposing (LiveState)
import MainSwitchState exposing (MainSwitchState)
import TimeOfDay exposing (TimeOfDay)


type PostProgrammeResult
    = Success String
    | Error


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
    let
        decodeResult : Bool -> Decoder PostProgrammeResult
        decodeResult success =
            if success then
                map Success (field "programme" string)
            else
                Decode.succeed Error
    in
        field "success" bool |> Decode.andThen decodeResult


vacationMode : TimeOfDay -> TimeOfDay -> Decoder VacationModeResult
vacationMode defaultStart defaultEnd =
    let
        convert : TimeOfDay -> Maybe String -> Decoder TimeOfDay
        convert default input =
            let
                defaultTimeString =
                    TimeOfDay.timeOfDayToString default

                timeOfDayResult =
                    TimeOfDay.timeOfDayFromString (Maybe.withDefault defaultTimeString input)
            in
                case timeOfDayResult of
                    Ok timeOfDay ->
                        Decode.succeed timeOfDay

                    _ ->
                        Decode.fail ("Invalid input value" ++ (toString input) ++ "'")
    in
        map3 VacationModeResult
            (field "state" string)
            ((maybe (field "start_time" string)) |> Decode.andThen (convert defaultStart))
            ((maybe (field "end_time" string)) |> Decode.andThen (convert defaultEnd))


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
