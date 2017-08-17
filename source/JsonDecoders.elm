module JsonDecoders exposing (availableProgrammes, currentProgramme, activationResponse, vacationMode, liveState, mainSwitchState, PostProgrammeResult, VacationModeResult)

import Json.Decode as Decode
import Json.Decode exposing (..)
import Programme exposing (Programme)
import LiveState exposing (LiveState)
import MainSwitchState exposing (MainSwitchState)


type alias PostProgrammeResult =
    { success : Bool
    , programme : String
    , recipients : Int
    }


type alias VacationModeResult =
    { state : String
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
    map3 PostProgrammeResult
        (field "success" bool)
        (field "programme" string)
        (field "recipients" int)


vacationMode : Decoder VacationModeResult
vacationMode =
    map VacationModeResult (field "state" string)


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
