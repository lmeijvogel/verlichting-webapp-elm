module JsonDecoders exposing (availableProgrammes, currentProgramme, activationResponse, liveState, mainSwitchState, PostProgrammeResult(..), healNetwork)

import Json.Decode as Decode
import Json.Decode exposing (..)
import Programmes.Model exposing (Programme)
import LiveState exposing (LiveState)
import MainSwitchState.Model exposing (MainSwitchState)


type PostProgrammeResult
    = Success String
    | Error


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


healNetwork : Decoder String
healNetwork =
    field "state" string


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
                        MainSwitchState.Model.Enabled
                    else
                        MainSwitchState.Model.Disabled
            in
                Decode.succeed newState
    in
        field "state" bool
            |> Decode.andThen convert
