module JsonDecoders exposing (liveState, mainSwitchState, healNetwork)

import Json.Decode as Decode
import Json.Decode exposing (..)
import LiveState exposing (LiveState)
import MainSwitchState.Model exposing (MainSwitchState)


healNetwork : Decoder String
healNetwork =
    field "state" string


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
