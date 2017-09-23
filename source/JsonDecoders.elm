module JsonDecoders exposing (liveState, healNetwork)

import Json.Decode as Decode
import Json.Decode exposing (..)
import LiveState exposing (LiveState)


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
