module JsonDecoders exposing (csrfToken, healNetwork, liveState)

import Json.Decode as Decode exposing (..)
import LiveState


healNetwork : Decoder String
healNetwork =
    field "state" string


csrfToken : Decoder String
csrfToken =
    field "csrf_token" string


liveState : Decoder LiveState.State
liveState =
    let
        convert : Bool -> Decoder LiveState.State
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
