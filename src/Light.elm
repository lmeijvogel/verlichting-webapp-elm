module Light exposing (Light(..), Msg, load, update)

import Http
import Json.Decode exposing (..)

type alias State = Bool
type alias Intensity = Int
type alias Name = String
type alias DisplayName = String

type Light
  = SwitchableLight Int Name DisplayName State
  | DimmableLight Int Name DisplayName Intensity

type Msg =
  Received (Result Http.Error (List Light))

load: Cmd Msg
load=
  let
      url = "/my_zwave/current_lights"
      request =
        Http.get url decodeLights
  in
      Http.send Received request

update : Msg -> List Light -> (List Light, Cmd Msg)
update msg lights =
  case msg of
    Received (Ok newLights) ->
      ( newLights, Cmd.none)
    Received (Err error) ->
      ( lights, Cmd.none)

decodeLights : Decoder (List Light)
decodeLights =
  let
    toLight : Decoder Light
    toLight = Json.Decode.oneOf
      [
        map4 DimmableLight
          (field "node_id" int)
          (field "name" string)
          (field "display_name" string)
          (field "value" int)
       ,
        map4 SwitchableLight
          (field "node_id" int)
          (field "name" string)
          (field "display_name" string)
          (field "state" bool)
      ]
  in
    field "lights" (list toLight)
