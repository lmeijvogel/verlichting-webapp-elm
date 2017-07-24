module Light exposing (Light, LightValue(..), Msg(Update,Save), load, update, save)

import Debug exposing (log)
import Http
import Json.Decode exposing (..)

type LightValue =
  Level Int
  | State Bool

type alias Light =
  {
    id: Int,
    name : String,
    displayName : String,
    value: LightValue
  }

type Msg =
  Received (Result Http.Error (List Light))
  | Update Light LightValue Bool
  | Save Light
  | ChangeAcknowledged (Result Http.Error LightValue)

load: Cmd Msg
load =
  let
      url = "/my_zwave/current_lights"
      request =
        Http.get url decodeLights
  in
      Http.send Received request

update : Msg -> List Light -> (Result Http.Error (List Light), Cmd Msg)
update msg lights =
  case msg of
    Received (Ok newLights) -> (Ok newLights, Cmd.none)
    Received (Err error) -> (Err error, Cmd.none)
    {- The `saveAfter` here is a workaround for a usability "quirk":
     - When a Slider is used, we don't want to bombard the server with
     - requests for each small chang, so we do a separate 'save' when
     - the mouse button is released.
     - However, when a Toggle is used, we want to save the new value immediately,
     - since waiting for the mouse button release is probably not reliable.
    -}
    Update light value saveAfter ->
      let
        newLight = { light | value = value }
        nextCommand = if saveAfter then (save newLight) else Cmd.none
      in
        (Ok (List.map (\l -> if l.id == light.id then newLight else l ) lights), nextCommand)
    Save light ->
      (Ok lights, save light)
    ChangeAcknowledged (Ok _) -> (Ok lights, Cmd.none)
    ChangeAcknowledged (Err error) -> (Err error, Cmd.none)

save: Light -> Cmd Msg
save light =
  let
      url = case light.value of
        State state ->
          let stateString = if state then "on" else "off"
          in
              "/my_zwave/light/" ++ (toString light.id) ++ "/switch/" ++ stateString
        Level intensity -> "/my_zwave/light/" ++ (toString light.id) ++ "/level/" ++ (toString intensity)
      request =
        Http.post url Http.emptyBody decodeChangeResponse
  in
      Http.send ChangeAcknowledged request

decodeLights : Decoder (List Light)
decodeLights =
  let
    toLight : Decoder Light
    toLight = map4 Light
                (field "node_id" int)
                (field "name" string)
                (field "display_name" string)
                (oneOf [
                  map Level (field "value" int)
                  , map State (field "state" bool)
                ])

  in
    field "lights" (list toLight)

decodeChangeResponse: Decoder LightValue
decodeChangeResponse =
  oneOf [
    map Level (field "level" int)
  , map State (field "state" bool)
  ]
