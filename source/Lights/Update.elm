module Lights.Update exposing (..)

import Http
import Lights.Model exposing (..)
import Material

type Msg
    = Received (Result Http.Error (List Light))
    | ShowLight (Maybe Light)
    | Update Light LightValue Bool
    | Save Light
    | ChangeAcknowledged (Result Http.Error LightValue)
    | Mdl (Material.Msg Msg)


load : Cmd Msg
load =
    let
        url =
            "/my_zwave/current_lights"

        request =
            Http.get url decodeLights
    in
        Http.send Received request


update : Msg -> LightsModel -> (LightsModel, Cmd Msg )
update msg lightsModel =
    case msg of
        Received (Ok newLights) ->
            ( { lightsModel | lights = newLights }, Cmd.none )

        Received (Err error) ->
            ( { lightsModel | error = Just "Error receiving lights" }, Cmd.none )

        ShowLight light ->
            ( { lightsModel | editingLightId = (Maybe.map (\l -> l.id) light) }, Cmd.none )

        {- The `saveAfter` here is a workaround for a usability "quirk":
           - When a Slider is used, we don't want to bombard the server with
           - requests for each small chang, so we do a separate 'save' when
           - the mouse button is released.
           - However, when a Toggle is used, we want to save the new value immediately,
           - since waiting for the mouse button release is probably not reliable.
        -}
        Update light value saveAfter ->
            let
                newLight =
                    { light | value = value }

                nextCommand =
                    if saveAfter then
                        (save newLight)
                    else
                        Cmd.none
                newLights = (List.map
                        (\l ->
                            if l.id == light.id then
                                newLight
                            else
                                l
                        )
                        lightsModel.lights
                    )

            in
                ( { lightsModel | lights = newLights }, nextCommand )

        Save light ->
            ( lightsModel, save light )

        ChangeAcknowledged (Ok _) ->
            ( lightsModel, Cmd.none )

        ChangeAcknowledged (Err error) ->
            ( { lightsModel | error = Just "Error saving change" }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ lightsModel

save : Light -> Cmd Msg
save light =
    let
        url =
            case light.value of
                State state ->
                    let
                        stateString =
                            if state then
                                "on"
                            else
                                "off"
                    in
                        "/my_zwave/light/" ++ (toString light.id) ++ "/switch/" ++ stateString

                Level intensity ->
                    "/my_zwave/light/" ++ (toString light.id) ++ "/level/" ++ (toString intensity)

        request =
            Http.post url Http.emptyBody decodeChangeResponse
    in
        Http.send ChangeAcknowledged request


newIntensityRequested : Light -> Float -> Msg
newIntensityRequested light level =
    Update light (Level (round level)) False



