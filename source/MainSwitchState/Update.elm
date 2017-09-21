module MainSwitchState.Update exposing (load, update, Msg(..))

import Http
import Json.Decode exposing (Decoder)

import JsonDecoders
import MainSwitchState.Model exposing (MainSwitchState, MainSwitchModel)
import Material

type Msg
    = MainSwitchStateClicked MainSwitchState
    | MainSwitchStateReceived (Result Http.Error MainSwitchState)
    | Mdl (Material.Msg Msg)

update : MainSwitchModel -> Msg -> (MainSwitchModel, Cmd Msg)
update model msg =
    case msg of
        MainSwitchStateClicked state ->
            ( { model | state = state }, setMainSwitchState state )

        MainSwitchStateReceived (Ok newState) ->
            ( { model | state = newState }, Cmd.none )

        MainSwitchStateReceived (Err _) ->
            ( { model | state = MainSwitchState.Model.Error }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model

get : Decoder a -> (Result Http.Error a -> Msg) -> String -> Cmd Msg
get decoder msg url =
    let
        request =
            Http.get url decoder
    in
        Http.send msg request

load : Cmd Msg
load =
    get JsonDecoders.mainSwitchState MainSwitchStateReceived "/my_zwave/main_switch"


setMainSwitchState : MainSwitchState -> Cmd Msg
setMainSwitchState newState =
    let
        stateString =
            case newState of
                MainSwitchState.Model.Disabled ->
                    "false"

                _ ->
                    "true"

        url =
            "/my_zwave/main_switch/" ++ stateString

        request =
            Http.post url Http.emptyBody JsonDecoders.mainSwitchState
    in
        Http.send MainSwitchStateReceived request

