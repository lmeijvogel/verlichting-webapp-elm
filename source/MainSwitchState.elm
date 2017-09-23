module MainSwitchState exposing (Model, State(..), Msg(..), new, load, update)

import Http
import Json.Decode as Decode
import Json.Decode exposing (..)
import Material


type State
    = Unknown
    | Enabled
    | Disabled
    | Error


type alias Model =
    { state : State
    , mdl : Material.Model
    }


new : Model
new =
    { state = Unknown
    , mdl = Material.model
    }



-- UPDATE --


type Msg
    = MainSwitchStateClicked State
    | MainSwitchStateReceived (Result Http.Error State)
    | Mdl (Material.Msg Msg)


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        MainSwitchStateClicked state ->
            ( { model | state = state }, setMainSwitchState state )

        MainSwitchStateReceived (Ok newState) ->
            ( { model | state = newState }, Cmd.none )

        MainSwitchStateReceived (Err _) ->
            ( { model | state = Error }, Cmd.none )

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
    get decodeMainSwitchState MainSwitchStateReceived "/my_zwave/main_switch"


setMainSwitchState : State -> Cmd Msg
setMainSwitchState newState =
    let
        stateString =
            case newState of
                Disabled ->
                    "false"

                _ ->
                    "true"

        url =
            "/my_zwave/main_switch/" ++ stateString

        request =
            Http.post url Http.emptyBody decodeMainSwitchState
    in
        Http.send MainSwitchStateReceived request


decodeMainSwitchState : Decoder State
decodeMainSwitchState =
    let
        convert : Bool -> Decoder State
        convert state =
            let
                newState =
                    if state then
                        Enabled
                    else
                        Disabled
            in
                Decode.succeed newState
    in
        field "state" bool
            |> Decode.andThen convert
