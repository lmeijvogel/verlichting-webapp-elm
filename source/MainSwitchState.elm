module MainSwitchState exposing (Model, Msg(Enable, Disable), new, load, update, enabled)

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


enabled : Model -> Bool
enabled model =
    model.state == Enabled



-- UPDATE --


type Msg
    = Enable
    | Disable
    | StateReceived (Result Http.Error State)
    | Mdl (Material.Msg Msg)


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        Enable ->
            ( { model | state = Enabled }, setState Enabled )

        Disable ->
            ( { model | state = Disabled }, setState Disabled )

        StateReceived (Ok newState) ->
            ( { model | state = newState }, Cmd.none )

        StateReceived (Err _) ->
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
    get decodeState StateReceived "/my_zwave/main_switch"


setState : State -> Cmd Msg
setState newState =
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
            Http.post url Http.emptyBody decodeState
    in
        Http.send StateReceived request


decodeState : Decoder State
decodeState =
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
