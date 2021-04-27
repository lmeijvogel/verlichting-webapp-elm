module MainSwitchState exposing (Model, Msg(Disable, Enable), isEnabled, load, new, update)

import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
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


isEnabled : Model -> Bool
isEnabled model =
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
    get decodeState StateReceived "/my_zwave_new/main_switch"


setState : State -> Cmd Msg
setState newState =
    let
        encodeJson : State -> Encode.Value
        encodeJson state =
            let
                encodeState : State -> Bool
                encodeState s =
                    case s of
                        Disabled ->
                            False

                        _ ->
                            True
            in
            Encode.object
                [ ( "state", Encode.bool (encodeState state) ) ]

        url =
            "/my_zwave_new/main_switch"

        request =
            Http.post url (Http.jsonBody (encodeJson newState)) decodeState
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
