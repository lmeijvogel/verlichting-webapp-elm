module Login exposing (Model, LoginState(..), Msg, new, checkLoggedIn, update, logIn)

import Http
import Json.Decode exposing (..)
import Json.Encode


type LoginState
    = Unknown
    | NotLoggedIn
    | LoggedIn


type alias Model =
    { state : LoginState
    }


new : Model
new =
    { state = Unknown
    }


type Msg
    = LoginChecked (Result Http.Error LoginState)


update : Msg -> Model -> ( Model, Bool )
update msg model =
    case msg of
        LoginChecked (Ok newLoginState) ->
            let
                stateChanged =
                    model.state /= newLoginState
            in
                ( { model | state = newLoginState }, stateChanged )

        LoginChecked (Err error) ->
            let
                stateChanged =
                    model.state == LoggedIn
            in
                ( { model | state = NotLoggedIn }, stateChanged )


checkLoggedIn : Cmd Msg
checkLoggedIn =
    let
        url =
            "/my_zwave/login/show"

        request =
            Http.get url decodeLogin
    in
        Http.send LoginChecked request


logIn : String -> String -> Cmd Msg
logIn username password =
    let
        url =
            "/my_zwave/login/create"

        requestData =
            [ ( "username", Json.Encode.string username )
            , ( "password", Json.Encode.string password )
            ]
                |> Json.Encode.object
                |> Http.jsonBody

        request =
            Http.post url requestData decodeLogin
    in
        Http.send LoginChecked request


decodeLogin : Decoder LoginState
decodeLogin =
    let
        convert result =
            let
                resultingState =
                    if result then
                        LoggedIn
                    else
                        NotLoggedIn
            in
                Json.Decode.succeed resultingState
    in
        (field "loggedIn" bool)
            |> Json.Decode.andThen convert
