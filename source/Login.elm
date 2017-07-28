module Login exposing (LoginState(..), Msg, new, checkLoggedIn, update, logIn)

import Http
import Json.Decode exposing (..)
import Json.Encode


type Msg
    = LoginChecked (Result Http.Error LoginState)


type LoginState
    = Unknown
    | NotLoggedIn
    | LoggedIn


new : LoginState
new =
    Unknown


update : Msg -> LoginState -> ( LoginState, Bool )
update msg loginState =
    case msg of
        LoginChecked (Ok newLoginState) ->
            let
                stateChanged =
                    loginState /= newLoginState
            in
                ( newLoginState, stateChanged )

        LoginChecked (Err error) ->
            let
                stateChanged =
                    loginState == LoggedIn
            in
                ( NotLoggedIn, stateChanged )


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
