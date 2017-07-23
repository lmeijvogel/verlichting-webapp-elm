module Login exposing (LoginState, Msg, newLoginState, checkLoggedIn, update, logIn)

import Http


import Json.Decode exposing (..)
import Json.Encode

type Msg =
 LoginChecked (Result Http.Error LoginState)

type alias LoginState =
  {
    loggedIn: Bool
  }

newLoginState : LoginState
newLoginState = LoginState False

update : Msg -> LoginState -> (LoginState, Bool)
update msg loginState =
  case msg of
    LoginChecked (Ok newLoginState) ->
      let stateChanged = loginState.loggedIn /= newLoginState.loggedIn
      in
        ( { loginState | loggedIn = newLoginState.loggedIn }, stateChanged )
    LoginChecked (Err error) ->
      let stateChanged = loginState.loggedIn == True
      in
        ( { loginState | loggedIn = False}, stateChanged )

checkLoggedIn : Cmd Msg
checkLoggedIn =
  let
      url = "/my_zwave/login/show"
      request =
        Http.get url decodeLogin
  in
      Http.send LoginChecked request

logIn : String -> String -> Cmd Msg
logIn username password =
  let
      url = "/my_zwave/login/create"
      requestData = [
        ("username", Json.Encode.string username),
        ("password", Json.Encode.string password)
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
      convert result = Json.Decode.succeed (LoginState result)
  in
    (field "loggedIn" bool)
    |> Json.Decode.andThen convert
