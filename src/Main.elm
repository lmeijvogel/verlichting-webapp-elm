import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Regex exposing (..)

import Programme exposing (Programme)
import Light exposing (..)
import JsonDecoders
import Json.Encode

main = Html.program { init = init,
                      view = view,
                      update = update,
                      subscriptions = subscriptions
                    }
type alias LoginModel =
  {
    username: String,
    password: String
  }

-- MODEL

type alias Model =
  {
    loggedIn : Bool
  , availableProgrammes: List Programme
  , currentProgramme : String
  , pendingProgramme : String
  , lights : List Light
  , error : String
  , loginData : LoginModel
  }

init : (Model, Cmd Msg)
init =
  (Model False [] "" "" [] "" (LoginModel "" ""), checkLoggedIn)


-- UPDATE

type Msg
  = LoginChecked (Result Http.Error JsonDecoders.LoginState)
  | UsernameChanged String
  | PasswordChanged String
  | SubmitLogin
  | ProgrammesReceived (Result Http.Error (List Programme))
  | CurrentProgrammeReceived (Result Http.Error String)
  | ProgrammeClicked Programme
  | ActivationResponseReceived (Result Http.Error JsonDecoders.PostProgrammeResult)
  | LightsReceived (Result Http.Error (List Light))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ProgrammesReceived (Ok availableProgrammes) ->
      ( { model | availableProgrammes = availableProgrammes }, getCurrentProgramme)
    ProgrammesReceived (Err error) ->
      ( { model | error = "Could not retrieve programmes list: " ++ (toString error) }, Cmd.none )
    CurrentProgrammeReceived (Ok id) ->
      ( { model | currentProgramme = id }, Cmd.none )
    CurrentProgrammeReceived (Err _) ->
      ( { model | error = "Could not retrieve current programme" }, Cmd.none )
    ProgrammeClicked programme ->
      ( { model | pendingProgramme = programme.id }, activateProgramme programme )
    ActivationResponseReceived (Ok result) ->
      if result.success then
        ( { model | currentProgramme = result.programme, pendingProgramme = "" }, loadLights )
      else
        ( { model | error = "Result was not success" }, Cmd.none)
    ActivationResponseReceived (Err _) ->
      ( { model | error = "An error occurred" }, Cmd.none)
    LightsReceived (Ok lights) ->
      ( { model | lights = lights }, Cmd.none)
    LightsReceived (Err error) ->
      ( { model | error = (toString error)}, Cmd.none)
    LoginChecked (Ok loginState) ->
      let
          nextCommand = if loginState.loggedIn then Cmd.batch [getAvailableProgrammes, loadLights]
                        else Cmd.none
      in
        ( { model | loggedIn = loginState.loggedIn }, nextCommand )
    LoginChecked (Err error) ->
      ( { model | loggedIn = False, error = "Not logged in" }, Cmd.none)
    UsernameChanged username ->
      let loginData = model.loginData
      in
        ( { model | loginData = { loginData | username = username } }, Cmd.none )
    PasswordChanged password ->
      let loginData = model.loginData
      in
        ( { model | loginData = { loginData | password = password } }, Cmd.none )
    SubmitLogin ->
      ( model, logIn model )


checkLoggedIn : Cmd Msg
checkLoggedIn =
  let
      url = "/my_zwave/login/show"
      request =
        Http.get url JsonDecoders.checkLogin
  in
      Http.send LoginChecked request

logIn : Model -> Cmd Msg
logIn model =
  let
      url = "/my_zwave/login/create"
      requestData = [
        ("username", Json.Encode.string model.loginData.username),
        ("password", Json.Encode.string model.loginData.password)
      ]
       |> Json.Encode.object
       |> Http.jsonBody

      request =
        Http.post url requestData JsonDecoders.checkLogin
  in
      Http.send LoginChecked request


getAvailableProgrammes : Cmd Msg
getAvailableProgrammes =
  let
      url = "/my_zwave/available_programmes"

      request =
        Http.get url JsonDecoders.availableProgrammes
  in
    Http.send ProgrammesReceived request

getCurrentProgramme : Cmd Msg
getCurrentProgramme =
  let
      url = "/my_zwave/current_programme"
      request =
        Http.get url JsonDecoders.currentProgramme
  in
      Http.send CurrentProgrammeReceived request

activateProgramme : Programme -> Cmd Msg
activateProgramme programme =
  let
      url = "/my_zwave/programme/" ++ programme.id ++ "/start"
      request =
        Http.post url Http.emptyBody JsonDecoders.activationResponse
  in
      Http.send ActivationResponseReceived request

loadLights : Cmd Msg
loadLights =
  let
      url = "/my_zwave/current_lights"
      request =
        Http.get url JsonDecoders.lights
  in
      Http.send LightsReceived request

-- VIEW

view : Model -> Html Msg
view model =
  body [] [
    node "link" [rel "stylesheet", href "style.css"] [],
      (if model.loggedIn then
        div []
          [ h2 [] [text "Available programmes"]
          , ul []
              (List.map (\programme -> programmeEntry programme model.currentProgramme model.pendingProgramme) model.availableProgrammes)
          , ul []
              (List.map (\light -> lightEntry light) model.lights)
          , div [] [ text model.error ]
          ]
      else
        loginScreen model.loginData
      )
    ]

loginScreen : LoginModel -> Html Msg
loginScreen loginData =
  div [] [
    div [] [
      label [] [text "Username"]
    , input [placeholder "Username", onInput UsernameChanged] []
    ]
    , div [] [
      label [] [text "Password"]
    , input [type_ "password", placeholder "Password", onInput PasswordChanged] []
    ]
    , div [] [
      button [ onClick SubmitLogin ] [ text "Submit" ]
    ]

  ]

programmeEntry : Programme -> String -> String -> Html Msg
programmeEntry programme currentProgramme pendingProgramme =
  let
      buttonText =
        if programme.id == currentProgramme then
          programme.name ++ " (current)"
        else if programme.id == pendingProgramme then
          programme.name ++ " (pending)"
        else
          programme.name
  in
      li [onClick (ProgrammeClicked programme)] [text buttonText]

lightEntry : Light -> Html Msg
lightEntry light =
  case light of
    SwitchableLight _ name _ state ->
      let onOffDisplay =
            case state of
              True -> "On"
              False -> "-"
      in
        li [] [text (name ++ " (" ++ onOffDisplay ++ ")")]
    DimmableLight _ name _ intensity ->
      let intensityDisplay =
            if intensity == 0 then "-"
            else (toString intensity)
      in
        li [] [text (name ++ " (" ++ intensityDisplay ++ ")")]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
