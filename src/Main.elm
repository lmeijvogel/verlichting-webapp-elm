import Http

import Html exposing (Html, ul, li, div, p, text, button, label, input)
import Html.Attributes exposing (placeholder, href, rel, type_, value)
import Html.Events exposing (onInput, onClick)

import Material
import Material.Button as Button
import Material.Card as Card
import Material.Chip as Chip
import Material.Color as Color
import Material.Grid as Grid
import Material.Layout as Layout
import Material.List as MatList
import Material.Options as Options exposing (css)
import Material.Textfield as Textfield
import Material.Typography as Typo

import Regex exposing (..)

import Programme exposing (Programme)
import Light exposing (..)
import VacationMode exposing (VacationMode, timeOfDayToString)

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
  , vacationMode: VacationMode
  , error : String
  , loginData : LoginModel

  , mdl : Material.Model
  }

init : (Model, Cmd Msg)
init =
  ({
    loggedIn = False,
    availableProgrammes = [],
    currentProgramme = "",
    pendingProgramme = "",
    lights = [],
    vacationMode = VacationMode.new,
    error = "",
    loginData = (LoginModel "" ""),
    mdl = Material.model
  }, checkLoggedIn)


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

  | VacationModeMsg VacationMode.Msg

  | Mdl (Material.Msg Msg)

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
          nextCommand = if loginState.loggedIn then Cmd.batch [getAvailableProgrammes, loadLights, Cmd.map VacationModeMsg VacationMode.load]
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

    VacationModeMsg msg ->
      let
          (newVacationMode, cmd) = VacationMode.update msg model.vacationMode
      in
          ( { model | vacationMode = newVacationMode }, Cmd.map VacationModeMsg cmd)

    -- Boilerplate: Mdl action handler.
    Mdl msg_ ->
        Material.update Mdl msg_ model

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

type alias Mdl = Material.Model

view : Model -> Html Msg
view model =
  Layout.render Mdl model.mdl
    [ Layout.fixedHeader
    ]
    { header = [ Layout.row [ Typo.title ] [ text "Verlichting" ] ]
    , drawer = []
    , tabs = ([], [])
    , main = [
        (if model.loggedIn then
          div [] [
            Grid.grid [] [
              Grid.cell [ Grid.size Grid.Phone 4, Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 4 ] [
                programmesCard model
              ],
              Grid.cell [ Grid.size Grid.Phone 4, Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 4 ] [
                vacationModeCard model
              ],
              Grid.cell [ Grid.size Grid.Phone 4, Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 4 ] [
                lightsCard model
              ]
            ]
            --, div [] [ text model.error ]
          ]
        else
          loginCard model
        )
      ]
    }

loginCard : Model -> Html Msg
loginCard model = Card.view [] [
  Card.title [] [
    Options.styled p [ Typo.title] [text "Please login"]
  ],
  Card.text [] [
    div [] [
      Textfield.render Mdl [0] model.mdl
      [ Textfield.label "Username"
      , Textfield.floatingLabel
      , Textfield.text_
      , Options.onInput UsernameChanged
      ]
      [],
      Textfield.render Mdl [1] model.mdl
      [ Textfield.label "Password"
      , Textfield.floatingLabel
      , Textfield.password
      , Options.onInput PasswordChanged
      ]
      []
    , div [] [
      Button.render Mdl [2] model.mdl
      [ Button.raised
      , Options.onClick SubmitLogin
      ]
      [ text "Log in"]
    ]
  ]
  ]
  ]

programmesCard : Model -> Html Msg
programmesCard model = Card.view [] [
  Card.title [] [
    Options.styled p [ Typo.title] [text "Programma's"]
  ],
  Card.text [] [
    MatList.ul []
        (List.map (\programme -> programmeEntry programme model.mdl model.currentProgramme model.pendingProgramme) model.availableProgrammes)
    ]
  ]


vacationModeCard : Model -> Html Msg
vacationModeCard model =
  let
      on = model.vacationMode.state

      buttonText   = if on then "Disable"
                     else "Enable"
      buttonAction = if on then VacationModeMsg VacationMode.Disable
                     else VacationModeMsg VacationMode.Enable
  in
    Card.view [] [
      Card.title [] [
        let titleText = if on then "Vacation mode is ON"
            else "Vacation mode is OFF"
        in
        Options.styled p [ Typo.title] [ text titleText]
      ],
      Card.text [] [
        MatList.ul [] [
          MatList.li [] [
            MatList.content [] [
              text "Average start time:"
            ]
          , MatList.content2 [] [
              if on then
                text (timeOfDayToString model.vacationMode.averageStartTime)
              else
                input [ type_ "time",
                        value (timeOfDayToString model.vacationMode.averageStartTime),
                        onInput (\s -> VacationModeMsg (VacationMode.StartTimeChanged s))
                      ] []
            ]
          ]
        , MatList.li [] [
            MatList.content [] [
              text "Average end time:"
            ]
          , MatList.content2 [] [
              if on then
                text (timeOfDayToString model.vacationMode.averageEndTime)
              else
                input [ type_ "time",
                        value (timeOfDayToString model.vacationMode.averageEndTime),
                      onInput (\s -> VacationModeMsg (VacationMode.EndTimeChanged s))
                      ] []
            ]
          ]
        ]
      ],
      Card.actions
        [ Card.border ]
        [ Button.render Mdl [1,0] model.mdl
            [ Button.ripple, Button.accent, Options.onClick buttonAction ]
            [ text buttonText ]
        ]
    ]

lightsCard : Model -> Html Msg
lightsCard model = Card.view [] [
    Card.title [] [
      Options.styled p [ Typo.title] [ text "Lichten"]
    ],
    Card.text [] [
      MatList.ul []
        (List.map (\light -> lightEntry light) model.lights)
    ]
  ]

compactListItem : List (Options.Property c m) -> List (Html m) -> Html m
compactListItem listStyles = MatList.li [
      Options.css "padding-top" "0",
      Options.css "padding-bottom" "0"
    ]

programmeEntry : Programme -> Material.Model -> String -> String -> Html Msg
programmeEntry programme mdl currentProgramme pendingProgramme =
  let
      buttonStyles = [ Options.css "width" "100%" ] ++
        if programme.id == currentProgramme then
          [ Button.ripple, Button.colored, Button.raised ]
        else if programme.id == pendingProgramme then
          [ Button.ripple, Button.raised ]
        else
          []
  in
        compactListItem [] [
          MatList.content [
          ] [
            Button.render Mdl
              [ 0 ]
              mdl
              (List.concat [ [
                Options.onClick (ProgrammeClicked programme)
              ], buttonStyles ] )
              [ text programme.name ]
          ]
        ]

lightEntry : Light -> Html Msg
lightEntry light =
  let
      valueDisplay = case light of
          SwitchableLight _ _ _ state ->
            if state then "On"
            else "-"
          DimmableLight _ _ _ intensity ->
            if intensity == 0 then "-"
            else (toString intensity)

      name = case light of
        SwitchableLight _ lightName _ _ -> lightName
        DimmableLight   _ lightName _ _ -> lightName

      chipBackgroundColor = case valueDisplay of
        "-" -> Color.color Color.Blue  Color.S100
        _   -> Color.color Color.Amber Color.S600

      valueBackgroundColor = case valueDisplay of
        "-" -> Color.color Color.Blue   Color.S100
        _   -> Color.color Color.Yellow Color.S300

      valueForegroundColor = case valueDisplay of
        "-" -> Color.white
        _   -> Color.black


  in
      compactListItem [] [
        MatList.content [] [
          Chip.span [
            Options.css "width" "100%"
          , Color.background (chipBackgroundColor)

          ]
          [
            Chip.contact Html.span
            [
              Color.background valueBackgroundColor
            , Color.text valueForegroundColor
            ]
            [ text valueDisplay ]
            , Chip.content [ ]
            [text name ]
          ]
        ]
      ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
