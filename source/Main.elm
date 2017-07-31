module Main exposing (..)

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
import Material.Spinner as Spinner
import Material.Options as Options exposing (css)
import Material.Slider as Slider
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Typography as Typo
import Programme exposing (Programme)
import Light exposing (..)
import Login
import VacationMode exposing (VacationMode, timeOfDayToString)
import JsonDecoders


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias LoginFormData =
    { username : String
    , password : String
    }


type alias Model =
    { loginState : Login.LoginState
    , availableProgrammes : List Programme
    , currentProgramme : Maybe String
    , pendingProgramme : Maybe String
    , lights : List Light
    , editingLightId : Maybe Int
    , vacationMode : VacationMode
    , error : String
    , loginFormData : LoginFormData
    , mdl : Material.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { loginState = Login.new
      , availableProgrammes = []
      , currentProgramme = Nothing
      , pendingProgramme = Nothing
      , lights = []
      , editingLightId = Nothing
      , vacationMode = VacationMode.new
      , error = ""
      , loginFormData = newLoginFormData
      , mdl = Material.model
      }
    , Cmd.map LoginMsg Login.checkLoggedIn
    )



-- UPDATE


type Msg
    = UsernameChanged String
    | PasswordChanged String
    | SubmitLogin
    | LoginMsg Login.Msg
    | ProgrammesReceived (Result Http.Error (List Programme))
    | CurrentProgrammeReceived (Result Http.Error String)
    | ProgrammeClicked Programme
    | ActivationResponseReceived (Result Http.Error JsonDecoders.PostProgrammeResult)
    | VacationModeMsg VacationMode.Msg
    | ShowLight (Maybe Light)
    | LightMsg Light.Msg
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProgrammesReceived (Ok availableProgrammes) ->
            ( { model | availableProgrammes = availableProgrammes }, getCurrentProgramme )

        ProgrammesReceived (Err error) ->
            ( { model | error = "Could not retrieve programmes list: " ++ (toString error) }, Cmd.none )

        CurrentProgrammeReceived (Ok id) ->
            ( { model | currentProgramme = Just id }, Cmd.none )

        CurrentProgrammeReceived (Err _) ->
            ( { model | error = "Could not retrieve current programme" }, Cmd.none )

        ProgrammeClicked programme ->
            ( { model | pendingProgramme = Just programme.id }, activateProgramme programme )

        ActivationResponseReceived (Ok result) ->
            if result.success then
                ( { model | currentProgramme = Just result.programme, pendingProgramme = Nothing }, Cmd.map LightMsg Light.load )
            else
                ( { model | error = "Result was not success" }, Cmd.none )

        ActivationResponseReceived (Err _) ->
            ( { model | error = "An error occurred" }, Cmd.none )

        LightMsg msg ->
            case Light.update msg model.lights of
                ( Ok lights, cmd ) ->
                    ( { model | lights = lights }, Cmd.map LightMsg cmd )

                ( Err error, cmd ) ->
                    ( { model | error = toString error }, Cmd.map LightMsg cmd )

        UsernameChanged username ->
            let
                loginFormData =
                    model.loginFormData
            in
                ( { model | loginFormData = { loginFormData | username = username } }, Cmd.none )

        PasswordChanged password ->
            let
                loginFormData =
                    model.loginFormData
            in
                ( { model | loginFormData = { loginFormData | password = password } }, Cmd.none )

        LoginMsg msg ->
            let
                ( newLoginState, loginStateChanged ) =
                    Login.update msg model.loginState

                nextLoginFormData = if loginStateChanged then
                  newLoginFormData -- Clear login form data after logging in
                else
                  model.loginFormData

                nextCommand =
                    if loginStateChanged then
                        initialize
                    else
                        Cmd.none
            in
                ( { model | loginState = newLoginState, loginFormData = nextLoginFormData }, nextCommand )

        SubmitLogin ->
            let
                { username, password } =
                    model.loginFormData
            in
                ( model, Cmd.map LoginMsg (Login.logIn username password) )

        VacationModeMsg msg ->
            let
                ( newVacationMode, cmd ) =
                    VacationMode.update msg model.vacationMode
            in
                ( { model | vacationMode = newVacationMode }, Cmd.map VacationModeMsg cmd )

        ShowLight light ->
            ( { model | editingLightId = (Maybe.map (\l -> l.id) light) }, Cmd.none )

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model


initialize : Cmd Msg
initialize =
    Cmd.batch
        [ getAvailableProgrammes
        , Cmd.map LightMsg Light.load
        , Cmd.map VacationModeMsg VacationMode.load
        ]


newLoginFormData : LoginFormData
newLoginFormData =
    (LoginFormData "" "")


getAvailableProgrammes : Cmd Msg
getAvailableProgrammes =
    let
        url =
            "/my_zwave/available_programmes"

        request =
            Http.get url JsonDecoders.availableProgrammes
    in
        Http.send ProgrammesReceived request


getCurrentProgramme : Cmd Msg
getCurrentProgramme =
    let
        url =
            "/my_zwave/current_programme"

        request =
            Http.get url JsonDecoders.currentProgramme
    in
        Http.send CurrentProgrammeReceived request


activateProgramme : Programme -> Cmd Msg
activateProgramme programme =
    let
        url =
            "/my_zwave/programme/" ++ programme.id ++ "/start"

        request =
            Http.post url Http.emptyBody JsonDecoders.activationResponse
    in
        Http.send ActivationResponseReceived request



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedHeader
        ]
        { header = [ Layout.row [ Typo.title ] [ text "Verlichting" ] ]
        , drawer = []
        , tabs = ( [], [] )
        , main =
            [ case model.loginState of
                Login.Unknown ->
                    div []
                        [ Grid.grid [ Options.center ]
                            [ Grid.cell
                                [ Options.center
                                , Grid.size Grid.Phone 4
                                , Grid.size Grid.Tablet 8
                                , Grid.size Grid.Desktop 12
                                ]
                                [ Spinner.spinner
                                    [ Spinner.active True ]
                                ]
                            ]
                        ]

                Login.LoggedIn ->
                    div []
                        [ Grid.grid []
                            [ Grid.cell [ Grid.size Grid.Phone 4, Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 4 ]
                                [ programmesCard model
                                ]
                            , Grid.cell [ Grid.size Grid.Phone 4, Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 4 ]
                                [ vacationModeCard model
                                ]
                            , Grid.cell [ Grid.size Grid.Phone 4, Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 4 ]
                                [ lightsCard model
                                ]
                            ]
                        , div [] [ text model.error ]
                        ]

                Login.NotLoggedIn ->
                    loginCard model
            ]
        }


loginCard : Model -> Html Msg
loginCard model =
    Card.view []
        [ Card.title []
            [ Options.styled p [ Typo.title ] [ text "Please login" ]
            ]
        , Card.text []
            [ div []
                [ Textfield.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Textfield.label "Username"
                    , Textfield.floatingLabel
                    , Textfield.text_
                    , Options.onInput UsernameChanged
                    ]
                    []
                , Textfield.render Mdl
                    [ 1 ]
                    model.mdl
                    [ Textfield.label "Password"
                    , Textfield.floatingLabel
                    , Textfield.password
                    , Options.onInput PasswordChanged
                    ]
                    []
                , div []
                    [ Button.render Mdl
                        [ 2 ]
                        model.mdl
                        [ Button.raised
                        , Options.onClick SubmitLogin
                        ]
                        [ text "Log in" ]
                    ]
                ]
            ]
        ]


programmesCard : Model -> Html Msg
programmesCard model =
    Card.view []
        [ Card.title []
            [ Options.styled p [ Typo.title ] [ text "Programma's" ]
            ]
        , Card.text []
            [ MatList.ul []
                (List.map (\programme -> programmeEntry programme model.mdl model.currentProgramme model.pendingProgramme) model.availableProgrammes)
            ]
        ]


vacationModeCard : Model -> Html Msg
vacationModeCard model =
    let
        on =
            model.vacationMode.state

        buttonText =
            if on then
                "Disable"
            else
                "Enable"

        buttonAction =
            if on then
                VacationModeMsg VacationMode.Disable
            else
                VacationModeMsg VacationMode.Enable
    in
        Card.view []
            [ Card.title []
                [ let
                    titleText =
                        if on then
                            "Vacation mode is ON"
                        else
                            "Vacation mode is OFF"
                  in
                    Options.styled p [ Typo.title ] [ text titleText ]
                ]
            , Card.text []
                [ MatList.ul []
                    [ MatList.li []
                        [ MatList.content []
                            [ text "Average start time:"
                            ]
                        , MatList.content2 []
                            [ if on then
                                text (timeOfDayToString model.vacationMode.averageStartTime)
                              else
                                input
                                    [ type_ "time"
                                    , value (timeOfDayToString model.vacationMode.averageStartTime)
                                    , onInput (\s -> VacationModeMsg (VacationMode.StartTimeChanged s))
                                    ]
                                    []
                            ]
                        ]
                    , MatList.li []
                        [ MatList.content []
                            [ text "Average end time:"
                            ]
                        , MatList.content2 []
                            [ if on then
                                text (timeOfDayToString model.vacationMode.averageEndTime)
                              else
                                input
                                    [ type_ "time"
                                    , value (timeOfDayToString model.vacationMode.averageEndTime)
                                    , onInput (\s -> VacationModeMsg (VacationMode.EndTimeChanged s))
                                    ]
                                    []
                            ]
                        ]
                    ]
                ]
            , Card.actions
                [ Card.border ]
                [ Button.render Mdl
                    [ 1, 0 ]
                    model.mdl
                    [ Button.ripple, Button.accent, Options.onClick buttonAction ]
                    [ text buttonText ]
                ]
            ]


lightsCard : Model -> Html Msg
lightsCard model =
    case model.editingLightId of
        Just editingLightId ->
            let
                light : Maybe Light
                light =
                    List.filter (\l -> l.id == editingLightId) model.lights |> List.head
            in
                case light of
                    Just light ->
                        singleLightCard light model

                    Nothing ->
                        text "Unknown light selected!"

        Nothing ->
            Card.view []
                [ Card.title []
                    [ Options.styled p [ Typo.title ] [ text "Lichten" ]
                    ]
                , Card.text []
                    [ MatList.ul []
                        (List.map (\light -> lightEntry light) model.lights)
                    ]
                ]


singleLightCard : Light -> Model -> Html Msg
singleLightCard light model =
    Card.view []
        [ Card.title []
            [ Options.styled p [ Typo.title ] [ text light.displayName ]
            ]
        , Card.text []
            [ case light.value of
                State currentState ->
                    Toggles.switch Mdl
                        [ 0 ]
                        model.mdl
                        [ Options.onToggle (LightMsg (Update light (State (not currentState)) True))
                        , Toggles.ripple
                        , Toggles.value currentState
                        ]
                        [ text "Switch" ]

                Level currentLevel ->
                    div []
                        [ Slider.view
                            [ Slider.onChange (newIntensityRequested light)
                            , Options.onMouseUp (LightMsg (Save light))
                            , Slider.value (toFloat currentLevel)
                            , Slider.max 99
                            , Slider.min 0
                            ]
                        , text (toString currentLevel)
                        ]
            ]
        , Card.actions
            [ Card.border ]
            [ Button.render Mdl
                [ 1, 0 ]
                model.mdl
                [ Button.ripple, Button.accent, Options.onClick (ShowLight Nothing) ]
                [ text "Close" ]
            ]
        ]


newIntensityRequested : Light -> Float -> Msg
newIntensityRequested light level =
    LightMsg (Update light (Level (round level)) False)


compactListItem : List (Options.Property c m) -> List (Html m) -> Html m
compactListItem listStyles =
    MatList.li
        [ Options.css "padding-top" "0"
        , Options.css "padding-bottom" "0"
        ]


programmeEntry : Programme -> Material.Model -> Maybe String -> Maybe String -> Html Msg
programmeEntry programme mdl currentProgramme pendingProgramme =
    let
        commonButtonStyles =
            [ Options.css "width" "100%" ]

        extraButtonStyles =
            if currentProgramme == Just programme.id then
                [ Button.ripple, Button.colored, Button.raised ]
            else if pendingProgramme == Just programme.id then
                [ Button.ripple, Button.raised ]
            else
                []

        buttonStyles =
            commonButtonStyles ++ extraButtonStyles
    in
        compactListItem []
            [ MatList.content
                []
                [ Button.render Mdl
                    [ 0 ]
                    mdl
                    (List.concat
                        [ [ Options.onClick (ProgrammeClicked programme)
                          ]
                        , buttonStyles
                        ]
                    )
                    [ text programme.name ]
                ]
            ]


lightEntry : Light -> Html Msg
lightEntry light =
    let
        valueDisplay =
            case light.value of
                State state ->
                    if state then
                        "On"
                    else
                        "-"

                Level level ->
                    if level == 0 then
                        "-"
                    else
                        (toString level)

        chipBackgroundColor =
            case valueDisplay of
                "-" ->
                    Color.color Color.Blue Color.S100

                _ ->
                    Color.color Color.Amber Color.S600

        valueBackgroundColor =
            case valueDisplay of
                "-" ->
                    Color.color Color.Blue Color.S100

                _ ->
                    Color.color Color.Yellow Color.S300

        valueForegroundColor =
            case valueDisplay of
                "-" ->
                    Color.white

                _ ->
                    Color.black
    in
        compactListItem []
            [ MatList.content []
                [ Chip.span
                    [ Options.css "width" "100%"
                    , Color.background (chipBackgroundColor)
                    , Options.onClick (ShowLight (Just light))
                    ]
                    [ Chip.contact Html.span
                        [ Color.background valueBackgroundColor
                        , Color.text valueForegroundColor
                        ]
                        [ text valueDisplay ]
                    , Chip.content []
                        [ text light.displayName
                        ]
                    ]
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
