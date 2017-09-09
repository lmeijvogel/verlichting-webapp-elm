module Main exposing (..)

import Http
import Html exposing (Html, ul, li, div, span, p, text, button, label, input)
import Html.Attributes exposing (placeholder, href, rel, type_, value)
import Html.Events exposing (onInput, onClick)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Chip as Chip
import Material.Color as Color
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Layout as Layout
import Material.List as MatList
import Material.Spinner as Spinner
import Material.Options as Options exposing (css)
import Material.Scheme as Scheme
import Material.Slider as Slider
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Typography as Typo
import Programmes.Model exposing (..)
import Programmes.Update
import Programmes.View
import Json.Decode exposing (Decoder)
import Light exposing (..)
import Login
import LiveState exposing (LiveState)
import MainSwitchState exposing (MainSwitchState)
import VacationMode exposing (VacationMode)
import TimeOfDay exposing (timeOfDayToString)
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
    , liveState : LiveState
    , programmesModel : ProgrammesModel
    , lights : List Light
    , editingLightId : Maybe Int
    , vacationMode : VacationMode
    , mainSwitchState : MainSwitchState
    , error : String
    , loginFormData : LoginFormData
    , mdl : Material.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { loginState = Login.new
      , liveState = LiveState.Unknown
      , programmesModel = newProgrammesModel
      , lights = []
      , editingLightId = Nothing
      , vacationMode = VacationMode.new
      , mainSwitchState = MainSwitchState.Unknown
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
    | ProgrammeMsg Programmes.Update.Msg
    | VacationModeMsg VacationMode.Msg
    | ShowLight (Maybe Light)
    | LightMsg Light.Msg
    | LiveStateClicked LiveState
    | LiveStateReceived (Result Http.Error LiveState)
    | MainSwitchStateClicked MainSwitchState
    | MainSwitchStateReceived (Result Http.Error MainSwitchState)
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProgrammeMsg msg ->
            let
                (newProgrammesModel, action) = Programmes.Update.update msg model.programmesModel
            in
                ( { model | programmesModel = newProgrammesModel }, Cmd.map ProgrammeMsg action)

        LightMsg msg ->
            case Light.update msg model.lights of
                ( Ok lights, cmd ) ->
                    ( { model | lights = lights }, Cmd.map LightMsg cmd )

                ( Err error, cmd ) ->
                    ( { model | error = toString error }, Cmd.map LightMsg cmd )

        LiveStateClicked liveState ->
            ( model, setLiveState liveState )

        LiveStateReceived (Ok liveState) ->
            ( { model | liveState = liveState }, Cmd.none )

        LiveStateReceived (Err _) ->
            ( { model | liveState = LiveState.Error }, Cmd.none )

        MainSwitchStateClicked mainSwitchState ->
            ( model, setMainSwitchState mainSwitchState )

        MainSwitchStateReceived (Ok mainSwitchState) ->
            ( { model | mainSwitchState = mainSwitchState }, Cmd.none )

        MainSwitchStateReceived (Err _) ->
            ( { model | mainSwitchState = MainSwitchState.Error }, Cmd.none )

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

                nextLoginFormData =
                    if loginStateChanged then
                        newLoginFormData
                        -- Clear login form data after logging in
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
        [ Cmd.map ProgrammeMsg Programmes.Update.load
        , getLiveState
        , getMainSwitchState
        , Cmd.map LightMsg Light.load
        , Cmd.map VacationModeMsg VacationMode.load
        ]


newLoginFormData : LoginFormData
newLoginFormData =
    (LoginFormData "" "")


type alias Url =
    String


get : Decoder a -> (Result Http.Error a -> Msg) -> Url -> Cmd Msg
get decoder msg url =
    let
        request =
            Http.get url decoder
    in
        Http.send msg request



getLiveState : Cmd Msg
getLiveState =
    get JsonDecoders.liveState LiveStateReceived "/my_zwave/live"


setLiveState : LiveState -> Cmd Msg
setLiveState newState =
    let
        stateString =
            case newState of
                LiveState.Simulation ->
                    "false"

                _ ->
                    "true"

        url =
            "/my_zwave/live/" ++ stateString

        request =
            Http.post url Http.emptyBody JsonDecoders.liveState
    in
        Http.send LiveStateReceived request


getMainSwitchState : Cmd Msg
getMainSwitchState =
    get JsonDecoders.mainSwitchState MainSwitchStateReceived "/my_zwave/main_switch"


setMainSwitchState : MainSwitchState -> Cmd Msg
setMainSwitchState newState =
    let
        stateString =
            case newState of
                MainSwitchState.Disabled ->
                    "false"

                _ ->
                    "true"

        url =
            "/my_zwave/main_switch/" ++ stateString

        request =
            Http.post url Http.emptyBody JsonDecoders.mainSwitchState
    in
        Http.send MainSwitchStateReceived request



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    let
        mainColor =
            case model.liveState of
                LiveState.Live ->
                    Color.Indigo

                LiveState.Simulation ->
                    Color.Teal

                _ ->
                    Color.BlueGrey

        scheduleIcon =
            if model.vacationMode.state then
                [ Icon.i "schedule" ]
            else
                []

        syncDisabledIcon =
            if model.liveState == LiveState.Live then
                []
            else
                [ Icon.i "sync_disabled" ]

        indeterminateCheckboxIcon =
            if model.mainSwitchState == MainSwitchState.Enabled then
                []
            else
                [ Icon.i "indeterminate_check_box" ]

        icons =
            scheduleIcon ++ syncDisabledIcon ++ indeterminateCheckboxIcon
    in
        (Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader
            ]
            { header =
                [ Layout.row [ Typo.title ]
                    [ div [ Html.Attributes.style [ ( "height", "100%" ), ( "display", "flex" ), ( "flex-direction", "row" ), ( "align-items", "center" ) ] ]
                        [ span
                            [ Html.Attributes.style [ ( "margin-right", "20px" ) ] ]
                            [ text "Verlichting" ]
                        , span [ Html.Attributes.style [ ( "display", "flex" ), ( "align-items", "center" ) ] ] icons
                        ]
                    ]
                ]
            , drawer = [ drawer model ]
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
                                    [ Html.map ProgrammeMsg (Programmes.View.view model.mdl model.programmesModel)
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
        )
            |> Scheme.topWithScheme mainColor Color.Red


drawer : Model -> Html Msg
drawer model =
    let
        newLiveState =
            if model.liveState == LiveState.Live then
                LiveState.Simulation
            else
                LiveState.Live

        newMainSwitchState =
            if model.mainSwitchState == MainSwitchState.Enabled then
                MainSwitchState.Disabled
            else
                MainSwitchState.Enabled
    in
        div []
            [ MatList.ul []
                [ MatList.li []
                    [ MatList.content
                        []
                        [ Toggles.checkbox Mdl
                            [ 0 ]
                            model.mdl
                            [ Toggles.value (model.liveState == LiveState.Live)
                            , Options.onToggle (LiveStateClicked newLiveState)
                            ]
                            [ text "Web Live" ]
                        ]
                    ]
                , MatList.li []
                    [ MatList.content
                        []
                        [ Toggles.checkbox Mdl
                            [ 1 ]
                            model.mdl
                            [ Toggles.value (model.mainSwitchState == MainSwitchState.Enabled)
                            , Options.onToggle (MainSwitchStateClicked newMainSwitchState)
                            ]
                            [ text "Main switch enabled" ]
                        ]
                    ]
                ]
            ]


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
