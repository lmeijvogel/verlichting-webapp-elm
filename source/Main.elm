module Main exposing (..)

import Http
import Html exposing (Html, ul, li, div, span, p, text, button, label, input)
import Html.Attributes exposing (placeholder, href, rel, type_, value)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Layout as Layout
import Material.List as MatList
import Material.Spinner as Spinner
import Material.Options as Options exposing (css)
import Material.Scheme as Scheme
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Typography as Typo
import Programmes.Model exposing (..)
import Programmes.Update
import Programmes.View
import Json.Decode exposing (Decoder)
import Lights.Model exposing (..)
import Lights.Update exposing (..)
import Lights.View exposing (..)
import Login
import LiveState exposing (LiveState)
import MainSwitchState exposing (MainSwitchState)
import VacationMode.Model exposing (..)
import VacationMode.Update
import VacationMode.View
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
    , lightsModel : LightsModel
    , editingLightId : Maybe Int
    , vacationModeModel : VacationModeModel
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
      , lightsModel = newLightsModel
      , editingLightId = Nothing
      , vacationModeModel = VacationMode.Model.new
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
    | VacationModeMsg VacationMode.Update.Msg
    | LightMsg Lights.Update.Msg
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
                ( newProgrammesModel, action ) =
                    Programmes.Update.update msg model.programmesModel
            in
                ( { model | programmesModel = newProgrammesModel }, Cmd.map ProgrammeMsg action )

        LightMsg msg ->
            let
                ( newLightsModel, action ) =
                    Lights.Update.update msg model.lightsModel
            in
                ( { model | lightsModel = newLightsModel }, Cmd.map LightMsg action )

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
                ( newVacationModeModel, action ) =
                    VacationMode.Update.update msg model.vacationModeModel
            in
                ( { model | vacationModeModel = newVacationModeModel }, Cmd.map VacationModeMsg action )

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model


initialize : Cmd Msg
initialize =
    Cmd.batch
        [ Cmd.map ProgrammeMsg Programmes.Update.load
        , getLiveState
        , getMainSwitchState
        , Cmd.map LightMsg Lights.Update.load
        , Cmd.map VacationModeMsg VacationMode.Update.load
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
            case model.vacationModeModel.state of
                VacationMode.Model.Enabled _ _ ->
                    [ Icon.i "schedule" ]

                _ ->
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
                                    [ Html.map VacationModeMsg (VacationMode.View.view model.mdl model.vacationModeModel)
                                    ]
                                , Grid.cell [ Grid.size Grid.Phone 4, Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 4 ]
                                    [ Html.map LightMsg (Lights.View.view model.mdl model.lightsModel)
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
