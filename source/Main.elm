module Main exposing (..)

import Date
import Html exposing (Html, div, span, text)
import Html.Attributes
import Http exposing (header)
import Json.Decode exposing (Decoder)
import JsonDecoders
import Lights
import LiveState
import Login
import MainSwitchState
import Material
import Material.Button as Button
import Material.Color as Color
import Material.Grid as Grid
import Material.Helpers exposing (map1st, map2nd)
import Material.Icon as Icon
import Material.Layout as Layout
import Material.List as MatList
import Material.Options as Options
import Material.Scheme as Scheme
import Material.Snackbar as Snackbar
import Material.Spinner as Spinner
import Material.Toggles as Toggles
import Material.Typography as Typo
import Programmes
import RecentEvents
import Task
import VacationMode


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { loginModel : Login.Model
    , liveState : LiveState.State
    , programmesModel : Programmes.Model
    , lightsModel : Lights.LightsModel
    , vacationModeModel : VacationMode.Model
    , recentEventsModel : RecentEvents.Model
    , mainSwitchState : MainSwitchState.Model
    , csrfToken : Maybe String
    , mdl : Material.Model
    , snackbar : Snackbar.Model Msg
    }


init : ( Model, Cmd Msg )
init =
    ( { loginModel = Login.new
      , liveState = LiveState.Unknown
      , programmesModel = Programmes.new
      , lightsModel = Lights.new
      , vacationModeModel = VacationMode.new
      , recentEventsModel = RecentEvents.new
      , mainSwitchState = MainSwitchState.new
      , csrfToken = Nothing
      , snackbar = Snackbar.model
      , mdl = Material.model
      }
    , Cmd.map LoginMsg Login.checkLoggedIn
    )


setCsrfToken : Model -> String -> Model
setCsrfToken model token =
    let
        newHeaders =
            [ header "X-Csrf-Token" token ]

        oldProgrammesModel =
            model.programmesModel

        newProgrammesModel =
            { oldProgrammesModel | httpRequestHeaders = newHeaders }

        oldLightsModel =
            model.lightsModel

        newLightsModel =
            { oldLightsModel | httpRequestHeaders = newHeaders }
    in
    { model
        | csrfToken = Just token
        , programmesModel = newProgrammesModel
        , lightsModel = newLightsModel
    }



-- UPDATE


type Msg
    = LoginMsg Login.Msg
    | ProgrammeMsg Programmes.Msg
    | VacationModeMsg VacationMode.Msg
    | LightMsg Lights.Msg
    | RecentEventsMsg RecentEvents.Msg
    | LiveStateClicked LiveState.State
    | LiveStateReceived (Result Http.Error LiveState.State)
    | CsrfTokenReceived (Result Http.Error String)
    | MainSwitchStateMsg MainSwitchState.Msg
    | HealNetwork
    | HealNetworkRequestSent (Result Http.Error String)
    | Snackbar (Snackbar.Msg Msg)
    | SnackbarClicked
    | InitDate
    | DateReceived Date.Date
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProgrammeMsg msg ->
            let
                ( newProgrammesModel, action ) =
                    Programmes.update msg model.programmesModel
            in
            ( { model | programmesModel = newProgrammesModel }, Cmd.map ProgrammeMsg action )

        LightMsg msg ->
            let
                ( newLightsModel, action ) =
                    Lights.update msg model.lightsModel
            in
            ( { model | lightsModel = newLightsModel }, Cmd.map LightMsg action )

        RecentEventsMsg msg ->
            let
                ( newRecentEventsModel, action ) =
                    RecentEvents.update msg model.recentEventsModel
            in
            ( { model | recentEventsModel = newRecentEventsModel }, Cmd.map RecentEventsMsg action )

        LiveStateClicked liveState ->
            ( model, setLiveState liveState )

        LiveStateReceived (Ok liveState) ->
            ( { model | liveState = liveState }, Cmd.none )

        LiveStateReceived (Err _) ->
            ( { model | liveState = LiveState.Error }, Cmd.none )

        CsrfTokenReceived (Ok csrfToken) ->
            ( setCsrfToken model csrfToken, Cmd.none )

        -- At least show an error
        CsrfTokenReceived (Err _) ->
            ( model, Cmd.none )

        LoginMsg msg ->
            let
                ( newLoginModel, cmd, loginSuccessful ) =
                    Login.update msg model.loginModel

                nextCommand =
                    if loginSuccessful then
                        initialize

                    else
                        Cmd.map LoginMsg cmd
            in
            ( { model | loginModel = newLoginModel }, nextCommand )

        MainSwitchStateMsg msg ->
            let
                ( newMainSwitchState, action ) =
                    MainSwitchState.update model.mainSwitchState msg
            in
            ( { model | mainSwitchState = newMainSwitchState }, Cmd.map MainSwitchStateMsg action )

        VacationModeMsg msg ->
            let
                ( newVacationModeModel, action ) =
                    VacationMode.update msg model.vacationModeModel
            in
            ( { model | vacationModeModel = newVacationModeModel }, Cmd.map VacationModeMsg action )

        HealNetwork ->
            ( model, healNetwork )

        HealNetworkRequestSent result ->
            showNetworkRequestSent model result

        -- Boilerplate: Mdl action handler.
        Snackbar msg_ ->
            Snackbar.update msg_ model.snackbar
                |> map1st (\s -> { model | snackbar = s })
                |> map2nd (Cmd.map Snackbar)

        SnackbarClicked ->
            ( model, Cmd.none )

        InitDate ->
            ( model, Task.perform DateReceived Date.now )

        DateReceived date ->
            let
                newRecentEventsModel =
                    RecentEvents.updateCurrentDate date model.recentEventsModel
            in
            ( { model | recentEventsModel = newRecentEventsModel }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


initialize : Cmd Msg
initialize =
    Cmd.batch
        [ Cmd.map ProgrammeMsg Programmes.load
        , getCsrfToken
        , getLiveState
        , getCurrentDate
        , Cmd.map MainSwitchStateMsg MainSwitchState.load
        , Cmd.map LightMsg Lights.load
        , Cmd.map VacationModeMsg VacationMode.load
        , Cmd.map RecentEventsMsg RecentEvents.load
        ]


get : Decoder a -> (Result Http.Error a -> Msg) -> String -> Cmd Msg
get decoder msg url =
    let
        request =
            Http.get url decoder
    in
    Http.send msg request


getCsrfToken : Cmd Msg
getCsrfToken =
    get JsonDecoders.csrfToken CsrfTokenReceived "/my_zwave_new/csrf_token"


getLiveState : Cmd Msg
getLiveState =
    get JsonDecoders.liveState LiveStateReceived "/my_zwave/live"


getCurrentDate : Cmd Msg
getCurrentDate =
    Task.perform DateReceived Date.now


setLiveState : LiveState.State -> Cmd Msg
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


healNetwork : Cmd Msg
healNetwork =
    let
        url =
            "/my_zwave/debug/heal_network"

        request =
            Http.post url Http.emptyBody JsonDecoders.healNetwork
    in
    Http.send HealNetworkRequestSent request


showNetworkRequestSent : Model -> Result Http.Error String -> ( Model, Cmd Msg )
showNetworkRequestSent model result =
    let
        message : String
        message =
            case result of
                Ok msg ->
                    msg

                Err msg ->
                    "Error: " ++ toString msg

        snackContents : Snackbar.Contents Msg
        snackContents =
            { message = message
            , action = Nothing
            , payload = SnackbarClicked
            , timeout = 4000
            , fade = 1000
            }

        ( snackbar_, effect ) =
            Snackbar.add snackContents model.snackbar
                |> map2nd (Cmd.map Snackbar)

        model_ =
            { model | snackbar = snackbar_ }
    in
    ( model_, Cmd.batch [ effect ] )



-- VIEW


view : Model -> Html Msg
view model =
    let
        mainColor =
            case model.liveState of
                LiveState.Live ->
                    Color.Indigo

                LiveState.Simulation ->
                    Color.Teal

                LiveState.Error ->
                    Color.Red

                LiveState.Unknown ->
                    Color.BlueGrey

        scheduleIcon =
            if VacationMode.isEnabled model.vacationModeModel then
                [ Icon.i "schedule" ]

            else
                []

        syncDisabledIcon =
            if model.liveState == LiveState.Live then
                []

            else
                [ Icon.i "sync_disabled" ]

        indeterminateCheckboxIcon =
            if MainSwitchState.isEnabled model.mainSwitchState then
                []

            else
                [ Icon.i "indeterminate_check_box" ]

        csrfErrorIndicator =
            if model.csrfToken == Nothing then
                [ span [] [ text "!CSRF!" ] ]

            else
                []

        icons =
            scheduleIcon ++ syncDisabledIcon ++ indeterminateCheckboxIcon ++ csrfErrorIndicator
    in
    Layout.render Mdl
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
            [ if Login.isLoginPending model.loginModel then
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

              else if Login.isLoggedIn model.loginModel then
                div []
                    [ Grid.grid []
                        [ Grid.cell [ Grid.size Grid.Phone 4, Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 4 ]
                            [ Html.map ProgrammeMsg (Programmes.view model.mdl model.programmesModel)
                            ]
                        , Grid.cell [ Grid.size Grid.Phone 4, Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 4 ]
                            [ Html.map VacationModeMsg (VacationMode.view model.mdl model.vacationModeModel)
                            ]
                        , Grid.cell [ Grid.size Grid.Phone 4, Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 4 ]
                            [ Html.map LightMsg (Lights.view model.mdl model.lightsModel)
                            ]
                        , Grid.cell [ Grid.size Grid.Phone 4, Grid.size Grid.Tablet 8, Grid.size Grid.Desktop 4 ]
                            [ Html.map RecentEventsMsg (RecentEvents.view model.mdl model.recentEventsModel)
                            ]
                        ]
                    ]

              else
                Html.map LoginMsg (Login.view model.loginModel)
            , Snackbar.view model.snackbar |> Html.map Snackbar
            ]
        }
        |> Scheme.topWithScheme mainColor Color.Red


drawer : Model -> Html Msg
drawer model =
    let
        newLiveState =
            if model.liveState == LiveState.Live then
                LiveState.Simulation

            else
                LiveState.Live

        newMainSwitchAction =
            if MainSwitchState.isEnabled model.mainSwitchState then
                MainSwitchState.Disable

            else
                MainSwitchState.Enable
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
                        [ Toggles.value (MainSwitchState.isEnabled model.mainSwitchState)
                        , Options.onToggle (MainSwitchStateMsg newMainSwitchAction)
                        ]
                        [ text "Main switch enabled" ]
                    ]
                ]
            , MatList.li []
                [ MatList.content
                    []
                    [ Button.render Mdl
                        [ 2 ]
                        model.mdl
                        [ Button.raised
                        , Options.onClick HealNetwork
                        ]
                        [ text "Heal network" ]
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
