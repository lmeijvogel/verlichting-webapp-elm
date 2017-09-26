module Main exposing (..)

import Http
import Html exposing (Html, div, span, text)
import Html.Attributes
import Material
import Material.Button as Button
import Material.Color as Color
import Material.Grid as Grid
import Material.Helpers exposing (map1st, map2nd)
import Material.Icon as Icon
import Material.Layout as Layout
import Material.List as MatList
import Material.Spinner as Spinner
import Material.Options as Options
import Material.Scheme as Scheme
import Material.Snackbar as Snackbar
import Material.Toggles as Toggles
import Material.Typography as Typo
import Json.Decode exposing (Decoder)
import Lights
import Login
import LiveState
import MainSwitchState
import Programmes
import VacationMode
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


type alias Model =
    { loginModel : Login.Model
    , liveState : LiveState.State
    , programmesModel : Programmes.Model
    , lightsModel : Lights.LightsModel
    , vacationModeModel : VacationMode.Model
    , mainSwitchState : MainSwitchState.Model
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
      , mainSwitchState = MainSwitchState.new
      , snackbar = Snackbar.model
      , mdl = Material.model
      }
    , Cmd.map LoginMsg Login.checkLoggedIn
    )



-- UPDATE


type Msg
    = LoginMsg Login.Msg
    | ProgrammeMsg Programmes.Msg
    | VacationModeMsg VacationMode.Msg
    | LightMsg Lights.Msg
    | LiveStateClicked LiveState.State
    | LiveStateReceived (Result Http.Error LiveState.State)
    | MainSwitchStateMsg MainSwitchState.Msg
    | HealNetwork
    | HealNetworkRequestSent (Result Http.Error String)
    | Snackbar (Snackbar.Msg Msg)
    | SnackbarClicked
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

        LiveStateClicked liveState ->
            ( model, setLiveState liveState )

        LiveStateReceived (Ok liveState) ->
            ( { model | liveState = liveState }, Cmd.none )

        LiveStateReceived (Err _) ->
            ( { model | liveState = LiveState.Error }, Cmd.none )

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

        Mdl msg_ ->
            Material.update Mdl msg_ model


initialize : Cmd Msg
initialize =
    Cmd.batch
        [ Cmd.map ProgrammeMsg Programmes.load
        , getLiveState
        , Cmd.map MainSwitchStateMsg MainSwitchState.load
        , Cmd.map LightMsg Lights.load
        , Cmd.map VacationModeMsg VacationMode.load
        ]


get : Decoder a -> (Result Http.Error a -> Msg) -> String -> Cmd Msg
get decoder msg url =
    let
        request =
            Http.get url decoder
    in
        Http.send msg request


getLiveState : Cmd Msg
getLiveState =
    get JsonDecoders.liveState LiveStateReceived "/my_zwave/live"


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
            "/my_zwave/heal_network"

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
                    "Error: " ++ (toString msg)

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
            if MainSwitchState.enabled model.mainSwitchState then
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
                            ]
                        ]
                  else
                    Html.map LoginMsg (Login.view model.loginModel)
                , Snackbar.view model.snackbar |> Html.map Snackbar
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

        newMainSwitchAction =
            if MainSwitchState.enabled model.mainSwitchState then
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
                            [ Toggles.value (MainSwitchState.enabled model.mainSwitchState)
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
