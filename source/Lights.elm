module Lights exposing (LightsModel, Msg, new, load, update, view)

import Http
import Html exposing (Html, ul, li, div, span, p, text, button, label, input)
import Json.Decode exposing (..)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Chip as Chip
import Material.Color as Color
import Material.List as MatList
import Material.Options as Options exposing (css)
import Material.Slider as Slider
import Material.Toggles as Toggles
import Material.Typography as Typo


-- MODEL --


type alias Light =
    { id : Int
    , name : String
    , displayName : String
    , value : LightValue
    }


type alias LightsModel =
    { lights : List Light
    , editingLightId : Maybe Int
    , error : Maybe String
    , mdl : Material.Model
    }


new : LightsModel
new =
    { lights = []
    , editingLightId = Nothing
    , error = Nothing
    , mdl = Material.model
    }


type LightValue
    = Level Int
    | State Bool



-- UPDATE --


type Msg
    = Received (Result Http.Error (List Light))
    | ShowLight (Maybe Light)
    | Update Light LightValue Bool
    | Save Light
    | ChangeAcknowledged (Result Http.Error LightValue)
    | Mdl (Material.Msg Msg)


update : Msg -> LightsModel -> ( LightsModel, Cmd Msg )
update msg lightsModel =
    case msg of
        Received (Ok newLights) ->
            ( { lightsModel | lights = newLights }, Cmd.none )

        Received (Err error) ->
            ( { lightsModel | error = Just "Error receiving lights" }, Cmd.none )

        ShowLight light ->
            ( { lightsModel | editingLightId = (Maybe.map (\l -> l.id) light) }, Cmd.none )

        {- The `saveAfter` here is a workaround for a usability "quirk":
           - When a Slider is used, we don't want to bombard the server with
           - requests for each small chang, so we do a separate 'save' when
           - the mouse button is released.
           - However, when a Toggle is used, we want to save the new value immediately,
           - since waiting for the mouse button release is probably not reliable.
        -}
        Update light value saveAfter ->
            let
                newLight =
                    { light | value = value }

                nextCommand =
                    if saveAfter then
                        (save newLight)
                    else
                        Cmd.none

                newLights =
                    (List.map
                        (\l ->
                            if l.id == light.id then
                                newLight
                            else
                                l
                        )
                        lightsModel.lights
                    )
            in
                ( { lightsModel | lights = newLights }, nextCommand )

        Save light ->
            ( lightsModel, save light )

        ChangeAcknowledged (Ok _) ->
            ( lightsModel, Cmd.none )

        ChangeAcknowledged (Err error) ->
            ( { lightsModel | error = Just "Error saving change" }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ lightsModel


decodeLights : Decoder (List Light)
decodeLights =
    let
        toLight : Decoder Light
        toLight =
            map4 Light
                (field "node_id" int)
                (field "name" string)
                (field "display_name" string)
                (oneOf
                    [ map Level (field "value" int)
                    , map State (field "state" bool)
                    ]
                )
    in
        field "lights" (list toLight)


decodeChangeResponse : Decoder LightValue
decodeChangeResponse =
    oneOf
        [ map Level (field "level" int)
        , map State (field "state" bool)
        ]


load : Cmd Msg
load =
    let
        url =
            "/my_zwave/current_lights"

        request =
            Http.get url decodeLights
    in
        Http.send Received request


save : Light -> Cmd Msg
save light =
    let
        url =
            case light.value of
                State state ->
                    let
                        stateString =
                            if state then
                                "on"
                            else
                                "off"
                    in
                        "/my_zwave/light/" ++ (toString light.id) ++ "/switch/" ++ stateString

                Level intensity ->
                    "/my_zwave/light/" ++ (toString light.id) ++ "/level/" ++ (toString intensity)

        request =
            Http.post url Http.emptyBody decodeChangeResponse
    in
        Http.send ChangeAcknowledged request


newIntensityRequested : Light -> Float -> Msg
newIntensityRequested light level =
    Update light (Level (round level)) False



-- VIEW --


view : Material.Model -> LightsModel -> Html Msg
view mdl model =
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


singleLightCard : Light -> LightsModel -> Html Msg
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
                        [ Options.onToggle (Update light (State (not currentState)) True)
                        , Toggles.ripple
                        , Toggles.value currentState
                        ]
                        [ text "Switch" ]

                Level currentLevel ->
                    div []
                        [ Slider.view
                            [ Slider.onChange (newIntensityRequested light)
                            , Options.onMouseUp (Save light)
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


compactListItem : List (Options.Property c m) -> List (Html m) -> Html m
compactListItem listStyles =
    MatList.li
        [ Options.css "padding-top" "0"
        , Options.css "padding-bottom" "0"
        ]
