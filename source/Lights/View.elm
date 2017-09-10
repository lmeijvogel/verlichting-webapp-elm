module Lights.View exposing (..)

import Html exposing (Html, ul, li, div, span, p, text, button, label, input)
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
import Lights.Model exposing (..)
import Lights.Update exposing (..)


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
