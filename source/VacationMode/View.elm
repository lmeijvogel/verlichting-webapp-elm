module VacationMode.View exposing (view)

import Html exposing (Html, ul, li, div, span, p, text, button, label, input)
import Html.Attributes exposing (placeholder, href, rel, type_, value)
import Html.Events exposing (onInput, onClick)

import Material
import Material.Button as Button
import Material.Card as Card
import Material.List as MatList
import Material.Options as Options exposing (css)
import Material.Typography as Typo

import TimeOfDay exposing (..)
import VacationMode.Model exposing (..)
import VacationMode.Update exposing (Msg)

type alias Mdl =
    Material.Model

view : Material.Model -> VacationModeModel -> Html Msg
view mdl vacationModeModel =
    let
        on =
            vacationModeModel.state

        buttonText =
            if on then
                "Disable"
            else
                "Enable"

        buttonAction =
            if on then
                VacationMode.Update.Disable
            else
                VacationMode.Update.Enable
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
                                text (timeOfDayToString vacationModeModel.averageStartTime)
                              else
                                input
                                    [ type_ "time"
                                    , value (timeOfDayToString vacationModeModel.averageStartTime)
                                    , onInput (\s -> VacationMode.Update.StartTimeChanged s)
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
                                text (timeOfDayToString vacationModeModel.averageEndTime)
                              else
                                input
                                    [ type_ "time"
                                    , value (timeOfDayToString vacationModeModel.averageEndTime)
                                    , onInput (\s -> VacationMode.Update.EndTimeChanged s)
                                    ]
                                    []
                            ]
                        ]
                    ]
                ]
            , Card.actions
                [ Card.border ]
                [ Button.render VacationMode.Update.Mdl
                    [ 1, 0 ]
                    mdl
                    [ Button.ripple, Button.accent, Options.onClick buttonAction ]
                    [ text buttonText ]
                ]
            ]


