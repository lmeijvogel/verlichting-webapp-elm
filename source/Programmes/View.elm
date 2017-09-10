module Programmes.View exposing (view)

import Html exposing (Html, ul, li, div, span, p, text, button, label, input)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.List as MatList
import Material.Typography as Typo
import Material.Options as Options exposing (css)
import Programmes.Model exposing (ProgrammesModel, Programme)
import Programmes.Update exposing (Msg(Mdl))


type alias Mdl =
    Material.Model


view : Material.Model -> ProgrammesModel -> Html Msg
view mdl programmesModel =
    Card.view []
        [ Card.title []
            [ Options.styled p [ Typo.title ] [ text "Programma's" ]
            ]
        , Card.text []
            [ MatList.ul []
                (List.map (\programme -> programmeEntry programme mdl programmesModel.currentProgramme programmesModel.pendingProgramme) programmesModel.availableProgrammes)
            ]
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
                        [ [ Options.onClick (Programmes.Update.ProgrammeClicked programme)
                          ]
                        , buttonStyles
                        ]
                    )
                    [ text programme.name ]
                ]
            ]


compactListItem : List (Options.Property c m) -> List (Html m) -> Html m
compactListItem listStyles =
    MatList.li
        [ Options.css "padding-top" "0"
        , Options.css "padding-bottom" "0"
        ]
