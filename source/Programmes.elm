module Programmes exposing (Model, Msg, new, load, update, view)

import Html exposing (Html, p, text)
import Http
import Json.Decode as Decode
import Json.Decode exposing (..)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Elevation as Elevation
import Material.Color as Color
import Material.Icon as Icon
import Material.List as MatList
import Material.Typography as Typo
import Material.Options as Options


-- MODEL --


type alias Model =
    { availableProgrammes : List Programme
    , currentProgramme : Maybe String
    , pendingProgramme : Maybe String
    , error : Maybe String
    , mdl : Material.Model
    }


new : Model
new =
    { availableProgrammes = []
    , currentProgramme = Nothing
    , pendingProgramme = Nothing
    , error = Nothing
    , mdl = Material.model
    }


type alias Programme =
    { id : String
    , name : String
    }


type PostProgrammeResult
    = Success String
    | Error



-- UPDATE --


type Msg
    = Load
    | ProgrammeClicked Programme
    | ProgrammesReceived (Result Http.Error (List Programme))
    | ActivationResponseReceived (Result Http.Error PostProgrammeResult)
    | CurrentProgrammeReceived (Result Http.Error String)
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg programmesModel =
    case msg of
        Load ->
            ( programmesModel, load )

        ProgrammesReceived (Ok availableProgrammes) ->
            ( { programmesModel | availableProgrammes = availableProgrammes }, getCurrentProgramme )

        ProgrammesReceived (Err error) ->
            ( { programmesModel | error = Just ("Could not retrieve programmes list: " ++ (toString error)) }, Cmd.none )

        CurrentProgrammeReceived (Ok id) ->
            ( { programmesModel | currentProgramme = Just id }, Cmd.none )

        CurrentProgrammeReceived (Err _) ->
            ( { programmesModel | error = Just "Could not retrieve current programme" }, Cmd.none )

        ProgrammeClicked programme ->
            ( { programmesModel | pendingProgramme = Just programme.id }, activateProgramme programme.id )

        ActivationResponseReceived (Ok result) ->
            case result of
                Success programme ->
                    ( { programmesModel
                        | currentProgramme = Just programme
                        , pendingProgramme = Nothing
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Error ->
                    ( { programmesModel | error = Just "Result was not success" }, Cmd.none )

        ActivationResponseReceived (Err _) ->
            ( { programmesModel | error = Just "An error occurred" }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ programmesModel


availableProgrammes : Decoder (List Programme)
availableProgrammes =
    let
        tupleToProgramme : ( String, String ) -> Programme
        tupleToProgramme ( id, name ) =
            Programme id name

        convert : List ( String, String ) -> Decoder (List Programme)
        convert tupleList =
            Decode.succeed (List.map tupleToProgramme tupleList)
    in
        field "availableProgrammes" (keyValuePairs (string))
            |> Decode.andThen convert
            |> Decode.map List.reverse


currentProgramme : Decoder String
currentProgramme =
    field "programme" string


activateProgramme : String -> Cmd Msg
activateProgramme programmeId =
    let
        url =
            "/my_zwave/programme/" ++ programmeId ++ "/start"

        request =
            Http.post url Http.emptyBody activationResponse
    in
        Http.send ActivationResponseReceived request


activationResponse : Decoder PostProgrammeResult
activationResponse =
    let
        decodeResult : Bool -> Decoder PostProgrammeResult
        decodeResult success =
            if success then
                map Success (field "programme" string)
            else
                Decode.succeed Error
    in
        field "success" bool |> Decode.andThen decodeResult


load : Cmd Msg
load =
    get availableProgrammes ProgrammesReceived "/my_zwave/available_programmes"


getCurrentProgramme : Cmd Msg
getCurrentProgramme =
    get currentProgramme CurrentProgrammeReceived "/my_zwave/current_programme"


get : Decoder a -> (Result Http.Error a -> Msg) -> String -> Cmd Msg
get decoder msg url =
    let
        request =
            Http.get url decoder
    in
        Http.send msg request


type alias Mdl =
    Material.Model



-- VIEW --


view : Material.Model -> Model -> Html Msg
view mdl programmesModel =
    Card.view [ Elevation.e2 ]
        [ Card.title []
            [ Options.styled p [ Typo.title ] [ text "Programma's" ]
            ]
        , Card.text []
            [ MatList.ul []
                (List.map (\programme -> programmeEntry programme mdl programmesModel.currentProgramme programmesModel.pendingProgramme) programmesModel.availableProgrammes)
            ]
        , Card.actions [ Card.border, Options.css "vertical-align" "center", Options.css "text-align" "right", (Color.text Color.black) ]
            [ Button.render Mdl
                [ 8, 1 ]
                programmesModel.mdl
                [ Button.icon, Button.ripple, Options.onClick Load ]
                [ Icon.i "refresh" ]
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
                        [ [ Options.onClick (ProgrammeClicked programme)
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
