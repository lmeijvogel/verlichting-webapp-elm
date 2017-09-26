module VacationMode
    exposing
        ( Model
        , Msg
        , new
        , load
        , update
        , view
        , isEnabled
        )

import Http
import Html exposing (Html, input, p, text)
import Html.Attributes
import Html.Events exposing (onInput)
import Json.Encode
import Json.Decode exposing (..)
import Json.Decode as Decode
import Material
import Material.Button as Button
import Material.Card as Card
import Material.List as MatList
import Material.Options as Options
import Material.Typography as Typo


type State
    = Enabled TimeOfDay TimeOfDay
    | Disabled
    | Unknown


type alias Model =
    { state : State
    , nextStartTime : TimeOfDay
    , nextEndTime : TimeOfDay
    , error : String
    , mdl : Material.Model
    }


new : Model
new =
    { state = Unknown
    , nextStartTime = TimeOfDay 18 30
    , nextEndTime = TimeOfDay 22 30
    , error = ""
    , mdl = Material.model
    }


type alias TimeOfDay =
    { hour : Int
    , minute : Int
    }


timeOfDayToString : TimeOfDay -> String
timeOfDayToString time =
    let
        hour =
            toString time.hour |> String.pad 2 '0'

        minute =
            toString time.minute |> String.pad 2 '0'
    in
        hour ++ ":" ++ minute


timeOfDayFromString : String -> Result String TimeOfDay
timeOfDayFromString time =
    let
        timeParts =
            String.split ":" time

        hour : Result String Int
        hour =
            timeParts
                |> List.head
                |> Result.fromMaybe "No hour part present"
                |> Result.andThen String.toInt

        minute : Result String Int
        minute =
            timeParts
                |> List.tail
                |> Maybe.andThen List.head
                |> Result.fromMaybe "No minute part exists"
                |> Result.andThen String.toInt
    in
        Result.map2 TimeOfDay hour minute


isEnabled : Model -> Bool
isEnabled model =
    case model.state of
        Enabled _ _ ->
            True

        _ ->
            False



-- UPDATE --


type alias VacationModeResult =
    { state : String
    , start_time : TimeOfDay
    , end_time : TimeOfDay
    }


type Msg
    = Enable
    | Disable
    | Received (Result Http.Error VacationModeResult)
    | StartTimeChanged String
    | EndTimeChanged String
    | Mdl (Material.Msg Msg)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg vacationMode =
    case msg of
        Enable ->
            let
                newVacationMode =
                    { vacationMode | state = Enabled vacationMode.nextStartTime vacationMode.nextEndTime }
            in
                ( vacationMode, sendNewVacationModeState newVacationMode.state vacationMode.state )

        Disable ->
            let
                newVacationMode =
                    { vacationMode | state = Disabled }
            in
                ( vacationMode, sendNewVacationModeState newVacationMode.state vacationMode.state )

        StartTimeChanged startTimeString ->
            case timeOfDayFromString startTimeString of
                Ok time ->
                    ( { vacationMode | nextStartTime = time }, Cmd.none )

                Err string ->
                    ( { vacationMode | error = "Invalid start time: " ++ string }, Cmd.none )

        EndTimeChanged endTimeString ->
            case timeOfDayFromString endTimeString of
                Ok time ->
                    ( { vacationMode | nextEndTime = time }, Cmd.none )

                Err string ->
                    ( { vacationMode | error = "Invalid end time: " ++ string }, Cmd.none )

        Received (Ok vacationModeResult) ->
            let
                newState =
                    if vacationModeResult.state == "on" then
                        Enabled vacationModeResult.start_time vacationModeResult.end_time
                    else
                        Disabled
            in
                ( { vacationMode | state = newState, nextStartTime = vacationModeResult.start_time, nextEndTime = vacationModeResult.end_time }, Cmd.none )

        Received (Err error) ->
            ( { vacationMode | error = toString error }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ vacationMode


sendNewVacationModeState : State -> State -> Cmd Msg
sendNewVacationModeState state oldState =
    let
        stateJson =
            case state of
                Enabled _ _ ->
                    "on"

                Disabled ->
                    "off"

                Unknown ->
                    "off"

        disabledRequestData =
            [ ( "state", Json.Encode.string stateJson ) ]

        requestData =
            case state of
                Enabled startTime endTime ->
                    [ ( "start_time", Json.Encode.string <| timeOfDayToString startTime )
                    , ( "end_time", Json.Encode.string <| timeOfDayToString endTime )
                    , ( "state", Json.Encode.string stateJson )
                    ]

                Disabled ->
                    disabledRequestData

                Unknown ->
                    disabledRequestData

        requestJson =
            requestData
                |> Json.Encode.object
                |> Http.jsonBody

        url =
            "/my_zwave/vacation_mode"

        ( defaultStartTime, defaultEndTime ) =
            case oldState of
                Enabled start end ->
                    ( start, end )

                _ ->
                    ( TimeOfDay 18 30, TimeOfDay 22 30 )

        request =
            Http.post url requestJson (decodeVacationMode defaultStartTime defaultEndTime)
    in
        Http.send Received request


load : Cmd Msg
load =
    let
        url =
            "/my_zwave/vacation_mode"

        request =
            Http.get url (decodeVacationMode (TimeOfDay 18 30) (TimeOfDay 22 30))
    in
        Http.send Received request


decodeVacationMode : TimeOfDay -> TimeOfDay -> Decoder VacationModeResult
decodeVacationMode defaultStart defaultEnd =
    let
        convert : TimeOfDay -> Maybe String -> Decoder TimeOfDay
        convert default input =
            let
                defaultTimeString =
                    timeOfDayToString default

                timeOfDayResult =
                    timeOfDayFromString (Maybe.withDefault defaultTimeString input)
            in
                case timeOfDayResult of
                    Ok timeOfDay ->
                        succeed timeOfDay

                    _ ->
                        fail ("Invalid input value" ++ (toString input) ++ "'")
    in
        map3 VacationModeResult
            (field "state" string)
            ((maybe (field "start_time" string)) |> andThen (convert defaultStart))
            ((maybe (field "end_time" string)) |> andThen (convert defaultEnd))



-- VIEW --


type alias Mdl =
    Material.Model


view : Material.Model -> Model -> Html Msg
view mdl vacationModeModel =
    let
        on =
            case vacationModeModel.state of
                Enabled _ _ ->
                    True

                _ ->
                    False

        buttonText =
            if on then
                "Disable"
            else
                "Enable"

        buttonAction =
            if on then
                Disable
            else
                Enable
    in
        Card.view []
            [ Card.title []
                [ let
                    titleText =
                        case vacationModeModel.state of
                            Enabled _ _ ->
                                "Vacation mode is ON"

                            Disabled ->
                                "Vacation mode is OFF"

                            Unknown ->
                                "Vacation mode (state unknown)"
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
                                text (timeOfDayToString vacationModeModel.nextStartTime)
                              else
                                input
                                    [ Html.Attributes.type_ "time"
                                    , Html.Attributes.value (timeOfDayToString vacationModeModel.nextStartTime)
                                    , onInput (\s -> StartTimeChanged s)
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
                                text (timeOfDayToString vacationModeModel.nextEndTime)
                              else
                                input
                                    [ Html.Attributes.type_ "time"
                                    , Html.Attributes.value (timeOfDayToString vacationModeModel.nextEndTime)
                                    , onInput (\s -> EndTimeChanged s)
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
                    mdl
                    [ Button.ripple, Button.accent, Options.onClick buttonAction ]
                    [ text buttonText ]
                ]
            ]
