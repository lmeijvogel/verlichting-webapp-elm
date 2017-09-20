module VacationMode.Update exposing (load, update, Msg(..))

import Http
import Json.Encode
import Material
import JsonDecoders
import VacationMode.Model exposing (VacationModeModel, State(..))
import TimeOfDay exposing (..)


type Msg
    = Enable
    | Disable
    | Received (Result Http.Error JsonDecoders.VacationModeResult)
    | StartTimeChanged String
    | EndTimeChanged String
    | Mdl (Material.Msg Msg)



-- UPDATE


update : Msg -> VacationModeModel -> ( VacationModeModel, Cmd Msg )
update msg vacationMode =
    case msg of
        Enable ->
            let
                newVacationMode =
                    { vacationMode | state = Enabled vacationMode.nextStartTime vacationMode.nextEndTime }
            in
                ( vacationMode, sendNewVacationModeState newVacationMode vacationMode.state )

        Disable ->
            let
                newVacationMode =
                    { vacationMode | state = Disabled }
            in
                ( vacationMode, sendNewVacationModeState newVacationMode vacationMode.state )

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


sendNewVacationModeState : VacationModeModel -> VacationMode.Model.State -> Cmd Msg
sendNewVacationModeState vacationMode oldState =
    let
        stateJson =
            case vacationMode.state of
                Enabled _ _ ->
                    "on"

                Disabled ->
                    "off"

                Unknown ->
                    "off"

        disabledRequestData =
            [ ( "state", Json.Encode.string stateJson ) ]

        requestData =
            case vacationMode.state of
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
            Http.post url requestJson (JsonDecoders.vacationMode defaultStartTime defaultEndTime)
    in
        Http.send Received request


load : Cmd Msg
load =
    let
        url =
            "/my_zwave/vacation_mode"

        request =
            Http.get url (JsonDecoders.vacationMode (TimeOfDay 18 30) (TimeOfDay 22 30))
    in
        Http.send Received request
