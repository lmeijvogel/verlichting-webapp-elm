module VacationMode
    exposing
        ( VacationMode
        , Msg(Enable, Disable, StartTimeChanged, EndTimeChanged)
        , new
        , update
        , load
        )

import TimeOfDay exposing (TimeOfDay, timeOfDayFromString, timeOfDayToString)
import JsonDecoders
import Http
import Json.Encode


type alias VacationMode =
    { state : Bool
    , averageStartTime : TimeOfDay
    , averageEndTime : TimeOfDay
    , error : String
    }


new : VacationMode
new =
    { state = False
    , averageStartTime = TimeOfDay 18 30
    , averageEndTime = TimeOfDay 22 30
    , error = ""
    }


load : Cmd Msg
load =
    let
        url =
            "/my_zwave/vacation_mode"

        request =
            Http.get url JsonDecoders.vacationMode
    in
        Http.send Received request


type Msg
    = Enable
    | Disable
    | Received (Result Http.Error JsonDecoders.VacationModeResult)
    | StartTimeChanged String
    | EndTimeChanged String



-- UPDATE


update : Msg -> VacationMode -> ( VacationMode, Cmd Msg )
update msg vacationMode =
    case msg of
        Enable ->
            let
                newVacationMode =
                    { vacationMode | state = True }
            in
                ( vacationMode, sendNewVacationModeState newVacationMode )

        Disable ->
            let
                newVacationMode =
                    { vacationMode | state = False }
            in
                ( vacationMode, sendNewVacationModeState newVacationMode )

        StartTimeChanged startTimeString ->
            case timeOfDayFromString startTimeString of
                Ok time ->
                    ( { vacationMode | averageStartTime = time }, Cmd.none )

                Err string ->
                    ( { vacationMode | error = "Invalid start time: " ++ string }, Cmd.none )

        EndTimeChanged endTimeString ->
            case timeOfDayFromString endTimeString of
                Ok time ->
                    ( { vacationMode | averageEndTime = time }, Cmd.none )

                Err string ->
                    ( { vacationMode | error = "Invalid end time: " ++ string }, Cmd.none )

        Received (Ok vacationModeResult) ->
            ( { vacationMode | state = vacationModeResult.state == "on", averageStartTime = vacationModeResult.start_time, averageEndTime = vacationModeResult.end_time }, Cmd.none )

        Received (Err error) ->
            ( { vacationMode | error = toString error }, Cmd.none )


sendNewVacationModeState : VacationMode -> Cmd Msg
sendNewVacationModeState vacationMode =
    let
        stateJson =
            if vacationMode.state then
                "on"
            else
                "off"

        requestData =
            [ ( "start_time", Json.Encode.string <| timeOfDayToString vacationMode.averageStartTime )
            , ( "end_time", Json.Encode.string <| timeOfDayToString vacationMode.averageEndTime )
            , ( "state", Json.Encode.string stateJson )
            ]
                |> Json.Encode.object
                |> Http.jsonBody

        url =
            "/my_zwave/vacation_mode"

        request =
            Http.post url requestData JsonDecoders.vacationMode
    in
        Http.send Received request
