module VacationMode exposing (VacationMode,
    Msg(Enable, Disable, StartTimeChanged, EndTimeChanged),
    new,
    update,
    load,
    timeOfDayToString
  )

import JsonDecoders

import Http
import Json.Encode

type alias VacationMode =
  {
    state: Bool,
    averageStartTime: TimeOfDay,
    averageEndTime: TimeOfDay,
    error: String
  }

new : VacationMode
new =
  {
    state = False,
    averageStartTime = TimeOfDay 18 30,
    averageEndTime = TimeOfDay 22 30,
    error = ""
  }

load : Cmd Msg
load =
  let
      url = "/my_zwave/vacation_mode"

      request =
        Http.get url JsonDecoders.vacationMode
  in
      Http.send Received request



type Msg =
  Enable
  | Disable
  | Received (Result Http.Error (JsonDecoders.VacationModeResult) )
  | StartTimeChanged String
  | EndTimeChanged String

type alias TimeOfDay =
  {
    hour: Int,
    minute: Int
  }

-- UPDATE

update : Msg -> VacationMode -> (VacationMode, Cmd Msg)
update msg vacationMode =
  case msg of
    Enable ->
      let
          newVacationMode = { vacationMode | state = True }
      in
      ( vacationMode, sendNewVacationModeState newVacationMode )
    Disable ->
      let
          newVacationMode = { vacationMode | state = False }
      in
      ( vacationMode, sendNewVacationModeState newVacationMode )

    StartTimeChanged startTimeString ->
      case timeOfDayFromString startTimeString of
        Ok time ->
          ( { vacationMode | averageStartTime = time }, Cmd.none )
        Err string ->
          ( { vacationMode | error = "Invalid start time: " ++ string }, Cmd.none )

    EndTimeChanged endTimeString->
      case timeOfDayFromString endTimeString of
        Ok time ->
          ( { vacationMode | averageEndTime = time }, Cmd.none )
        Err string ->
          ( { vacationMode | error = "Invalid end time: " ++ string }, Cmd.none )


    Received (Ok vacationModeResult) ->
      ( { vacationMode | state = vacationModeResult.state == "on" }, Cmd.none )

    Received (Err error) ->
      ( { vacationMode | error = toString error }, Cmd.none)


sendNewVacationModeState : VacationMode -> Cmd Msg
sendNewVacationModeState vacationMode =
  let
      stateJson = if vacationMode.state then "on" else "off"

      requestData = [
          ("start_time", Json.Encode.string <| timeOfDayToString vacationMode.averageStartTime),
          ("end_time", Json.Encode.string <| timeOfDayToString vacationMode.averageEndTime),
          ("state", Json.Encode.string stateJson)
      ]
      |> Json.Encode.object
      |> Http.jsonBody

      url = "/my_zwave/vacation_mode"
      request =
        Http.post url requestData JsonDecoders.vacationMode
  in
      Http.send Received request

-- VIEW

timeOfDayToString: TimeOfDay -> String
timeOfDayToString time =
  let
    hour = toString time.hour |> String.pad 2 '0'
    minute = toString time.minute |> String.pad 2 '0'
  in
    hour ++ ":" ++ minute

timeOfDayFromString: String -> Result String TimeOfDay
timeOfDayFromString time =
  let
    timeParts = String.split ":" time

    hour: Result String Int
    hour = timeParts
    |> List.head
    |> Result.fromMaybe "No hour part present"
    |> Result.andThen String.toInt

    minute: Result String Int
    minute = timeParts
    |> List.tail
    |> Maybe.andThen List.head
    |> Result.fromMaybe "No minute part exists"
    |> Result.andThen String.toInt

  in
    Result.map2 TimeOfDay hour minute

