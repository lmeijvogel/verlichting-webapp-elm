module VacationMode exposing
    ( Model
    , Msg
    , isEnabled
    , load
    , new
    , update
    , view
    )

import Html exposing (Html, input, p, text)
import Html.Attributes
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Elevation as Elevation
import Material.List as MatList
import Material.Options as Options
import Material.Typography as Typo


type State
    = Enabled TimeOfDay TimeOfDay
    | Disabled


type VacationModeState
    = Loaded State
    | Loading
    | Error String


type alias Model =
    { state : VacationModeState
    , nextStartTime : TimeOfDay
    , nextEndTime : TimeOfDay
    , mdl : Material.Model
    }


new : Model
new =
    { state = Loading
    , nextStartTime = { hour = 18, minute = 30 }
    , nextEndTime = { hour = 22, minute = 30 }
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
        Loaded state ->
            case state of
                Enabled _ _ ->
                    True

                _ ->
                    False

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
update msg model =
    case msg of
        Enable ->
            let
                defaultStartAndEndTime =
                    ( model.nextStartTime, model.nextEndTime )

                ( startTime, endTime ) =
                    case model.state of
                        Loaded state ->
                            case state of
                                Enabled startTime endTime ->
                                    ( startTime, endTime )

                                _ ->
                                    defaultStartAndEndTime

                        _ ->
                            defaultStartAndEndTime

                newState : State
                newState =
                    Enabled startTime endTime

                newVacationMode : Model
                newVacationMode =
                    { model | state = Loaded newState }
            in
            ( model, sendNewVacationModeState model newState )

        Disable ->
            let
                newVacationMode =
                    { model | state = Loaded Disabled }
            in
            ( model, sendNewVacationModeState model Disabled )

        StartTimeChanged startTimeString ->
            case timeOfDayFromString startTimeString of
                Ok time ->
                    ( { model | nextStartTime = time }, Cmd.none )

                Err string ->
                    ( { model | state = Error ("Invalid start time: " ++ string) }, Cmd.none )

        EndTimeChanged endTimeString ->
            case timeOfDayFromString endTimeString of
                Ok time ->
                    ( { model | nextEndTime = time }, Cmd.none )

                Err string ->
                    ( { model | state = Error ("Invalid end time: " ++ string) }, Cmd.none )

        Received (Ok vacationModeResult) ->
            let
                newState =
                    if vacationModeResult.state == "on" then
                        Loaded (Enabled vacationModeResult.start_time vacationModeResult.end_time)

                    else
                        Loaded Disabled
            in
            ( { model | state = newState, nextStartTime = vacationModeResult.start_time, nextEndTime = vacationModeResult.end_time }, Cmd.none )

        Received (Err error) ->
            ( { model | state = Error (httpErrorToString error) }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadStatus response ->
            response.status.message

        Http.BadUrl _ ->
            "Invalid URL"

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadPayload _ _ ->
            "Bad payload"


sendNewVacationModeState : Model -> State -> Cmd Msg
sendNewVacationModeState model state =
    let
        stateJson =
            case state of
                Enabled _ _ ->
                    "on"

                Disabled ->
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

        requestJson =
            requestData
                |> Json.Encode.object
                |> Http.jsonBody

        url =
            "/my_zwave_new/vacation_mode"

        ( defaultStartTime, defaultEndTime ) =
            case state of
                Enabled start end ->
                    ( start, end )

                _ ->
                    ( model.nextStartTime, model.nextEndTime )

        request =
            Http.post url requestJson (decodeVacationMode defaultStartTime defaultEndTime)
    in
    Http.send Received request


load : Cmd Msg
load =
    let
        url =
            "/my_zwave_new/vacation_mode"

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
                    fail ("Invalid input value" ++ toString input ++ "'")
    in
    map3 VacationModeResult
        (field "state" string)
        (maybe (field "start_time" string) |> andThen (convert defaultStart))
        (maybe (field "end_time" string) |> andThen (convert defaultEnd))



-- VIEW --


type alias Mdl =
    Material.Model


view : Material.Model -> Model -> Html Msg
view mdl vacationModeModel =
    let
        on =
            case vacationModeModel.state of
                Loaded state ->
                    case state of
                        Enabled _ _ ->
                            True

                        _ ->
                            False

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
    Card.view [ Elevation.e2 ]
        [ Card.title []
            [ let
                titleText =
                    case vacationModeModel.state of
                        Loaded state ->
                            case state of
                                Enabled _ _ ->
                                    "Vacation mode is ON"

                                Disabled ->
                                    "Vacation mode is OFF"

                        Loading ->
                            "Vacation mode (loading)"

                        Error _ ->
                            "Vacation mode (error)"
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
            , MatList.li []
                [ MatList.content []
                    (case vacationModeModel.state of
                        Error error ->
                            [ text ("Error: " ++ error) ]

                        _ ->
                            []
                    )
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
