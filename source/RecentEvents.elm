module RecentEvents exposing (Model, Msg, load, new, update, updateCurrentDate, view)

import Date
import DateToString exposing (dateToString)
import Html exposing (Html, div, table, tbody, td, text, th, tr)
import Http
import Json.Decode as JD exposing (..)
import Material


type alias Model =
    { messages : List Event
    , errorMessage : Maybe String
    , currentDate : Date.Date
    }


type alias Event =
    { initiator : Maybe String
    , event : String
    , data : Maybe String
    , time : Date.Date
    }


type Msg
    = Load
    | Received (Result Http.Error (List Event))


new : Model
new =
    { messages = []
    , errorMessage = Nothing
    , currentDate = Date.fromTime 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load ->
            ( model, Cmd.none )

        Received (Ok events) ->
            ( { model | messages = events, errorMessage = Nothing }, Cmd.none )

        Received (Err error) ->
            ( { model | errorMessage = Just (toString error) }, Cmd.none )


updateCurrentDate : Date.Date -> Model -> Model
updateCurrentDate date model =
    { model | currentDate = date }


load : Cmd Msg
load =
    let
        url =
            "/my_zwave/events"

        request =
            Http.get url decodeRecentEvents
    in
    Http.send Received request


decodeRecentEvents : Decoder (List Event)
decodeRecentEvents =
    let
        convert : String -> Decoder Date.Date
        convert input =
            case Date.fromString input of
                Ok date ->
                    succeed date

                Err error ->
                    fail error

        date =
            string |> andThen convert
    in
    list
        (JD.map4 Event
            (maybe (field "initiator" string))
            (field "event" string)
            (maybe (field "data" string))
            (field "time" date)
        )


view : Material.Model -> Model -> Html Msg
view mdl model =
    let
        errorDiv =
            case model.errorMessage of
                Just message ->
                    div [] [ text message ]

                Nothing ->
                    div [] []

        messageToLi : Event -> Html Msg
        messageToLi event =
            tr []
                [ td [] [ text (dateToString model.currentDate event.time) ]
                , td [] [ text event.event ]
                , td [] [ text (Maybe.withDefault "" event.data) ]
                ]
    in
    div []
        [ table []
            [ tbody []
                (List.map messageToLi model.messages)
            ]
        , errorDiv
        ]
