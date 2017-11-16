module RecentEvents exposing (Model, Msg, load, new, update, view)

import Date
import Html exposing (Html, div, text, table, tbody, th, tr, td)
import Http
import Json.Decode as JD exposing (..)
import Material

type alias Model = {
    messages: List Event
  , errorMessage: Maybe String
  }

type alias Event = {
    initiator: Maybe String
  , event: String
  , data: Maybe String
  , time: Date.Date
  }
type Msg =
    Load
  | Received (Result Http.Error (List Event))

new : Model
new = {
    messages = []
  , errorMessage = Nothing
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Load -> (model, Cmd.none)
    Received (Ok events) -> ( { model | messages = events, errorMessage = Nothing }, Cmd.none)
    Received (Err error) -> ( { model | errorMessage = Just (toString error) }, Cmd.none)


load : Cmd Msg
load =
    let
        url =
            "/my_zwave/latest_events"

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
            Ok date -> succeed date
            Err error -> fail error

        date = string |> andThen convert
    in
        list (
          JD.map4 Event
            (maybe (field "initiator" string))
            (field "event" string)
            (maybe (field "data" string))
            (field "time" date)
        )

view : Material.Model -> Model -> Html Msg
view mdl model =
  let
      errorDiv = case model.errorMessage of
        Just message -> div [] [text message]
        Nothing -> div [] []

      monthToString : Date.Month -> String
      monthToString month = case month of
          Date.Jan -> "jan"
          Date.Feb -> "feb"
          Date.Mar -> "maa"
          Date.Apr -> "apr"
          Date.May -> "mei"
          Date.Jun -> "jun"
          Date.Jul -> "jul"
          Date.Aug -> "aug"
          Date.Sep -> "sep"
          Date.Oct -> "okt"
          Date.Nov -> "nov"
          Date.Dec -> "dec"

      timeToString : Date.Date -> String
      timeToString date =
        (toString (Date.day date)) ++ " " ++
        (monthToString (Date.month date)) ++ " " ++
        (toString (Date.year date))

      messageToLi : Event -> Html Msg
      messageToLi event = tr [] [
        td [] [text (timeToString event.time)]
      , td [] [text event.event]
      ]
  in
      div [] [
          table []
            [
              tbody []
               (List.map messageToLi model.messages)
            ]

          , errorDiv
      ]
--    {\"initiator\":\"wall switch\",\"event\":\"switch pressed\",\"data\":\"on\",\"time\":\"2017-09-26T18:43:41.298Z\"}"
