import Array
import Http
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Regex exposing (..)

import Json.Decode exposing(..)

main = Html.program { init = init,
                      view = view,
                      update = update,
                      subscriptions = subscriptions
                    }

-- MODEL

type alias Programme =
  {
    id: String,
    name: String
  }

type alias Model =
  { availableProgrammes: List Programme }

init : (Model, Cmd Msg)
init =
  (Model [], getAvailableProgrammes)


-- UPDATE

type Msg
  = RequestProgrammes
  | ProgrammesReceived (Result Http.Error (List Programme))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RequestProgrammes ->
      (model, getAvailableProgrammes)
    ProgrammesReceived (Ok availableProgrammes) ->
      ( { model | availableProgrammes = availableProgrammes }, Cmd.none)
    ProgrammesReceived (Err _) ->
      ( model, Cmd.none )

getAvailableProgrammes : Cmd Msg
getAvailableProgrammes =
  let
      url = "/available_programmes.json"

      request =
        Http.get url (field "availableProgrammes" programmeList)
  in
    Http.send ProgrammesReceived request


programmeList : Decoder (List Programme)
programmeList =
  let
      tupleToProgramme : (String, String) -> Programme
      tupleToProgramme (id, name) = Programme id name

      convert : List (String, String) -> Decoder (List Programme)
      convert tupleList = Decode.succeed (List.map tupleToProgramme tupleList)

  in
      keyValuePairs(string)
      |> Decode.andThen convert
      |> Decode.map List.reverse

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text "Available programmes"]
    , ul []
        (List.map (\programme -> li [] [text programme.name]) model.availableProgrammes)
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


