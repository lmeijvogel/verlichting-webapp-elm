import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Regex exposing (..)

import Programme exposing (Programme)
import CustomJsonDecoders exposing (availableProgrammes, currentProgramme, activationResponse, PostProgrammeResult)

main = Html.program { init = init,
                      view = view,
                      update = update,
                      subscriptions = subscriptions
                    }

-- MODEL

type alias Model =
  {
    availableProgrammes: List Programme
  , currentProgramme : String
  , pendingProgramme : String
  , error: String
  }

init : (Model, Cmd Msg)
init =
  (Model [] "" "" "", getAvailableProgrammes)


-- UPDATE

type Msg
  = RequestProgrammes
  | ProgrammesReceived (Result Http.Error (List Programme))
  | CurrentProgrammeReceived (Result Http.Error String)
  | ProgrammeClicked Programme
  | ActivationResponseReceived (Result Http.Error PostProgrammeResult)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RequestProgrammes ->
      (model, getAvailableProgrammes)
    ProgrammesReceived (Ok availableProgrammes) ->
      ( { model | availableProgrammes = availableProgrammes }, getCurrentProgramme)
    ProgrammesReceived (Err _) ->
      ( { model | error = "Could not retrieve programmes list" }, Cmd.none )
    CurrentProgrammeReceived (Ok id) ->
      ( { model | currentProgramme = id }, Cmd.none )
    CurrentProgrammeReceived (Err _) ->
      ( { model | error = "Could not retrieve current programme" }, Cmd.none )
    ProgrammeClicked programme ->
      ( { model | pendingProgramme = programme.id }, activateProgramme programme )
    ActivationResponseReceived (Ok result) ->
      if result.success then
        ( { model | currentProgramme = result.programme, pendingProgramme = "" }, Cmd.none )
      else
        ( { model | error = "Result was not success" }, Cmd.none)
    ActivationResponseReceived (Err _) ->
      ( { model | error = "An error occurred" }, Cmd.none)

getAvailableProgrammes : Cmd Msg
getAvailableProgrammes =
  let
      url = "/my_zwave/available_programmes"

      request =
        Http.get url availableProgrammes
  in
    Http.send ProgrammesReceived request

getCurrentProgramme : Cmd Msg
getCurrentProgramme =
  let
      url = "/my_zwave/current_programme"
      request =
        Http.get url currentProgramme
  in
      Http.send CurrentProgrammeReceived request

activateProgramme : Programme -> Cmd Msg
activateProgramme programme =
  let
      url = "/my_zwave/programme/" ++ programme.id ++ "/start"
      request =
        Http.post url Http.emptyBody activationResponse
  in
      Http.send ActivationResponseReceived request

-- VIEW

view : Model -> Html Msg
view model =
  body [] [
    node "link" [rel "stylesheet", href "style.css"] [],
    div []
      [ h2 [] [text "Available programmes"]
      , ul []
          (List.map (\programme -> programmeEntry programme model.currentProgramme model.pendingProgramme) model.availableProgrammes)
      , div [] [ text model.error ]
      ]
    ]

programmeEntry : Programme -> String -> String -> Html Msg
programmeEntry programme currentProgramme pendingProgramme =
  let
      buttonText =
        if programme.id == currentProgramme then
          programme.name ++ " (current)"
        else if programme.id == pendingProgramme then
          programme.name ++ " (pending)"
        else
          programme.name
  in
      li [onClick (ProgrammeClicked programme)] [text buttonText]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


