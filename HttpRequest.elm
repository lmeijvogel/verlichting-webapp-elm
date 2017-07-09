import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Regex exposing (..)

import Programme exposing (Programme)
import CustomJsonDecoders exposing (availableProgrammes, currentProgramme)

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
  }

init : (Model, Cmd Msg)
init =
  (Model [] "", getAvailableProgrammes)


-- UPDATE

type Msg
  = RequestProgrammes
  | ProgrammesReceived (Result Http.Error (List Programme))
  | CurrentProgrammeReceived (Result Http.Error String)
  | ProgrammeClicked Programme

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RequestProgrammes ->
      (model, getAvailableProgrammes)
    ProgrammesReceived (Ok availableProgrammes) ->
      ( { model | availableProgrammes = availableProgrammes }, getCurrentProgramme)
    ProgrammesReceived (Err _) ->
      ( model, Cmd.none )
    CurrentProgrammeReceived (Ok id) ->
      ( { model | currentProgramme = id }, Cmd.none )
    CurrentProgrammeReceived (Err _) ->
      ( model, Cmd.none )
    ProgrammeClicked programme ->
      ( { model | currentProgramme = programme.id }, Cmd.none )

getAvailableProgrammes : Cmd Msg
getAvailableProgrammes =
  let
      url = "/available_programmes.json"

      request =
        Http.get url availableProgrammes
  in
    Http.send ProgrammesReceived request

getCurrentProgramme : Cmd Msg
getCurrentProgramme =
  let
      url = "/current_programme.json"
      request =
        Http.get url currentProgramme
  in
      Http.send CurrentProgrammeReceived request

-- VIEW

view : Model -> Html Msg
view model =
  body [] [
    node "link" [rel "stylesheet", href "style.css"] [],
    div []
      [ h2 [] [text "Available programmes"]
      , ul []
          (List.map (\programme -> programmeEntry programme model.currentProgramme) model.availableProgrammes)
      ]
    ]

programmeEntry : Programme -> String -> Html Msg
programmeEntry programme currentProgramme =
  let
      buttonText =
        if programme.id == currentProgramme then
          programme.name ++ " (current) "
        else
          programme.name
  in
      li [onClick (ProgrammeClicked programme)] [text buttonText]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


