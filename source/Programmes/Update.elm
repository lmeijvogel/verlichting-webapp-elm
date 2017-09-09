module Programmes.Update exposing (Msg(..), update, load)

import Http
import Json.Decode exposing (Decoder)
import Material

import Programmes.Model exposing (..)
import JsonDecoders

type Msg =
    ProgrammeClicked Programme
    | ProgrammesReceived (Result Http.Error (List Programme))
    | ActivationResponseReceived (Result Http.Error JsonDecoders.PostProgrammeResult)
    | CurrentProgrammeReceived (Result Http.Error String)
    | Mdl (Material.Msg Msg)

update: Msg -> ProgrammesModel -> (ProgrammesModel, Cmd Msg)
update msg programmesModel =
  case msg of
    ProgrammesReceived (Ok availableProgrammes) ->
        ( { programmesModel | availableProgrammes = availableProgrammes }, getCurrentProgramme )
    ProgrammesReceived (Err error) ->
        ( { programmesModel | error = Just ("Could not retrieve programmes list: " ++ (toString error)) }, Cmd.none )

    CurrentProgrammeReceived (Ok id) ->
        ( { programmesModel | currentProgramme = Just id }, Cmd.none )
    CurrentProgrammeReceived (Err _) ->
        ( { programmesModel | error = Just "Could not retrieve current programme" }, Cmd.none )

    ProgrammeClicked programme ->
     ( { programmesModel | pendingProgramme = Just programme.id }, activateProgramme programme.id)
    ActivationResponseReceived (Ok result) ->
        case result of
            JsonDecoders.Success programme ->
              ( { programmesModel | currentProgramme = Just programme,
                  pendingProgramme = Nothing,
                  error = Nothing }
                , Cmd.none)

            JsonDecoders.Error ->
              ( { programmesModel | error = Just "Result was not success" }, Cmd.none )

    ActivationResponseReceived (Err _) ->
        ( { programmesModel | error = Just "An error occurred" }, Cmd.none )
    Mdl msg_ ->
        Material.update Mdl msg_ programmesModel

activateProgramme : String -> Cmd Msg
activateProgramme programmeId =
    let
        url =
            "/my_zwave/programme/" ++ programmeId ++ "/start"

        request =
            Http.post (Debug.log "URL" url) Http.emptyBody JsonDecoders.activationResponse
    in
        Http.send ActivationResponseReceived request

load : Cmd Msg
load =
    get JsonDecoders.availableProgrammes ProgrammesReceived "/my_zwave/available_programmes"


getCurrentProgramme : Cmd Msg
getCurrentProgramme =
    get JsonDecoders.currentProgramme CurrentProgrammeReceived "/my_zwave/current_programme"


type alias Url =
    String

get : Decoder a -> (Result Http.Error a -> Msg) -> Url -> Cmd Msg
get decoder msg url =
    let
        request =
            Http.get url decoder
    in
        Http.send msg request
