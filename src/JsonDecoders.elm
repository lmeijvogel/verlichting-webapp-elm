module JsonDecoders exposing (checkLogin, availableProgrammes, currentProgramme, activationResponse, lights, LoginState, PostProgrammeResult)

import Json.Decode as Decode
import Json.Decode exposing(..)

import Programme exposing (Programme)

import Light exposing (..)

type alias LoginState =
  {
    loggedIn: Bool
  }

type alias PostProgrammeResult =
  {
    success: Bool,
    programme: String,
    recipients: Int
  }

checkLogin : Decoder LoginState
checkLogin =
  let
      convert result = Decode.succeed (LoginState result)
  in
    (field "loggedIn" bool)
    |> Decode.andThen convert

availableProgrammes : Decoder (List Programme)
availableProgrammes =
  let
      tupleToProgramme : (String, String) -> Programme
      tupleToProgramme (id, name) = Programme id name

      convert : List (String, String) -> Decoder (List Programme)
      convert tupleList = Decode.succeed (List.map tupleToProgramme tupleList)

  in
      field "availableProgrammes" (keyValuePairs(string))
      |> Decode.andThen convert
      |> Decode.map List.reverse

currentProgramme : Decoder String
currentProgramme = field "programme" string

activationResponse : Decoder PostProgrammeResult
activationResponse = map3 PostProgrammeResult
                      (field "success" bool)
                      (field "programme" string)
                      (field "recipients" int)

lights : Decoder (List Light)
lights =
  let
    toLight : Decoder Light
    toLight = Json.Decode.oneOf
      [
        map4 DimmableLight
          (field "node_id" int)
          (field "name" string)
          (field "display_name" string)
          (field "value" int)
       ,
        map4 SwitchableLight
          (field "node_id" int)
          (field "name" string)
          (field "display_name" string)
          (field "state" bool)
      ]
  in
    field "lights" (list toLight)

