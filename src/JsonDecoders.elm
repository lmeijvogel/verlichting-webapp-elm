module JsonDecoders exposing (checkLogin, availableProgrammes, currentProgramme, activationResponse, vacationMode, LoginState, PostProgrammeResult, VacationModeResult)

import Json.Decode as Decode
import Json.Decode exposing(..)

import Programme exposing (Programme)

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

type alias VacationModeResult = {
  state: String
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

vacationMode : Decoder (VacationModeResult)
vacationMode =
  map VacationModeResult ( field "state" string )
