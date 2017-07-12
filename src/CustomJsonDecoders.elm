module CustomJsonDecoders exposing (availableProgrammes, currentProgramme, activationResponse, PostProgrammeResult)

import Json.Decode as Decode
import Json.Decode exposing(..)

import Programme exposing (Programme)

type alias PostProgrammeResult =
  {
    success: Bool,
    programme: String,
    recipients: Int
  }

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
