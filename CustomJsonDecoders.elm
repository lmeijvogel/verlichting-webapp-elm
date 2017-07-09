module CustomJsonDecoders exposing (availableProgrammes)

import Json.Decode as Decode
import Json.Decode exposing(..)

import Programme exposing (Programme)

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
