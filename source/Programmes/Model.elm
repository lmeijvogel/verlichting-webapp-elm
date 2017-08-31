module Programmes.Model exposing (..)

type alias ProgrammesModel =
    { availableProgrammes : List Programme
    , currentProgramme : Maybe String
    , pendingProgramme : Maybe String
    }


newProgrammesModel : ProgrammesModel
newProgrammesModel =
    { availableProgrammes = []
    , currentProgramme = Nothing
    , pendingProgramme = Nothing
    }

type alias Programme =
    { id : String
    , name : String
    }
