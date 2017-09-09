module Programmes.Model exposing (..)

import Material

type alias ProgrammesModel =
    { availableProgrammes : List Programme
    , currentProgramme : Maybe String
    , pendingProgramme : Maybe String
    , error: Maybe String
    , mdl : Material.Model
    }


newProgrammesModel : ProgrammesModel
newProgrammesModel =
    { availableProgrammes = []
    , currentProgramme = Nothing
    , pendingProgramme = Nothing
    , error = Nothing
    , mdl = Material.model
    }

type alias Programme =
    { id : String
    , name : String
    }
