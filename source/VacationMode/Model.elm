module VacationMode.Model
    exposing
        ( VacationModeModel
        , new
        )

import Material
import TimeOfDay exposing (TimeOfDay)


type alias VacationModeModel =
    { state : Bool
    , averageStartTime : TimeOfDay
    , averageEndTime : TimeOfDay
    , error : String
    , mdl : Material.Model
    }


new : VacationModeModel
new =
    { state = False
    , averageStartTime = TimeOfDay 18 30
    , averageEndTime = TimeOfDay 22 30
    , error = ""
    , mdl = Material.model
    }
