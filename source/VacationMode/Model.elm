module VacationMode.Model
    exposing
        ( VacationModeModel
        , State(..)
        , new
        )

import Material
import TimeOfDay exposing (TimeOfDay)


type State
    = Enabled TimeOfDay TimeOfDay
    | Disabled
    | Unknown


type alias VacationModeModel =
    { state : State
    , nextStartTime : TimeOfDay
    , nextEndTime : TimeOfDay
    , error : String
    , mdl : Material.Model
    }


new : VacationModeModel
new =
    { state = Unknown
    , nextStartTime = TimeOfDay 18 30
    , nextEndTime = TimeOfDay 22 30
    , error = ""
    , mdl = Material.model
    }
