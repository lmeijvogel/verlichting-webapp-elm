module MainSwitchState.Model exposing (MainSwitchState(..), MainSwitchModel, new)

import Material

type MainSwitchState
    = Unknown
    | Enabled
    | Disabled
    | Error

type alias MainSwitchModel =
  { state: MainSwitchState
  , mdl : Material.Model
  }

new : MainSwitchModel
new =
  { state = Unknown
  , mdl = Material.model
  }
