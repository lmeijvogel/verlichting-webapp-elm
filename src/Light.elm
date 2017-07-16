module Light exposing (Light(..))

type alias State = Bool
type alias Intensity = Int
type alias Name = String
type alias DisplayName = String

type Light
  = SwitchableLight Int Name DisplayName State
  | DimmableLight Int Name DisplayName Intensity
