module Lights.Model exposing (..)

import Json.Decode exposing (..)
import Material


type alias Light =
    { id : Int
    , name : String
    , displayName : String
    , value : LightValue
    }


type alias LightsModel =
    { lights : List Light
    , editingLightId : Maybe Int
    , error : Maybe String
    , mdl : Material.Model
    }


newLightsModel : LightsModel
newLightsModel =
    { lights = []
    , editingLightId = Nothing
    , error = Nothing
    , mdl = Material.model
    }


type LightValue
    = Level Int
    | State Bool


decodeLights : Decoder (List Light)
decodeLights =
    let
        toLight : Decoder Light
        toLight =
            map4 Light
                (field "node_id" int)
                (field "name" string)
                (field "display_name" string)
                (oneOf
                    [ map Level (field "value" int)
                    , map State (field "state" bool)
                    ]
                )
    in
        field "lights" (list toLight)


decodeChangeResponse : Decoder LightValue
decodeChangeResponse =
    oneOf
        [ map Level (field "level" int)
        , map State (field "state" bool)
        ]
