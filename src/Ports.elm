port module Ports exposing (..)

import Json.Encode
import Json.Decode


---- PORTS ----


port login : Json.Encode.Value -> Cmd msg


port logout : Json.Encode.Value -> Cmd msg


port loginResult : (Json.Decode.Value -> msg) -> Sub msg


port hostRequest : String -> Cmd msg


port hostResult : (String -> msg) -> Sub msg
