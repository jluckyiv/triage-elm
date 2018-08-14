port module Ports exposing (login, logout, loginResult)

import Json.Encode
import Json.Decode


---- PORTS ----


port login : Json.Encode.Value -> Cmd msg


port logout : Json.Encode.Value -> Cmd msg


port loginResult : (Json.Decode.Value -> msg) -> Sub msg
