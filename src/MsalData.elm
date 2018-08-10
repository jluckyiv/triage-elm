port module MsalData exposing (..)

import Json.Encode
import Json.Decode
import Json.Decode.Pipeline


---- MODEL ----


type alias AccessToken =
    String


type alias User =
    { odataContext : String
    , id : String
    , displayName : String
    , givenName : String
    , mail : String
    , surname : String
    , userPrincipalName : String
    }


type alias IdTokenClaim =
    { displayableId : String
    , name : String
    , identityProvider : String
    , userIdentifier : String
    , idToken : IdToken
    }


type alias IdToken =
    { aud : String
    , iss : String
    , iat : Int
    , nbf : Int
    , exp : Int
    , aio : String
    , name : String
    , nonce : String
    , oid : String
    , preferred_username : String
    , sub : String
    , tid : String
    , uti : String
    , ver : String
    }


decodeUser : Json.Decode.Decoder User
decodeUser =
    Json.Decode.Pipeline.decode User
        |> Json.Decode.Pipeline.required "@odata.context" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "displayName" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "givenName" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "mail" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "surname" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "userPrincipalName" (Json.Decode.string)


encodeUser : User -> Json.Encode.Value
encodeUser record =
    Json.Encode.object
        [ ( "@odata.context", Json.Encode.string <| record.odataContext )
        , ( "id", Json.Encode.string <| record.id )
        , ( "displayName", Json.Encode.string <| record.displayName )
        , ( "givenName", Json.Encode.string <| record.givenName )
        , ( "mail", Json.Encode.string <| record.mail )
        , ( "surname", Json.Encode.string <| record.surname )
        , ( "userPrincipalName", Json.Encode.string <| record.userPrincipalName )
        ]


decodeIdTokenClaim : Json.Decode.Decoder IdTokenClaim
decodeIdTokenClaim =
    Json.Decode.Pipeline.decode IdTokenClaim
        |> Json.Decode.Pipeline.required "displayableId" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "identityProvider" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "userIdentifier" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "idToken" (decodeIdToken)


decodeIdToken : Json.Decode.Decoder IdToken
decodeIdToken =
    Json.Decode.Pipeline.decode IdToken
        |> Json.Decode.Pipeline.required "aud" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "iss" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "iat" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "nbf" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "exp" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "aio" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "nonce" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "oid" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "preferred_username" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "sub" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "tid" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "uti" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "ver" (Json.Decode.string)


encodeIdTokenClaim : IdTokenClaim -> Json.Encode.Value
encodeIdTokenClaim record =
    Json.Encode.object
        [ ( "displayableId", Json.Encode.string <| record.displayableId )
        , ( "name", Json.Encode.string <| record.name )
        , ( "identityProvider", Json.Encode.string <| record.identityProvider )
        , ( "userIdentifier", Json.Encode.string <| record.userIdentifier )
        , ( "idToken", encodeIdToken <| record.idToken )
        ]


encodeIdToken : IdToken -> Json.Encode.Value
encodeIdToken record =
    Json.Encode.object
        [ ( "aud", Json.Encode.string <| record.aud )
        , ( "iss", Json.Encode.string <| record.iss )
        , ( "iat", Json.Encode.int <| record.iat )
        , ( "nbf", Json.Encode.int <| record.nbf )
        , ( "exp", Json.Encode.int <| record.exp )
        , ( "aio", Json.Encode.string <| record.aio )
        , ( "name", Json.Encode.string <| record.name )
        , ( "nonce", Json.Encode.string <| record.nonce )
        , ( "oid", Json.Encode.string <| record.oid )
        , ( "preferred_username", Json.Encode.string <| record.preferred_username )
        , ( "sub", Json.Encode.string <| record.sub )
        , ( "tid", Json.Encode.string <| record.tid )
        , ( "uti", Json.Encode.string <| record.uti )
        , ( "ver", Json.Encode.string <| record.ver )
        ]



---- PORTS ----


port login : Json.Encode.Value -> Cmd msg


port logout : Json.Encode.Value -> Cmd msg


port loginResult : (Json.Decode.Value -> msg) -> Sub msg
