module TriageData exposing (..)

import CaseManagementData
import Json.Decode
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode


type alias Action =
    String


type alias Category =
    String


type alias DateTime =
    String


type alias Email =
    String


type alias Guid =
    String


type alias Id =
    Int


type alias Name =
    String


type alias Note =
    { note : String
    , dateTime : DateTime
    , hearing : CaseManagementData.Hearing
    }


type alias Subject =
    String


type alias Event =
    { caseNumber : String
    , category : Category
    , subject : Subject
    , author : String
    , action : String
    , timestamp : String
    }


decodeEvent : Json.Decode.Decoder Event
decodeEvent =
    Json.Decode.Pipeline.decode Event
        |> Json.Decode.Pipeline.required "CaseNumber" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "Category" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "Subject" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "Author" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "Action" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "Timestamp" (Json.Decode.string)


encodeEvent : Event -> Json.Encode.Value
encodeEvent record =
    Json.Encode.object
        [ ( "CaseNumber", Json.Encode.string <| record.caseNumber )
        , ( "Category", Json.Encode.string <| record.category )
        , ( "Subject", Json.Encode.string <| record.subject )
        , ( "Author", Json.Encode.string <| record.author )
        , ( "Action", Json.Encode.string <| record.action )
        , ( "Timestamp", Json.Encode.string <| record.timestamp )
        ]


type alias User =
    { id : Id
    , guid : Guid
    , email : Email
    , givenName : Name
    }


decodeUser : Json.Decode.Decoder User
decodeUser =
    Json.Decode.Pipeline.decode User
        |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "guid" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "email" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "givenName" (Json.Decode.string)


encodeUser : User -> Json.Encode.Value
encodeUser record =
    Json.Encode.object
        [ ( "id", Json.Encode.int <| record.id )
        , ( "guid", Json.Encode.string <| record.guid )
        , ( "email", Json.Encode.string <| record.email )
        , ( "givenName", Json.Encode.string <| record.givenName )
        ]


maybeIntDecoder : Json.Decode.Decoder (Maybe Int)
maybeIntDecoder =
    Json.Decode.map
        (\value ->
            if value == "" then
                Nothing
            else
                value
                    |> String.toInt
                    |> Result.toMaybe
        )
        Json.Decode.string


maybeStringDecoder : Json.Decode.Decoder (Maybe String)
maybeStringDecoder =
    Json.Decode.map
        (\value ->
            if value == "" then
                Nothing
            else
                Just value
        )
        Json.Decode.string
