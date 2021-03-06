module TriageData exposing (..)

import Http
import Json.Decode
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Encode


feedUrl : String
feedUrl =
    "wss://triage/ws"


getEventsUrl : String
getEventsUrl =
    "https://triage/api/v1/events/timeStamp="


postEventUrl : String
postEventUrl =
    "https://triage/api/v1/event"


getNotesUrl : String
getNotesUrl =
    "https://triage/api/v1/notes/timeStamp="


postNoteUrl : String
postNoteUrl =
    "https://triage/api/v1/note"


type alias Feed =
    { notes : List Note
    , events : List Event
    }


decodeFeed : Json.Decode.Decoder Feed
decodeFeed =
    Json.Decode.Pipeline.decode Feed
        |> Json.Decode.Pipeline.required "notes" (Json.Decode.list decodeNote)
        |> Json.Decode.Pipeline.required "events" (Json.Decode.list decodeEvent)


encodeFeed : Feed -> Json.Encode.Value
encodeFeed record =
    Json.Encode.object
        [ ( "notes", Json.Encode.list <| List.map encodeNote <| record.notes )
        , ( "events", Json.Encode.list <| List.map encodeEvent <| record.events )
        ]


type alias Event =
    { matterId : Int
    , category : String
    , subject : String
    , action : String
    , userId : String
    , timestamp : Maybe String
    }


decodeEvents : Json.Decode.Decoder (List Event)
decodeEvents =
    Json.Decode.list decodeEvent


decodeEvent : Json.Decode.Decoder Event
decodeEvent =
    Json.Decode.Pipeline.decode Event
        |> Json.Decode.Pipeline.required "matterId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "category" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "action" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "userId" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "timestamp" (maybeStringDecoder) Nothing


encodeEvent : Event -> Json.Encode.Value
encodeEvent record =
    Json.Encode.object
        [ ( "matterId", Json.Encode.int <| record.matterId )
        , ( "category", Json.Encode.string <| record.category )
        , ( "subject", Json.Encode.string <| record.subject )
        , ( "action", Json.Encode.string <| record.action )
        , ( "userId", Json.Encode.string <| record.userId )
        ]


type alias Note =
    { matterId : Int
    , subject : String
    , body : String
    , userId : String
    , timestamp : Maybe String
    }


decodeNotes : Json.Decode.Decoder (List Note)
decodeNotes =
    Json.Decode.list decodeNote


decodeNote : Json.Decode.Decoder Note
decodeNote =
    Json.Decode.Pipeline.decode Note
        |> Json.Decode.Pipeline.required "matterId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "subject" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "body" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "userId" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "timestamp" maybeStringDecoder Nothing


encodeNote : Note -> Json.Encode.Value
encodeNote record =
    Json.Encode.object
        [ ( "matterId", Json.Encode.int <| record.matterId )
        , ( "subject", Json.Encode.string <| record.subject )
        , ( "body", Json.Encode.string <| record.body )
        , ( "userId", Json.Encode.string <| record.userId )
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


postEventRequest : Event -> Http.Request Event
postEventRequest event =
    Http.request
        { body = encodeEvent event |> Http.jsonBody
        , expect = Http.expectJson decodeEvent
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = postEventUrl
        , withCredentials = False
        }


postNoteRequest : Note -> Http.Request Note
postNoteRequest note =
    Http.request
        { body = encodeNote note |> Http.jsonBody
        , expect = Http.expectJson decodeNote
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = postNoteUrl
        , withCredentials = False
        }
