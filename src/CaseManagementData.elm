module CaseManagementData exposing (..)

import Json.Decode
import Json.Decode.Pipeline exposing (decode, required, optional)


hearingsUrl : ( String, Department ) -> String
hearingsUrl ( date, department ) =
    -- dev
    -- "https://cbmdev.riverside.courts.ca.gov/Hearing/FL/" ++ date ++ "/" ++ department
    -- prod
    "https://cbmtriage/Hearing/FL/" ++ date ++ "/" ++ department



-- local
-- "http://localhost:4049/" ++ department
-- postman
-- "https://09ee33ca-34c1-48c5-a6f8-31e31c976519.mock.pstmn.io/" ++ date ++ "/" ++ department


type alias Hearing =
    { scheduledEventId : Int
    , scheduledEventDateTime : DateTime
    , scheduledEventType : EventType
    , scheduledEventName : EventName
    , caseId : Int
    , caseNumber : CaseNumber
    , department : Department
    , interpreter : Maybe (List Interpreter)
    , parties : List Party
    }


hearingsDecoder : Json.Decode.Decoder (List Hearing)
hearingsDecoder =
    Json.Decode.list hearingDecoder


hearingDecoder : Json.Decode.Decoder Hearing
hearingDecoder =
    Json.Decode.Pipeline.decode Hearing
        |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "startDateTime" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "type" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "schedEvntName" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "case_id" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "caseNumber" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "code" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "interpreter" (maybeInterpreterDecoder) Nothing
        |> Json.Decode.Pipeline.required "party" (Json.Decode.list partyDecoder)


type alias Interpreter =
    { language : Language }


interpreterDecoder : Json.Decode.Decoder Interpreter
interpreterDecoder =
    Json.Decode.Pipeline.decode Interpreter
        |> Json.Decode.Pipeline.required "language" (Json.Decode.string)


maybeInterpreterDecoder : Json.Decode.Decoder (Maybe (List Interpreter))
maybeInterpreterDecoder =
    Json.Decode.map
        (\value ->
            if value == [] then
                Nothing
            else
                Just value
        )
        (Json.Decode.list interpreterDecoder)


type alias Attorney =
    { organizationName : Maybe String
    , repFullName : Maybe String
    }


attorneyDecoder : Json.Decode.Decoder Attorney
attorneyDecoder =
    Json.Decode.Pipeline.decode Attorney
        |> Json.Decode.Pipeline.optional "organizationName" (maybeStringDecoder) Nothing
        |> Json.Decode.Pipeline.optional "repFullName" (maybeStringDecoder) Nothing


maybeAttorneyDecoder : Json.Decode.Decoder (Maybe (List Attorney))
maybeAttorneyDecoder =
    Json.Decode.map
        (\value ->
            if value == [] then
                Nothing
            else
                Just value
        )
        (Json.Decode.list attorneyDecoder)


type alias Party =
    { id : Int
    , partyType : String
    , firstName : Maybe String
    , lastName : Maybe String
    , organizationName : Maybe String
    , selfRepresented : Maybe String
    , attorneys : Maybe (List Attorney)
    }


partyDecoder : Json.Decode.Decoder Party
partyDecoder =
    Json.Decode.Pipeline.decode Party
        |> Json.Decode.Pipeline.required "id" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "partyType" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "firstName" (maybeStringDecoder) Nothing
        |> Json.Decode.Pipeline.optional "lastName" (maybeStringDecoder) Nothing
        |> Json.Decode.Pipeline.optional "organizationName" (maybeStringDecoder) Nothing
        |> Json.Decode.Pipeline.optional "selfRepresented" (maybeStringDecoder) Nothing
        |> Json.Decode.Pipeline.optional "representedBy" (maybeAttorneyDecoder) Nothing


type alias EventType =
    String


type alias EventName =
    String


type alias DateTime =
    String


type alias CaseNumber =
    String


type alias Department =
    String


type alias BirthDate =
    String


type alias Language =
    String


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
