module CaseManagementData exposing (..)

import Json.Decode
import Json.Decode.Pipeline exposing (decode, required, optional)


-- Possible "selfRepresented" values: "In Pro Per", "Unrepresented"
-- Possible "representedBy" values: List Attorney
-- Attorneys are partyType: "Attorney". Discard them.


type alias Hearing =
    { scheduledEventId : Int
    , scheduledEventDateTime : DateTime
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
        |> Json.Decode.Pipeline.required "schedEvntId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "schedEvntDateTime" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "caseId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "caseNumber" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "locationCode" (Json.Decode.string)
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
    { repFullName : String
    , organizationName : String
    }


attorneyDecoder : Json.Decode.Decoder Attorney
attorneyDecoder =
    Json.Decode.Pipeline.decode Attorney
        |> Json.Decode.Pipeline.required "repFullName" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "organizationName" (Json.Decode.string)


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
    { partyId : Int
    , partyType : String
    , personId : Int
    , firstName : Maybe String
    , lastName : Maybe String
    , organizationName : Maybe String
    , selfRepresented : Maybe String
    , attorneys : Maybe (List Attorney)
    }


partyDecoder : Json.Decode.Decoder Party
partyDecoder =
    Json.Decode.Pipeline.decode Party
        |> Json.Decode.Pipeline.required "partyId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "partyType" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "personId" (Json.Decode.int)
        |> Json.Decode.Pipeline.optional "firstName" (maybeStringDecoder) Nothing
        |> Json.Decode.Pipeline.optional "lastName" (maybeStringDecoder) Nothing
        |> Json.Decode.Pipeline.optional "organizationName" (maybeStringDecoder) Nothing
        |> Json.Decode.Pipeline.optional "selfRepresented" (maybeStringDecoder) Nothing
        |> Json.Decode.Pipeline.optional "representedBy" (maybeAttorneyDecoder) Nothing


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
