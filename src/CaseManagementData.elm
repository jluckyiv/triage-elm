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


decodeHearings : Json.Decode.Decoder (List Hearing)
decodeHearings =
    Json.Decode.list decodeHearing


decodeHearing : Json.Decode.Decoder Hearing
decodeHearing =
    Json.Decode.Pipeline.decode Hearing
        |> Json.Decode.Pipeline.required "schedEvntId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "schedEvntDateTime" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "caseId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "caseNumber" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "locationCode" (Json.Decode.string)
        |> Json.Decode.Pipeline.optional "interpreter" (decodeMaybeInterpreter) Nothing
        |> Json.Decode.Pipeline.required "party" (Json.Decode.list decodeParty)


type alias Interpreter =
    { language : Language }


decodeInterpreter : Json.Decode.Decoder Interpreter
decodeInterpreter =
    Json.Decode.Pipeline.decode Interpreter
        |> Json.Decode.Pipeline.required "language" (Json.Decode.string)


decodeMaybeInterpreter : Json.Decode.Decoder (Maybe (List Interpreter))
decodeMaybeInterpreter =
    Json.Decode.map
        (\value ->
            if value == [] then
                Nothing
            else
                Just value
        )
        (Json.Decode.list decodeInterpreter)


type alias Attorney =
    { repFullName : String
    , organizationName : String
    }


decodeAttorney : Json.Decode.Decoder Attorney
decodeAttorney =
    Json.Decode.Pipeline.decode Attorney
        |> Json.Decode.Pipeline.required "repFullName" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "organizationName" (Json.Decode.string)


decodeMaybeAttorneys : Json.Decode.Decoder (Maybe (List Attorney))
decodeMaybeAttorneys =
    Json.Decode.map
        (\value ->
            if value == [] then
                Nothing
            else
                Just value
        )
        (Json.Decode.list decodeAttorney)


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


decodeParty : Json.Decode.Decoder Party
decodeParty =
    Json.Decode.Pipeline.decode Party
        |> Json.Decode.Pipeline.required "partyId" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "partyType" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "personId" (Json.Decode.int)
        |> Json.Decode.Pipeline.optional "firstName" (decodeMaybeString) Nothing
        |> Json.Decode.Pipeline.optional "lastName" (decodeMaybeString) Nothing
        |> Json.Decode.Pipeline.optional "organizationName" (decodeMaybeString) Nothing
        |> Json.Decode.Pipeline.optional "selfRepresented" (decodeMaybeString) Nothing
        |> Json.Decode.Pipeline.optional "representedBy" (decodeMaybeAttorneys) Nothing


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


decodeMaybeString : Json.Decode.Decoder (Maybe String)
decodeMaybeString =
    Json.Decode.map
        (\value ->
            if value == "" then
                Nothing
            else
                Just value
        )
        Json.Decode.string
