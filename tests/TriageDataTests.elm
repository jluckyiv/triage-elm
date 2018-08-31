module TriageDataTests exposing (..)

import Test exposing (..)
import Expect
import CaseManagementData exposing (..)
import TriageData exposing (..)
import Json.Decode as Decode


all : Test
all =
    describe "TriageData encoders and decoders"
        [ test "Unrepresented party" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "firstName": "JANE",
                            "lastName": "SMITH",
                            "personId": 473761,
                            "partyId": 410086,
                            "partyType": "Respondent",
                            "selfRepresented": "Unrepresented"
                        }
                        """

                    party =
                        Party 410086 "Respondent" 473761 (Just "JANE") (Just "SMITH") Nothing (Just "Unrepresented") Nothing
                in
                    Expect.equal (Ok party) (Decode.decodeString decodeParty json)
        ]
