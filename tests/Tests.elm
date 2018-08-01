module Tests exposing (..)

import Test exposing (..)
import Expect
import CaseManagementData exposing (..)
import Json.Decode as Decode


all : Test
all =
    describe "CBM data encoders and decoders"
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
        , test "Represented party" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "firstName": "JOHN",
                            "lastName": "SNOW",
                            "personId": 473760,
                            "partyId": 410085,
                            "partyType": "Petitioner",
                            "representedBy": [
                                {
                                    "repFullName": "Victor Emil Kaplan",
                                    "organizationName": "Kaplan Law Corp"
                                }
                            ]
                        }
                        """

                    attorney =
                        Attorney "Victor Emil Kaplan" "Kaplan Law Corp"

                    party =
                        Party 410085 "Petitioner" 473760 (Just "JOHN") (Just "SNOW") Nothing Nothing (Just [ attorney ])
                in
                    Expect.equal (Ok party) (Decode.decodeString decodeParty json)
        , test "Attorney party" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "firstName": "Victor",
                            "middleName": "Emil",
                            "lastName": "Kaplan",
                            "organizationName": "Kaplan Law Corp",
                            "partyId": 410084,
                            "personId": 473759,
                            "partyType": "Attorney"
                        }
                        """

                    party =
                        Party 410084 "Attorney" 473759 (Just "Victor") (Just "Kaplan") (Just "Kaplan Law Corp") Nothing Nothing
                in
                    Expect.equal (Ok party) (Decode.decodeString decodeParty json)
        , test "Interpreter" <|
            \_ ->
                let
                    json =
                        """
                    {
                        "language" : "Mandarin"
                    }
                    """

                    interpreter =
                        Interpreter "Mandarin"
                in
                    Expect.equal (Ok interpreter) (Decode.decodeString decodeInterpreter json)
        , test "Hearing" <|
            \_ ->
                let
                    json =
                        fullHearing

                    hearing =
                        Hearing 599319
                            "2018-08-06T10:00:00"
                            106514
                            "FLRI1800081"
                            "F201"
                            (Just [ Interpreter "Mandarin" ])
                            [ Party 410084 "Attorney" 473759 (Just "Victor") (Just "Kaplan") (Just "Kaplan Law Corp") Nothing Nothing
                            , Party 410085 "Petitioner" 473760 (Just "JOHN") (Just "SNOW") Nothing Nothing (Just [ Attorney "Victor Emil Kaplan" "Kaplan Law Corp" ])
                            , Party 410086 "Respondent" 473761 (Just "JANE") (Just "SMITH") Nothing (Just "Unrepresented") Nothing
                            , Party 410088 "Claimant" 473764 (Just "CALLY") (Just "SMITH") Nothing (Just "In Pro Per") Nothing
                            ]
                in
                    Expect.equal (Ok hearing) (Decode.decodeString decodeHearing json)
        , test "Calendar" <|
            \_ ->
                let
                    json =
                        departmentCalendar

                    calendar =
                        [ Hearing 599319
                            "2018-08-06T10:00:00"
                            106514
                            "FLRI1800081"
                            "F201"
                            (Just [ Interpreter "Mandarin" ])
                            [ Party 410084 "Attorney" 473759 (Just "Victor") (Just "Kaplan") (Just "Kaplan Law Corp") Nothing Nothing
                            , Party 410085 "Petitioner" 473760 (Just "JOHN") (Just "SNOW") Nothing Nothing (Just [ Attorney "Victor Emil Kaplan" "Kaplan Law Corp" ])
                            , Party 410086 "Respondent" 473761 (Just "JANE") (Just "SMITH") Nothing (Just "Unrepresented") Nothing
                            , Party 410088 "Claimant" 473764 (Just "CALLY") (Just "SMITH") Nothing (Just "In Pro Per") Nothing
                            ]
                        , Hearing 599318
                            "2018-08-06T08:30:00"
                            106508
                            "FLRI1800078"
                            "F201"
                            (Just [ Interpreter "Sign Language - American" ])
                            [ Party 410059 "Father" 473733 (Just "JOHN") (Just "SMITH") Nothing (Just "In Pro Per") Nothing
                            , Party 410060 "Father" 473734 (Just "WARREN") (Just "JONES") Nothing (Just "In Pro Per") Nothing
                            , Party 410061 "Child" 473735 (Just "TIM") (Just "SMITH") Nothing (Just "Unrepresented") Nothing
                            ]
                        , Hearing 599321
                            "2018-08-06T08:30:00"
                            106685
                            "FLRI1800110"
                            "F201"
                            (Just [ Interpreter "Sign Language - American" ])
                            [ Party 410556 "Petitioner" 474259 (Just "MISSY") (Just "STONES") Nothing (Just "In Pro Per") Nothing
                            , Party 410556 "Petitioner" 474259 (Just "MISSY") (Just "STONES") Nothing (Just "In Pro Per") Nothing
                            , Party 410557 "Respondent" 474260 (Just "DALE") (Just "STONE") Nothing (Just "Unrepresented") Nothing
                            ]
                        ]
                in
                    Expect.equal (Ok calendar) (Decode.decodeString decodeHearings json)
        ]


fullHearing : String
fullHearing =
    """
        {
            "schedEvntId": 599319,
            "schedEvntDateTime": "2018-08-06T10:00:00",
            "caseId": 106514,
            "schedEvntType": "HEP",
            "eventStatus": "EVENT",
            "schedEvntLocation": "Department F201",
            "docketNumber": 2147483647,
            "schedEvntName": "Ex-Parte Hearing",
            "caseNumber": "FLRI1800081",
            "caseName": "SNOW vs. SMITH",
            "caseType": "FL",
            "category": "ROSCD",
            "dispositionDate": "2018-06-25T00:00:00",
            "dispositionType": "AD",
            "judicialOfficer": "Belinda A. Handy",
            "caseLocation": "RI",
            "filingDate": "6/25/2018",
            "filedByProPer": "False",
            "locationCode": "F201",
            "locationName": "Department F201",
            "parentLocationCode": "FLD",
            "parentLocationName": "Riverside Family Law Courthouse",
            "interpreter": [
                {
                    "language": "Mandarin"
                }
            ],
            "party": [
                {
                    "firstName": "Victor",
                    "middleName": "Emil",
                    "lastName": "Kaplan",
                    "organizationName": "Kaplan Law Corp",
                    "partyId": 410084,
                    "personId": 473759,
                    "partyType": "Attorney"
                },
                {
                    "firstName": "JOHN",
                    "lastName": "SNOW",
                    "partyId": 410085,
                    "personId": 473760,
                    "representedBy": [
                        {
                            "repFullName": "Victor Emil Kaplan",
                            "organizationName": "Kaplan Law Corp"
                        }
                    ],
                    "partyType": "Petitioner"
                },
                {
                    "firstName": "JANE",
                    "lastName": "SMITH",
                    "partyId": 410086,
                    "personId": 473761,
                    "selfRepresented": "Unrepresented",
                    "partyType": "Respondent"
                },
                {
                    "firstName": "CALLY",
                    "lastName": "SMITH",
                    "partyId": 410088,
                    "personId": 473764,
                    "selfRepresented": "In Pro Per",
                    "partyType": "Claimant"
                }
            ]
        }
    """


departmentCalendar : String
departmentCalendar =
    """
[
    {
        "schedEvntId": 599319,
        "schedEvntDateTime": "2018-08-06T10:00:00",
        "caseId": 106514,
        "schedEvntType": "HEP",
        "eventStatus": "EVENT",
        "schedEvntLocation": "Department F201",
        "docketNumber": 2147483647,
        "schedEvntName": "Ex-Parte Hearing",
        "caseNumber": "FLRI1800081",
        "caseName": "SNOW vs. SMITH",
        "caseType": "FL",
        "category": "ROSCD",
        "dispositionDate": "2018-06-25T00:00:00",
        "dispositionType": "AD",
        "judicialOfficer": "Belinda A. Handy",
        "caseLocation": "RI",
        "filingDate": "6/25/2018",
        "filedByProPer": "False",
        "locationCode": "F201",
        "locationName": "Department F201",
        "parentLocationCode": "FLD",
        "parentLocationName": "Riverside Family Law Courthouse",
        "interpreter": [
            {
                "language": "Mandarin"
            }
        ],
        "party": [
            {
                "firstName": "Victor",
                "middleName": "Emil",
                "lastName": "Kaplan",
                "organizationName": "Kaplan Law Corp",
                "partyId": 410084,
                "personId": 473759,
                "partyType": "Attorney"
            },
            {
                "firstName": "JOHN",
                "lastName": "SNOW",
                "partyId": 410085,
                "personId": 473760,
                "representedBy": [
                    {
                        "repFullName": "Victor Emil Kaplan",
                        "organizationName": "Kaplan Law Corp"
                    }
                ],
                "partyType": "Petitioner"
            },
            {
                "firstName": "JANE",
                "lastName": "SMITH",
                "partyId": 410086,
                "personId": 473761,
                "selfRepresented": "Unrepresented",
                "partyType": "Respondent"
            },
            {
                "firstName": "CALLY",
                "lastName": "SMITH",
                "partyId": 410088,
                "personId": 473764,
                "selfRepresented": "In Pro Per",
                "partyType": "Claimant"
            }
        ]
    },
    {
        "schedEvntId": 599318,
        "schedEvntDateTime": "2018-08-06T08:30:00",
        "caseId": 106508,
        "schedEvntType": "CAUHS",
        "eventStatus": "EVENT",
        "schedEvntLocation": "Department F201",
        "docketNumber": 2147483647,
        "schedEvntName": "Child Abduction Unit Hearing Set",
        "caseNumber": "FLRI1800078",
        "caseName": "SMITH & JONES",
        "caseType": "FL",
        "category": "COJ",
        "dispositionDate": "2018-07-02T00:00:00",
        "dispositionType": "AD",
        "judicialOfficer": "Belinda A. Handy",
        "caseLocation": "RI",
        "filingDate": "7/2/2018",
        "filedByProPer": "True",
        "locationCode": "F201",
        "locationName": "Department F201",
        "parentLocationCode": "FLD",
        "parentLocationName": "Riverside Family Law Courthouse",
        "interpreter": [
            {
                "language": "Sign Language - American"
            }
        ],
        "party": [
            {
                "firstName": "JOHN",
                "lastName": "SMITH",
                "partyId": 410059,
                "personId": 473733,
                "selfRepresented": "In Pro Per",
                "partyType": "Father"
            },
            {
                "firstName": "WARREN",
                "lastName": "JONES",
                "partyId": 410060,
                "personId": 473734,
                "selfRepresented": "In Pro Per",
                "partyType": "Father"
            },
            {
                "firstName": "TIM",
                "lastName": "SMITH",
                "partyId": 410061,
                "personId": 473735,
                "selfRepresented": "Unrepresented",
                "partyType": "Child"
            }
        ]
    },
    {
        "schedEvntId": 599321,
        "schedEvntDateTime": "2018-08-06T08:30:00",
        "caseId": 106685,
        "schedEvntType": "PRCOM",
        "eventStatus": "EVENT",
        "schedEvntLocation": "Department F201",
        "docketNumber": 2147483647,
        "schedEvntName": "Court on its Own Motion:",
        "caseNumber": "FLRI1800110",
        "caseName": "STONES &. STONE",
        "caseType": "FL",
        "category": "CSM",
        "judicialOfficer": "Belinda A. Handy",
        "caseLocation": "RI",
        "filingDate": "7/6/2018",
        "filedByProPer": "True",
        "locationCode": "F201",
        "locationName": "Department F201",
        "parentLocationCode": "FLD",
        "parentLocationName": "Riverside Family Law Courthouse",
        "interpreter": [
            {
                "language": "Sign Language - American"
            }
        ],
        "party": [
            {
                "firstName": "MISSY",
                "lastName": "STONES",
                "partyId": 410556,
                "personId": 474259,
                "selfRepresented": "In Pro Per",
                "partyType": "Petitioner"
            },
            {
                "firstName": "MISSY",
                "lastName": "STONES",
                "dateOfBirth": "1/1/1970",
                "partyId": 410556,
                "personId": 474259,
                "selfRepresented": "In Pro Per",
                "partyType": "Petitioner"
            },
            {
                "firstName": "DALE",
                "lastName": "STONE",
                "dateOfBirth": "1/1/1969",
                "partyId": 410557,
                "personId": 474260,
                "selfRepresented": "Unrepresented",
                "partyType": "Respondent"
            }
        ]
    }
]
    """
