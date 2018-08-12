module Disposition exposing (..)

import CaseManagementData
import MsalData as Msal
import TriageData


type Location
    = CCRC CCRCReason
    | DCSS
    | Triage
    | Send Location
    | Left


type Party
    = BothParties
    | Petitioner
    | Respondent


type Action
    = Transition Location Party
    | Disposition Location Result


type State
    = Initial
    | Disposed
    | BothPartiesAtCCRC CCRCReason
    | BothPartiesAtTriage
    | BothPartiesSentCCRC CCRCReason
    | BothPartiesSentDCSS
    | BothPartiesSentTriage
    | OnePartyAtCCRC CCRCReason Party
    | OnePartyAtTriage Party
    | OnePartySentDCSS Party
    | OnePartySentTriage Party


createStateFromAction : Action -> State
createStateFromAction action =
    case action of
        Disposition Triage _ ->
            Disposed

        Disposition DCSS (ChildSupport (Default party)) ->
            OnePartySentTriage party

        Disposition location _ ->
            BothPartiesSentTriage

        Transition (CCRC reason) BothParties ->
            BothPartiesAtCCRC reason

        Transition Triage BothParties ->
            BothPartiesAtTriage

        Transition (Send (CCRC reason)) BothParties ->
            BothPartiesSentCCRC reason

        Transition (Send DCSS) BothParties ->
            BothPartiesSentDCSS

        Transition (Send Triage) BothParties ->
            BothPartiesSentTriage

        Transition (CCRC reason) party ->
            OnePartyAtCCRC reason party

        Transition Triage party ->
            OnePartyAtTriage party

        Transition (Send DCSS) party ->
            OnePartySentDCSS party

        Transition (Send Triage) party ->
            OnePartySentTriage party

        _ ->
            Initial


type Result
    = ChildSupport AgreementStatus
    | Continuance ContinuanceReason
    | CustodyVisitation AgreementStatus
    | FOAH AgreementStatus
    | Hearing AgreementStatus
    | Ineligible IneligibleReason
    | Judgment AgreementStatus
    | OffCalendar OffCalendarReason


type CCRCReason
    = Agreement
    | Session


type AgreementStatus
    = Default Party
    | Dispute
    | FullStipulation
    | PartialStipulation


type ContinuanceReason
    = DefectiveService
    | NoService
    | Other


type OffCalendarReason
    = FTA
    | Withdrawn


type IneligibleReason
    = DV
    | OptOut
    | Represented


otherParty : Party -> Party
otherParty party =
    case party of
        Petitioner ->
            Respondent

        Respondent ->
            Petitioner

        BothParties ->
            BothParties


availableActions : State -> List Action
availableActions state =
    case state of
        Initial ->
            checkinActions Triage
                ++ [ Disposition Triage (OffCalendar FTA)
                   , Disposition Triage (Ineligible DV)
                   , Disposition Triage (Ineligible Represented)
                   , Disposition Triage (Ineligible OptOut)
                   ]

        OnePartySentTriage party ->
            [ Transition Triage party ]

        BothPartiesSentTriage ->
            checkinActions Triage

        OnePartyAtTriage party ->
            [ Transition Triage BothParties
            , Transition (Send DCSS) party
            , Disposition Triage (Continuance DefectiveService)
            , Disposition Triage (Continuance NoService)
            , Disposition Triage (FOAH (Default party))
            , Disposition Triage (Judgment (Default party))
            , Disposition Triage (OffCalendar Withdrawn)
            ]

        BothPartiesAtTriage ->
            [ Disposition Triage (Continuance Other)
            , Disposition Triage (Judgment FullStipulation)
            , Disposition Triage (Judgment PartialStipulation)
            , Disposition Triage (Hearing Dispute)
            , Disposition Triage (Hearing PartialStipulation)
            , Transition (Send <| CCRC Agreement) BothParties
            , Transition (Send <| CCRC Session) BothParties
            , Transition (Send DCSS) BothParties
            ]

        BothPartiesSentCCRC reason ->
            checkinActions (CCRC reason)

        OnePartyAtCCRC reason party ->
            [ Transition (CCRC reason) BothParties
            , Transition (Send Triage) party
            ]

        BothPartiesSentDCSS ->
            twoPartyActions DCSS ChildSupport

        OnePartySentDCSS party ->
            [ Disposition DCSS (ChildSupport (Default party))
            ]

        BothPartiesAtCCRC reason ->
            twoPartyActions (CCRC reason) CustodyVisitation

        Disposed ->
            []


checkinActions : Location -> List Action
checkinActions location =
    [ Transition location Petitioner
    , Transition location Respondent
    , Transition location BothParties
    ]


twoPartyActions : Location -> (AgreementStatus -> Result) -> List Action
twoPartyActions location resultType =
    [ Disposition location (resultType FullStipulation)
    , Disposition location (resultType PartialStipulation)
    , Disposition location (resultType Dispute)
    , Transition Left Petitioner
    , Transition Left Respondent
    ]



-- Transition Location Party
-- Disposition Location Result


createActionFromEvent : TriageData.Event -> Action
createActionFromEvent event =
    case event.category of
        "Disposition" ->
            createDispositionFromEvent event

        "Transition" ->
            createTransitionFromEvent event

        _ ->
            createTransitionFromEvent event


createDispositionFromEvent : TriageData.Event -> Action
createDispositionFromEvent event =
    Disposition (locationFromString event.subject) (resultFromString event.action)


createTransitionFromEvent : TriageData.Event -> Action
createTransitionFromEvent event =
    Transition (locationFromString event.subject) (partyFromString event.action)


resultFromString : String -> Result
resultFromString string =
    case string of
        "ChildSupport FullStipulation" ->
            ChildSupport FullStipulation

        "ChildSupport PartialStipulation" ->
            ChildSupport PartialStipulation

        "ChildSupport Dispute" ->
            ChildSupport Dispute

        "ChildSupport (Default Petitioner)" ->
            ChildSupport (Default Petitioner)

        "ChildSupport (Default Respondent)" ->
            ChildSupport (Default Respondent)

        "Continuance DefectiveService" ->
            Continuance DefectiveService

        "Continuance NoService" ->
            Continuance NoService

        "Continuance Other" ->
            Continuance Other

        "CustodyVisitation FullStipulation" ->
            CustodyVisitation FullStipulation

        "CustodyVisitation PartialStipulation" ->
            CustodyVisitation PartialStipulation

        "CustodyVisitation Dispute" ->
            CustodyVisitation Dispute

        "FOAH (Default Petitioner)" ->
            FOAH (Default Petitioner)

        "FOAH (Default Respondent)" ->
            FOAH (Default Respondent)

        "Hearing Dispute" ->
            Hearing Dispute

        "Ineligible DV" ->
            Ineligible DV

        "Ineligible OptOut" ->
            Ineligible OptOut

        "Ineligible Represented" ->
            Ineligible Represented

        "Judgment FullStipulation" ->
            Judgment FullStipulation

        "Judgment PartialStipulation" ->
            Judgment PartialStipulation

        "Judgment (Default Petitioner)" ->
            Judgment (Default Petitioner)

        "Judgment (Default Respondent)" ->
            Judgment (Default Respondent)

        "OffCalendar FTA" ->
            OffCalendar FTA

        "OffCalendar Withdrawn" ->
            OffCalendar Withdrawn

        _ ->
            OffCalendar Withdrawn


locationFromString : String -> Location
locationFromString string =
    case string of
        "Send (CCRC Agreement)" ->
            Send (CCRC Agreement)

        "Send (CCRC Session)" ->
            Send (CCRC Session)

        "Send DCSS" ->
            Send DCSS

        "Send Triage" ->
            Send Triage

        "Left" ->
            Left

        "CCRC Agreement" ->
            CCRC Agreement

        "CCRC Session" ->
            CCRC Session

        "DCSS" ->
            DCSS

        "Triage" ->
            Triage

        _ ->
            Triage



--  "Send" Location | Send Location


partyFromString : String -> Party
partyFromString string =
    case string of
        "Petitioner" ->
            Petitioner

        "Respondent" ->
            Respondent

        _ ->
            BothParties


createEventFromAction : Msal.User -> CaseManagementData.Hearing -> Action -> TriageData.Event
createEventFromAction user hearing action =
    case action of
        Transition location party ->
            TriageData.Event hearing.caseId
                "Transition"
                (toString location)
                (toString party)
                user.id
                Nothing

        Disposition location result ->
            TriageData.Event hearing.caseId
                "Disposition"
                (toString location)
                (toString result)
                user.id
                Nothing



-- case action of
--     Transition location party ->
--     (
--     ,
--     case disposition of
--         Checkin location party ->
--             TriageData.Event hearing.caseId
--                 "checkin"
--                 (toString party)
--                 (toString location)
--                 user.id
--                 Nothing
--         Continuance reason ->
--             TriageData.Event hearing.caseId
--                 (toString Disposition)
--                 (toString Continuance)
--                 (toString reason)
--                 (user.id)
--                 Nothing
--         Dispatch location agreementStatus ->
--             TriageData.Event hearing.caseId
--                 (toString Dispatch)
--                 (toString location)
--                 (toString agreementStatus)
--                 (user.id)
--                 Nothing
--         Disposition location agreementStatus ->
--             TriageData.Event hearing.caseId
--                 (toString Disposition)
--                 (toString location)
--                 (toString agreementStatus)
--                 (user.id)
--                 Nothing
--         FOAH ->
--             TriageData.Event hearing.caseId
--                 (toString Disposition)
--                 (toString FOAH)
--                 (toString Default)
--                 (user.id)
--                 Nothing
--         Hearing agreementStatus ->
--             TriageData.Event hearing.caseId
--                 (toString Disposition)
--                 (toString Hearing)
--                 (toString agreementStatus)
--                 (user.id)
--                 Nothing
--         Judgment agreementStatus ->
--             TriageData.Event hearing.caseId
--                 (toString Disposition)
--                 (toString Hearing)
--                 (toString agreementStatus)
--                 (user.id)
--                 Nothing
--         OffCalendar reason ->
--             TriageData.Event hearing.caseId
--                 "disposition"
--                 "Off calendar"
--                 (toString reason)
--                 (user.id)
--                 Nothing
--         Stipulation agreementStatus ->
--             TriageData.Event hearing.caseId
--                 (toString Disposition)
--                 (toString Stipulation)
--                 (toString agreementStatus)
--                 (user.id)
--                 Nothing
--         Transition location party ->
--             TriageData.Event hearing.caseId
--                 "Transition"
--                 (toString location)
--                 (toString party)
--                 (user.id)
--                 Nothing


locationToColor : Location -> Color
locationToColor location =
    case location of
        Triage ->
            Success

        CCRC _ ->
            Success

        Left ->
            Danger

        Send _ ->
            Warning

        DCSS ->
            Warning



-- Move to other module


type Color
    = Danger
    | Info
    | Primary
    | Secondary
    | Success
    | Warning


type alias BootstrapClass =
    String


colorToBackgroundClass : Color -> BootstrapClass
colorToBackgroundClass color =
    case color of
        Info ->
            ".bg-info"

        Warning ->
            ".bg-warning"

        Success ->
            ".bg-success"

        Primary ->
            ".bg-primary"

        Danger ->
            ".bg-danger"

        Secondary ->
            ".bg-secondary"


colorToTextClass : Color -> BootstrapClass
colorToTextClass color =
    case color of
        Info ->
            ".text-info"

        Warning ->
            ".text-warning"

        Success ->
            ".text-success"

        Primary ->
            ".text-primary"

        Danger ->
            ".text-danger"

        Secondary ->
            ".text-secondary"
