module Disposition exposing (..)

import CaseManagementData
import MsalData as Msal
import TriageData


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


type Action
    = Transition Location Party
    | Disposition Location Result


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


type Result
    = ChildSupport AgreementStatus
    | Continuance ContinuanceReason
    | CustodyVisitation CustodyVisitationResult
    | FOAH AgreementStatus
    | Hearing AgreementStatus
    | Ineligible IneligibleReason
    | Judgment AgreementStatus
    | OffCalendar OffCalendarReason


type CustodyVisitationResult
    = Memo
    | Status AgreementStatus


type AgreementStatus
    = Default Party
    | Dispute
    | FullStipulation
    | PartialStipulation


type CCRCReason
    = Agreement
    | Session


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
    | Modification
    | PendingProbateCase
    | PendingJuvenileCase
    | PendingDNA


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
                   , Disposition Triage (Ineligible Modification)
                   , Disposition Triage (Ineligible OptOut)
                   , Disposition Triage (Ineligible PendingDNA)
                   , Disposition Triage (Ineligible PendingJuvenileCase)
                   , Disposition Triage (Ineligible PendingProbateCase)
                   , Disposition Triage (Ineligible Represented)
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
            [ Transition (Send <| CCRC Agreement) BothParties
            , Transition (Send <| CCRC Session) BothParties
            , Transition (Send DCSS) BothParties
            , Disposition Triage (Judgment FullStipulation)
            , Disposition Triage (Judgment PartialStipulation)
            , Disposition Triage (Hearing PartialStipulation)
            , Disposition Triage (Hearing Dispute)
            , Disposition Triage (Continuance Other)
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
            [ Disposition (CCRC reason) (CustodyVisitation (Status FullStipulation))
            , Disposition (CCRC reason) (CustodyVisitation (Status PartialStipulation))
            , Disposition (CCRC reason) (CustodyVisitation (Status Dispute))
            , Disposition (CCRC reason) (CustodyVisitation Memo)
            , Transition Left Petitioner
            , Transition Left Respondent
            ]

        Disposed ->
            []


checkinActions : Location -> List Action
checkinActions location =
    [ Transition location Petitioner
    , Transition location Respondent
    , Transition location BothParties
    , Transition Left Petitioner
    , Transition Left Respondent
    ]


twoPartyActions : Location -> (AgreementStatus -> Result) -> List Action
twoPartyActions location resultType =
    [ Disposition location (resultType FullStipulation)
    , Disposition location (resultType PartialStipulation)
    , Disposition location (resultType Dispute)
    , Transition Left Petitioner
    , Transition Left Respondent
    ]


actionToString : Action -> String
actionToString action =
    case action of
        Transition (Send location) party ->
            "Send " ++ toString party ++ " to " ++ toString location

        Transition Left party ->
            toString party ++ " left (no session)"

        Transition location party ->
            "Checkin " ++ toString party ++ " " ++ toString location

        Disposition Triage result ->
            toString result

        Disposition _ (CustodyVisitation Memo) ->
            "CustodyVisitation Memo"

        Disposition _ (CustodyVisitation (Status status)) ->
            "CustodyVisitation " ++ toString status

        Disposition _ result ->
            toString result



-- Transition Location Party
-- Disposition Location Result


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
            CustodyVisitation (Status FullStipulation)

        "CustodyVisitation PartialStipulation" ->
            CustodyVisitation (Status PartialStipulation)

        "CustodyVisitation Dispute" ->
            CustodyVisitation (Status Dispute)

        "CustodyVisitation Memo" ->
            CustodyVisitation Memo

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

        "Ineligible Modification" ->
            Ineligible Modification

        "Ineligible PendingProbateCase" ->
            Ineligible PendingProbateCase

        "Ineligible PendingJuvenileCase" ->
            Ineligible PendingJuvenileCase

        "Ineligible PendingDNA" ->
            Ineligible PendingDNA

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


partyFromString : String -> Party
partyFromString string =
    case string of
        "Petitioner" ->
            Petitioner

        "Respondent" ->
            Respondent

        _ ->
            BothParties


ccrcResultToString result =
    case result of
        Status FullStipulation ->
            "CustodyVisitation FullStipulation"

        Status PartialStipulation ->
            "CustodyVisitation PartialStipulation"

        Status Dispute ->
            "CustodyVisitation Dispute"

        Memo ->
            "CustodyVisitation Memo"

        _ ->
            ""


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

        Disposition (CCRC reason) (CustodyVisitation result) ->
            TriageData.Event hearing.caseId
                "Disposition"
                (toString (CCRC reason))
                (ccrcResultToString result)
                user.id
                Nothing

        Disposition location result ->
            TriageData.Event hearing.caseId
                "Disposition"
                (toString location)
                (toString result)
                user.id
                Nothing



-- Move to other module


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
