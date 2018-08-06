module Disposition exposing (..)

import CaseManagementData exposing (Hearing)


type Party
    = Petitioner
    | Respondent
    | Both


type Status
    = Absent
    | CheckedIn
    | WaitingFor


type Location
    = Courtroom
    | Triage
    | CCRC
    | DCSS


type alias HearingDispositions =
    ( Hearing, List Disposition )


type Disposition
    = Checkin Location Party
    | Continuance Reason
    | Dispatch Location (Maybe AgreementStatus)
    | Disposition Location AgreementStatus
    | FOAH
    | FTA
    | Hearing AgreementStatus
    | Judgment AgreementStatus
    | OffCalendar
    | Stipulation AgreementStatus


type AgreementStatus
    = FullStipulation
    | PartialStipulation
    | Dispute
    | Default


type Reason
    = NoService
    | DefectiveService
    | Other


type State
    = NoParties Location
    | OneParty Party Location
    | BothParties Location


otherParty : Party -> Party
otherParty party =
    case party of
        Petitioner ->
            Respondent

        Respondent ->
            Petitioner

        Both ->
            Both


availableDispositions : State -> List Disposition
availableDispositions state =
    case state of
        NoParties DCSS ->
            [ Disposition DCSS FullStipulation
            , Disposition DCSS PartialStipulation
            , Disposition DCSS Dispute
            ]

        NoParties location ->
            [ Checkin location Petitioner
            , Checkin location Respondent
            , Checkin location Both
            , FTA
            ]

        OneParty party Courtroom ->
            [ Checkin Courtroom (otherParty party)
            , Continuance DefectiveService
            , Continuance NoService
            , FOAH
            , Judgment Default
            , OffCalendar
            ]

        BothParties Courtroom ->
            [ Dispatch CCRC (Just FullStipulation)
            , Dispatch CCRC (Just PartialStipulation)
            , Dispatch CCRC (Just Dispute)
            , Dispatch Triage Nothing
            ]

        OneParty party CCRC ->
            [ Checkin CCRC (otherParty party)
            , Dispatch Triage Nothing
            ]

        BothParties CCRC ->
            [ Disposition CCRC FullStipulation
            , Disposition CCRC PartialStipulation
            , Disposition CCRC Dispute
            ]

        BothParties Triage ->
            [ Continuance Other
            , Dispatch DCSS Nothing
            , Hearing Dispute
            , Judgment FullStipulation
            , Judgment PartialStipulation
            , Stipulation FullStipulation
            , Stipulation PartialStipulation
            ]

        _ ->
            []


updateDispositions : List HearingDispositions -> Hearing -> Disposition -> List HearingDispositions
updateDispositions dispositions hearing disposition =
    List.map
        (\( h, ds ) ->
            if h == hearing then
                ( h, (dispositionsToAdd disposition) ++ ds )
            else
                ( h, ds )
        )
        dispositions


dispositionsToAdd : Disposition -> List Disposition
dispositionsToAdd disposition =
    case disposition of
        Disposition location agreementStatus ->
            [ Dispatch Triage Nothing, Disposition location agreementStatus ]

        _ ->
            [ disposition ]
