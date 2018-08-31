module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing
import CaseManagementData exposing (DateTime, Department, Hearing, Interpreter, Party, Attorney)
import Date
import Dict exposing (Dict)
import Disposition
import Html exposing (Html, text, div, span, p, ol, ul, li, h1)
import Html.Attributes exposing (class, id, src, type_, placeholder, href, attribute, value)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Json.Encode
import List.Extra
import Maybe.Extra
import MsalData as Msal
import Moment
import Ports
import RemoteData exposing (WebData)
import Task
import TriageData exposing (Event, Note)
import WebSocket


---- MODEL ----


type alias Model =
    { currentDateString : DateString
    , departmentDropdownState : Dropdown.State
    , departmentFilter : DepartmentFilter
    , departmentFilters : List DepartmentFilter
    , departments : List Department
    , events : WebData (List TriageData.Event)
    , hearings : Dict Department HearingResponse
    , hearingsDropdownStates : Dict CaseNumber Dropdown.State
    , statusFilter : StatusFilter
    , statusFilters : List StatusFilter
    , navbarState : Navbar.State
    , noteBoxValues : Dict CaseNumber String
    , host : String
    , notes : WebData (List Note)
    , searchBoxValue : String
    , statusDropdownState : Dropdown.State
    , user : Maybe Msal.User
    }


type alias CaseNumber =
    String


type alias DateString =
    String


type StatusFilter
    = Any
    | Pending
    | CCRC
    | DCSS
    | Triage
    | Eligible


type DepartmentFilter
    = All
    | F201
    | F301
    | F401
    | F402
    | F501
    | F502


type alias Department =
    String


type alias HearingResponse =
    WebData (List Hearing)


type alias Flags =
    { host : String, user : Json.Decode.Value }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        state =
            Dropdown.initialState

        host =
            flags.host

        user =
            decodeUser flags.user
    in
        ( { currentDateString = ""
          , departmentDropdownState = state
          , departmentFilter = All
          , departmentFilters = [ All, F201, F301, F401, F402, F501, F502 ]
          , departments = [ "F201", "F301", "F401", "F402", "F501", "F502" ]
          , events = RemoteData.Loading
          , hearings = initHearings
          , hearingsDropdownStates = Dict.empty
          , statusFilter = Any
          , statusFilters = [ Any, Eligible, Pending, CCRC, DCSS, Triage ]
          , navbarState = navbarState
          , noteBoxValues = Dict.empty
          , host = host
          , notes = RemoteData.Loading
          , searchBoxValue = ""
          , statusDropdownState = state
          , user = user
          }
        , Cmd.batch
            [ navbarCmd
            , Task.perform ReceiveDate Date.now
            , requestTodaysEventsCmd host
            , requestTodaysNotesCmd host
            ]
        )


initHearings : Dict Department HearingResponse
initHearings =
    Dict.fromList
        [ ( toString F201, RemoteData.Loading )
        , ( toString F301, RemoteData.Loading )
        , ( toString F401, RemoteData.Loading )
        , ( toString F402, RemoteData.Loading )
        , ( toString F501, RemoteData.Loading )
        , ( toString F502, RemoteData.Loading )
        ]


initHearingDropdownStates : Dict Department HearingResponse -> Dict CaseNumber Dropdown.State
initHearingDropdownStates departmentHearings =
    departmentHearings
        |> Dict.values
        |> List.concatMap (RemoteData.withDefault [])
        |> List.map (\h -> ( h.caseNumber, Dropdown.initialState ))
        |> Dict.fromList


initNoteBoxValues : Dict Department HearingResponse -> Dict CaseNumber String
initNoteBoxValues departmentHearings =
    departmentHearings
        |> Dict.values
        |> List.concatMap (RemoteData.withDefault [])
        |> List.map (\h -> ( h.caseNumber, "" ))
        |> Dict.fromList


countHearings : Dict Department HearingResponse -> Int
countHearings departmentHearings =
    departmentHearings
        |> Dict.values
        |> List.concatMap (RemoteData.withDefault [])
        |> List.length



---- UPDATE ----


type Msg
    = NoOp
    | AddEvent Hearing Disposition.Action
    | AddNote Hearing
    | FilterDepartment DepartmentFilter
    | FilterStatus StatusFilter
    | Login Json.Encode.Value
    | LoginResult Json.Decode.Value
    | Logout Json.Encode.Value
    | NavbarMsg Navbar.State
    | NewFeed String
    | OnEventSave (Result Http.Error Event)
    | OnNoteSave (Result Http.Error Note)
    | ReceiveDate Date.Date
    | ReceiveEvents (WebData (List Event))
    | ReceiveHearings Department HearingResponse
    | ReceiveNotes (WebData (List Note))
    | RequestEvents String
    | RequestHearings
    | RequestNotes String
    | RequestTodaysEventsCmd
    | RequestTodaysNotes
    | ToggleDepartmentDropdown Dropdown.State
    | ToggleHearingDropdown String Dropdown.State
    | ToggleStatusDropdown Dropdown.State
    | UpdateCurrentDateString DateString
    | UpdateNoteBoxValue Hearing String
    | UpdateSearchBoxValue String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddEvent hearing action ->
            ( model, addEventCmd model hearing action )

        AddNote hearing ->
            let
                noteBoxValues =
                    Dict.update hearing.caseNumber
                        (\_ -> Just "")
                        model.noteBoxValues
            in
                ( { model
                    | noteBoxValues = noteBoxValues

                    {- TODO: update UI with "Saving" -}
                  }
                , addNoteCmd model hearing
                )

        FilterDepartment department ->
            ( { model | departmentFilter = department }
            , Cmd.none
            )

        FilterStatus status ->
            ( { model | statusFilter = status }
            , Cmd.none
            )

        Login value ->
            ( model, Ports.login value )

        LoginResult value ->
            ( { model | user = decodeUser value }, Cmd.none )

        Logout value ->
            ( { model | user = Nothing }, Ports.logout value )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        NewFeed json ->
            let
                feed =
                    Json.Decode.decodeString TriageData.decodeFeed json
            in
                case feed of
                    Ok data ->
                        ( { model
                            | events = RemoteData.Success data.events
                            , notes = RemoteData.Success data.notes
                          }
                        , Cmd.none
                        )

                    Err error ->
                        let
                            _ =
                                Debug.log "NewFeed error" error
                        in
                            ( model, Cmd.none )

        OnEventSave (Ok event) ->
            Debug.log "OnEventSave Ok" ( model, Cmd.none )

        OnEventSave (Err error) ->
            let
                _ =
                    Debug.log "OnEventSave Err" error
            in
                ( model, Cmd.none )

        OnNoteSave (Ok event) ->
            Debug.log "OnNoteSave Ok" ( model, Cmd.none )

        OnNoteSave (Err error) ->
            let
                _ =
                    Debug.log "OnNoteSave Err" error
            in
                ( model, Cmd.none )

        ReceiveDate date ->
            let
                currentDateString =
                    dateToDateString date
            in
                ( { model | currentDateString = currentDateString }
                , requestHearingsCmd currentDateString
                )

        ReceiveEvents response ->
            ( { model | events = response }, Cmd.none )

        ReceiveHearings department response ->
            let
                hearings =
                    (receiveHearings model.hearings department response)
                        |> Dict.map (\_ hs -> RemoteData.map filterTriageHearings hs)

                hearingDropdownStates =
                    initHearingDropdownStates hearings
            in
                ( { model
                    | hearings = hearings
                    , hearingsDropdownStates = hearingDropdownStates
                    , noteBoxValues = initNoteBoxValues hearings
                  }
                , Cmd.none
                )

        ReceiveNotes response ->
            ( { model | notes = response }, Cmd.none )

        RequestEvents dateIsoString ->
            ( { model | events = RemoteData.Loading }
            , Cmd.batch [ requestEventsCmd model.host dateIsoString ]
            )

        RequestTodaysEventsCmd ->
            ( { model | events = RemoteData.Loading }
            , requestTodaysEventsCmd model.host
            )

        RequestHearings ->
            let
                currentDateString =
                    model.searchBoxValue
            in
                ( { model
                    | hearings = initHearings
                    , currentDateString = currentDateString
                    , searchBoxValue = ""
                  }
                , requestHearingsCmd currentDateString
                )

        RequestNotes dateIsoString ->
            ( { model | notes = RemoteData.Loading }
            , Cmd.batch [ requestNotesCmd model.host dateIsoString ]
            )

        RequestTodaysNotes ->
            ( { model | notes = RemoteData.Loading }
            , requestTodaysNotesCmd model.host
            )

        ToggleHearingDropdown caseNumber state ->
            let
                hearingsDropdownStates =
                    toggleDropdownForCaseNumber model caseNumber state
            in
                ( { model | hearingsDropdownStates = hearingsDropdownStates }, Cmd.none )

        ToggleDepartmentDropdown state ->
            ( { model | departmentDropdownState = state }
            , Cmd.none
            )

        ToggleStatusDropdown state ->
            ( { model | statusDropdownState = state }
            , Cmd.none
            )

        UpdateCurrentDateString currentDateString ->
            ( { model | currentDateString = currentDateString }, Cmd.none )

        UpdateNoteBoxValue hearing value ->
            let
                noteBoxValues =
                    model.noteBoxValues
                        |> Dict.update hearing.caseNumber (\_ -> Just value)
            in
                ( { model | noteBoxValues = noteBoxValues }, Cmd.none )

        UpdateSearchBoxValue string ->
            ( { model | searchBoxValue = string }, Cmd.none )



---- HELPERS ----


decodeUser : Json.Decode.Value -> Maybe Msal.User
decodeUser value =
    value
        |> Json.Decode.decodeValue Msal.decodeUser
        |> Result.toMaybe


toggleDropdownForCaseNumber : Model -> CaseNumber -> Dropdown.State -> Dict CaseNumber Dropdown.State
toggleDropdownForCaseNumber model caseNumber state =
    model.hearingsDropdownStates
        |> Dict.update caseNumber (\_ -> Just state)


postEventCmd : String -> Event -> Cmd Msg
postEventCmd host event =
    TriageData.postEventRequest host event
        |> Http.send OnEventSave


postNoteCmd : String -> Note -> Cmd Msg
postNoteCmd host note =
    TriageData.postNoteRequest host note
        |> Http.send OnNoteSave


notesForHearing : WebData (List Note) -> Hearing -> WebData (List Note)
notesForHearing notes hearing =
    RemoteData.map (List.filter (\n -> n.matterId == hearing.caseId)) notes


lastEventForHearing : Hearing -> List Event -> Maybe Event
lastEventForHearing hearing events =
    events
        |> List.filter (\e -> e.matterId == hearing.caseId)
        |> List.Extra.last


maybeLastEventForHearing : WebData (List Event) -> Hearing -> Maybe Event
maybeLastEventForHearing events hearing =
    RemoteData.map (lastEventForHearing hearing) events
        |> RemoteData.toMaybe
        |> Maybe.Extra.join


dropdownStateForCaseNumber : Model -> CaseNumber -> Dropdown.State
dropdownStateForCaseNumber model caseNumber =
    model.hearingsDropdownStates
        |> Dict.get caseNumber
        |> Maybe.withDefault Dropdown.initialState


dateToDateString : Date.Date -> String
dateToDateString date =
    Moment.format
        [ Moment.YearNumberCapped
        , Moment.MonthFixed
        , Moment.DayOfMonthFixed
        ]
        date


receiveHearings : Dict comparable a -> comparable -> a -> Dict comparable a
receiveHearings hearings department response =
    hearings
        |> Dict.update department (\_ -> Just response)


requestEventsCmd : String -> String -> Cmd Msg
requestEventsCmd host dateString =
    TriageData.decodeEvents
        |> Http.get (TriageData.getEventsUrl host ++ dateString)
        |> RemoteData.sendRequest
        |> Cmd.map ReceiveEvents


requestTodaysEventsCmd : String -> Cmd Msg
requestTodaysEventsCmd host =
    TriageData.decodeEvents
        |> Http.get (TriageData.getEventsUrl host)
        |> RemoteData.sendRequest
        |> Cmd.map ReceiveEvents


requestNotesCmd : String -> String -> Cmd Msg
requestNotesCmd host dateString =
    TriageData.decodeNotes
        |> Http.get (TriageData.getNotesUrl host ++ dateString)
        |> RemoteData.sendRequest
        |> Cmd.map ReceiveNotes


requestTodaysNotesCmd : String -> Cmd Msg
requestTodaysNotesCmd host =
    TriageData.decodeNotes
        |> Http.get (TriageData.getNotesUrl host)
        |> RemoteData.sendRequest
        |> Cmd.map ReceiveNotes


saveEventRequest : String -> Event -> Http.Request Event
saveEventRequest host event =
    Http.request
        { body = TriageData.encodeEvent event |> Http.jsonBody
        , expect = Http.expectJson TriageData.decodeEvent
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = TriageData.postEventUrl host
        , withCredentials = False
        }


addEventCmd : Model -> Hearing -> Disposition.Action -> Cmd Msg
addEventCmd model hearing action =
    case model.user of
        Nothing ->
            Cmd.none

        Just user ->
            let
                event =
                    (Disposition.createEventFromAction user hearing action)
            in
                saveEventRequest model.host event
                    |> Http.send OnEventSave


addNoteCmd : Model -> Hearing -> Cmd Msg
addNoteCmd model hearing =
    case model.user of
        Nothing ->
            Cmd.none

        Just user ->
            let
                note =
                    Note
                        hearing.caseId
                        "subject"
                        (model.noteBoxValues
                            |> Dict.get hearing.caseNumber
                            |> Maybe.withDefault ""
                        )
                        user.id
                        Nothing
            in
                postNoteCmd model.host note


requestHearingsCmd : DateString -> Cmd Msg
requestHearingsCmd currentDateString =
    let
        departments =
            [ "F201", "F301", "F401", "F402", "F501", "F502" ]
    in
        List.map (\d -> requestHearingsForDepartmentCmd ( currentDateString, d )) departments
            |> Cmd.batch


requestHearingsForDepartmentCmd : ( DateString, Department ) -> Cmd Msg
requestHearingsForDepartmentCmd ( date, department ) =
    CaseManagementData.hearingsDecoder
        |> Http.get (CaseManagementData.hearingsUrl ( date, department ))
        |> RemoteData.sendRequest
        |> Cmd.map (ReceiveHearings department)


filterTriageHearings : List Hearing -> List Hearing
filterTriageHearings =
    List.filter (\h -> String.endsWith "T08:15:00" h.scheduledEventDateTime)
        >> filterUniqueCaseNumberHearings


filterRfoHearings : List Hearing -> List Hearing
filterRfoHearings hearings =
    List.filter (\h -> String.startsWith "HREO" h.scheduledEventType) hearings


filterCustodyVisitationSupport : List Hearing -> List Hearing
filterCustodyVisitationSupport hearings =
    List.filter hasCustodyVisitationOrSupport hearings


hasCustodyVisitationOrSupport : Hearing -> Bool
hasCustodyVisitationOrSupport { scheduledEventName } =
    let
        hasCust =
            scheduledEventName
                |> String.toUpper
                |> String.contains "CUST"

        hasVisit =
            scheduledEventName
                |> String.toUpper
                |> String.contains "VISIT"

        hasSupp =
            scheduledEventName
                |> String.toUpper
                |> String.contains "SUPP"
    in
        hasCust || hasVisit || hasSupp


filterNotModHearings : List Hearing -> List Hearing
filterNotModHearings hearings =
    hearings
        |> List.Extra.filterNot (.scheduledEventName >> String.toUpper >> String.contains "CHANGE")
        |> List.Extra.filterNot (.scheduledEventName >> String.toUpper >> String.contains "MODIFICATION")


filterUniqueCaseNumberHearings : List Hearing -> List Hearing
filterUniqueCaseNumberHearings hearings =
    List.Extra.uniqueBy (.caseNumber) hearings


filterNotDualRepresentedHearings : List Hearing -> List Hearing
filterNotDualRepresentedHearings hearings =
    hearings


filterByDepartment : DepartmentFilter -> Dict String HearingResponse -> Dict String HearingResponse
filterByDepartment filter hearings =
    case filter of
        All ->
            hearings

        _ ->
            Dict.filter (\d _ -> d == (toString filter)) hearings


isFinalEvent : Event -> Bool
isFinalEvent event =
    event.category == "Disposition" && event.subject == "Triage"


isPendingEvent : Event -> Bool
isPendingEvent event =
    not (isFinalEvent event)


isCCRCEvent : Event -> Bool
isCCRCEvent event =
    event
        |> toString
        |> String.contains "CCRC"


isDCSSEvent : Event -> Bool
isDCSSEvent event =
    event
        |> toString
        |> String.contains "DCSS"


isTriageEvent : Event -> Bool
isTriageEvent event =
    event.category == "Transition" && event.subject == "Triage"


isIneligible : Event -> Bool
isIneligible event =
    event.action
        |> String.contains "Ineligible"


isEligible : Event -> Bool
isEligible event =
    not (isIneligible event)


filterByLastEvent : (Event -> Bool) -> Model -> List Hearing -> List Hearing
filterByLastEvent test model hearings =
    List.filter
        (maybeLastEventForHearing model.events
            >> Maybe.map test
            >> Maybe.withDefault False
        )
        hearings


filterByStatus : Model -> List Hearing -> List Hearing
filterByStatus model hearings =
    let
        filter =
            model.statusFilter

        events =
            model.events
    in
        case filter of
            Any ->
                hearings

            Pending ->
                filterByLastEvent isPendingEvent model hearings

            CCRC ->
                filterByLastEvent isCCRCEvent model hearings

            DCSS ->
                filterByLastEvent isDCSSEvent model hearings

            Triage ->
                filterByLastEvent isTriageEvent model hearings

            Eligible ->
                filterByLastEvent isEligible model hearings



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        hearings =
            model.hearings
    in
        div []
            [ navbar model
            , div [ attribute "role" "main", class "container" ]
                [ div [ class "starter-template" ]
                    [ h1 [] [ text "Riverside Superior Court Triage" ]
                    ]
                , authMessage model
                , hearingsGrid model
                ]
            ]


authMessage : Model -> Html Msg
authMessage model =
    case model.user of
        Nothing ->
            p [ class "text-primary" ] [ text "To update cases, login with your court ID, e.g., 'jsmith@riverside.courts.ca.gov'" ]

        Just user ->
            p [ class "text-primary" ] [ text ("Welcome " ++ user.givenName) ]


navbar : Model -> Html Msg
navbar model =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.dark
        |> Navbar.brand [ href "#" ] [ text "Triage" ]
        |> Navbar.customItems
            [ dateSearchForm model, navAuthButton model ]
        |> Navbar.view model.navbarState


navAuthButton : Model -> Navbar.CustomItem Msg
navAuthButton model =
    let
        button =
            case model.user of
                Nothing ->
                    Button.button
                        [ Button.primary
                        , Button.attrs [ Spacing.ml5Sm ]
                        , Button.onClick (Login (Json.Encode.null))
                        ]
                        [ text "Login" ]

                Just user ->
                    Button.button
                        [ Button.info
                        , Button.attrs [ Spacing.ml5Sm ]
                        , Button.onClick (Logout (Json.Encode.null))
                        ]
                        [ text "Logout" ]
    in
        Navbar.formItem [] [ button ]


dateSearchForm : Model -> Navbar.CustomItem Msg
dateSearchForm model =
    Navbar.formItem []
        [ Input.text
            [ Input.attrs [ placeholder ("Date format: " ++ model.currentDateString), value model.searchBoxValue ]
            , Input.onInput UpdateSearchBoxValue
            ]
        , Button.button
            [ Button.outlineLight
            , Button.attrs [ Spacing.ml2Sm ]
            , Button.onClick RequestHearings
            ]
            [ text "Get date" ]
        ]


hearingsGrid : Model -> Html Msg
hearingsGrid model =
    let
        hearingsList =
            model.hearings
                |> filterByDepartment model.departmentFilter
                |> Dict.toList
    in
        Grid.container []
            ([ Grid.row
                [ Row.topLg, Row.attrs [ class "p-2" ] ]
                [ Grid.col [ Col.attrs [ class "lead text-info" ] ]
                    [ text
                        ("Triage for "
                            ++ model.currentDateString
                            ++ ": "
                            ++ (toString <| countHearings model.hearings)
                            ++ " cases."
                        )
                    ]
                , Grid.colBreak []
                , Grid.col [ Col.xs1, Col.textAlign Text.alignXsLeft ] [ departmentDropdown model ]
                , Grid.col [ Col.xs2, Col.middleXs ] [ text "Case Number" ]
                , Grid.col [ Col.xs1, Col.middleXs ] [ text "Interp" ]
                , Grid.col [ Col.xs3, Col.middleXs ] [ text "Petitioner" ]
                , Grid.col [ Col.xs3, Col.middleXs ] [ text "Respondent" ]
                , Grid.col [ Col.xs1, Col.middleXs ] [ statusDropdown model ]
                ]
             ]
                ++ List.concatMap (rowForDepartmentHearings model) hearingsList
            )


rowForDepartmentHearings : Model -> ( Department, HearingResponse ) -> List (Html Msg)
rowForDepartmentHearings model ( department, hearingResponse ) =
    case hearingResponse of
        RemoteData.NotAsked ->
            [ Grid.row [ Row.middleLg ]
                [ Grid.col [] [ text "Pending" ] ]
            ]

        RemoteData.Loading ->
            [ Grid.row [ Row.middleLg ]
                [ Grid.col [] [ text "Loading" ] ]
            ]

        RemoteData.Success hearings ->
            hearingRows model ( department, hearings )

        RemoteData.Failure e ->
            [ Grid.row [ Row.middleLg ]
                [ Grid.col [] [ text <| (department) ++ ": " ++ (toString e) ]
                ]
            ]


hearingRows : Model -> ( Department, List Hearing ) -> List (Html Msg)
hearingRows model ( department, hearings ) =
    case hearings of
        [] ->
            [ Grid.row [ Row.middleLg ]
                [ Grid.col [ Col.attrs [ class "text-muted" ] ]
                    [ text (department ++ " has no RFOs on calendar.") ]
                ]
            ]

        _ ->
            List.map (hearingRow model) (hearings |> filterByStatus model)


hearingRow : Model -> Hearing -> Html Msg
hearingRow model hearing =
    Grid.row [ Row.topLg, Row.attrs [ class "p-4" ] ]
        [ Grid.col [ Col.xs1 ] [ text hearing.department ]
        , Grid.col [ Col.xs2 ]
            [ p [] [ text hearing.caseNumber ]
            ]
        , interpreterCol hearing.interpreter
        , petitionerCol hearing.parties
        , respondentCol hearing.parties
        , Grid.col [] [ hearingDropdown model hearing ]
        , Grid.colBreak []
        , notesCol model hearing
        , eventsCol model hearing
        ]


eventsForHearing : WebData (List Event) -> Hearing -> WebData (List Event)
eventsForHearing events hearing =
    events
        |> RemoteData.map
            (List.filter (\e -> e.matterId == hearing.caseId))


eventsCol : Model -> Hearing -> Grid.Column msg
eventsCol model hearing =
    let
        events =
            eventsForHearing model.events hearing
    in
        case events of
            RemoteData.Success events ->
                Grid.col [ Col.xs6 ]
                    [ ol [ class "text-muted" ]
                        (eventsList events)
                    ]

            RemoteData.Loading ->
                Grid.col [] [ text "Loading events" ]

            RemoteData.Failure e ->
                Grid.col [] [ text (toString e) ]

            RemoteData.NotAsked ->
                Grid.col [] [ text "Loading" ]


eventsList : List Event -> List (Html msg)
eventsList events =
    events
        |> List.map (\event -> li [] [ text (eventsColText event) ])


eventsColText : Event -> String
eventsColText event =
    event
        |> Disposition.createActionFromEvent
        |> Disposition.actionToString


notesCol : Model -> Hearing -> Grid.Column Msg
notesCol model hearing =
    let
        notes =
            notesForHearing model.notes hearing
    in
        case notes of
            RemoteData.Success data ->
                Grid.col [ Col.xs6 ]
                    [ ul [ class "text-muted" ]
                        (List.map (\note -> li [] [ text (notesColText note) ]) data)
                    , (notesForm model hearing)
                    ]

            RemoteData.Failure err ->
                Grid.col [ Col.xs6 ]
                    [ text (toString err) ]

            _ ->
                Grid.col [ Col.xs6 ]
                    [ text "Loading/NotAsked" ]


notesColText : Note -> String
notesColText note =
    note.body


notesForm : Model -> Hearing -> Html Msg
notesForm model hearing =
    let
        noteBoxValues =
            model.noteBoxValues

        caseNumber =
            hearing.caseNumber
    in
        case model.user of
            Nothing ->
                div [] []

            Just user ->
                Form.formInline []
                    [ Input.text
                        [ Input.attrs
                            [ placeholder "Write a note"
                            , value (noteBoxValueForCaseNumber noteBoxValues caseNumber)
                            ]
                        , Input.onInput (UpdateNoteBoxValue hearing)
                        ]
                    , Button.button
                        [ Button.outlineLight
                        , Button.attrs [ Spacing.ml2Sm ]
                        , Button.onClick (AddNote hearing)
                        ]
                        [ text "Enter" ]
                    ]


noteBoxValueForCaseNumber : Dict CaseNumber String -> CaseNumber -> String
noteBoxValueForCaseNumber noteBoxValues caseNumber =
    noteBoxValues
        |> Dict.get caseNumber
        |> Maybe.withDefault ""


interpreterCol : Maybe (List Interpreter) -> Grid.Column msg
interpreterCol interpreter =
    let
        col c t =
            Grid.col [ Col.xs1, Col.attrs [ class c ] ] [ text t ]
    in
        case interpreter of
            Nothing ->
                col "text-muted" "None"

            Just interpreters ->
                col "text-info" (interpreters |> interpreterLanguages |> String.left 8)


interpreterLanguages : List { a | language : String } -> String
interpreterLanguages interpreters =
    (interpreters
        |> List.map (\i -> i.language)
        |> String.join ", "
    )


petitionerCol : List Party -> Grid.Column msg
petitionerCol parties =
    let
        maybePetitioner_ =
            maybePetitioner parties

        name =
            Maybe.map firstAndLastName maybePetitioner_
                |> Maybe.withDefault "Person Doe"

        hasAttorney_ =
            Maybe.map hasAttorney maybePetitioner_

        class_ =
            case (hasAttorney_) of
                Just True ->
                    "text-warning"

                _ ->
                    "text-dark"
    in
        Grid.col [ Col.attrs [ class class_ ], Col.xs3 ] [ text name ]


respondentCol : List Party -> Grid.Column msg
respondentCol parties =
    let
        maybeRespondent_ =
            maybeRespondent parties

        name =
            Maybe.map firstAndLastName maybeRespondent_
                |> Maybe.withDefault "Person Doe"

        hasAttorney_ =
            Maybe.map hasAttorney maybeRespondent_

        class_ =
            case (hasAttorney_) of
                Just True ->
                    "text-warning"

                _ ->
                    "text-dark"
    in
        Grid.col [ Col.attrs [ class class_ ], Col.xs3 ] [ text name ]


maybePetitioner : List Party -> Maybe Party
maybePetitioner parties =
    parties
        |> filterParty [ "Other Parent", "Mother", "Wife", "Protected Person", "Petitioner" ]
        |> filterNotCountyOfRiverside
        |> List.head


filterNotCountyOfRiverside : List Party -> List Party
filterNotCountyOfRiverside parties =
    parties
        |> List.Extra.filterNot
            (\{ organizationName } ->
                organizationName
                    |> Maybe.withDefault ""
                    |> String.toUpper
                    |> String.contains "COUNTY OF RIVERSIDE"
            )


maybeRespondent : List Party -> Maybe Party
maybeRespondent parties =
    parties
        |> filterParty [ "Respondent", "Father", "Husband", "Restrained Person" ]
        |> List.head


filterParty : List String -> List Party -> List Party
filterParty partyList parties =
    parties
        |> List.filter (\{ partyType } -> List.member partyType partyList)
        |> List.Extra.uniqueBy (\{ id } -> id)


hasAttorney : Party -> Bool
hasAttorney party =
    let
        name =
            party

        attorneys =
            party.attorneys
    in
        case attorneys of
            Nothing ->
                False

            _ ->
                True


firstAndLastName : Party -> String
firstAndLastName { firstName, lastName } =
    Just (String.join " ")
        |> Maybe.Extra.andMap (Maybe.Extra.combine [ firstName, lastName ])
        |> Maybe.withDefault "Name not found"


hearingDropdown : Model -> Hearing -> Html Msg
hearingDropdown model hearing =
    case model.user of
        Nothing ->
            div [] []

        _ ->
            Dropdown.dropdown
                (dropdownStateForCaseNumber model hearing.caseNumber)
                { options = []
                , toggleMsg = ToggleHearingDropdown hearing.caseNumber
                , toggleButton =
                    Dropdown.toggle [ Button.light ] [ text "Action" ]
                , items =
                    List.map
                        (hearingDropdownItems model hearing)
                        (availableActions model hearing)
                }


availableActions : Model -> Hearing -> List Disposition.Action
availableActions model hearing =
    Disposition.availableActions (hearingDisposition model hearing)


hearingDisposition : Model -> Hearing -> Disposition.State
hearingDisposition model hearing =
    maybeLastEventForHearing model.events hearing
        |> Maybe.map
            (Disposition.createActionFromEvent
                >> Disposition.createStateFromAction
            )
        |> Maybe.withDefault Disposition.Initial


hearingDropdownItems : Model -> Hearing -> Disposition.Action -> Dropdown.DropdownItem Msg
hearingDropdownItems { user } hearing action =
    Dropdown.buttonItem
        [ onClick (AddEvent hearing action) ]
        [ text (Disposition.actionToString action) ]


departmentDropdown : Model -> Html Msg
departmentDropdown model =
    let
        label =
            toString (model.departmentFilter)
    in
        Dropdown.dropdown
            model.departmentDropdownState
            { options = []
            , toggleMsg = ToggleDepartmentDropdown
            , toggleButton = Dropdown.toggle [ Button.light ] [ text label ]
            , items =
                List.map
                    (\d ->
                        Dropdown.buttonItem [ onClick (FilterDepartment d) ] [ text (toString d) ]
                    )
                    model.departmentFilters
            }


statusDropdown : Model -> Html Msg
statusDropdown model =
    Dropdown.dropdown
        model.statusDropdownState
        { options = []
        , toggleMsg = ToggleStatusDropdown
        , toggleButton =
            Dropdown.toggle [ Button.light ] [ text (toString model.statusFilter) ]
        , items = statusDropdownItems model.statusFilters
        }


statusDropdownItems : List StatusFilter -> List (Dropdown.DropdownItem Msg)
statusDropdownItems statusFilters =
    statusFilters
        |> List.map
            (\filter ->
                Dropdown.buttonItem
                    [ onClick (FilterStatus filter) ]
                    [ text (toString filter) ]
            )



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (hearingDropdownSubscriptions model
            ++ [ Navbar.subscriptions model.navbarState NavbarMsg
               , Dropdown.subscriptions model.departmentDropdownState ToggleDepartmentDropdown
               , Dropdown.subscriptions model.statusDropdownState ToggleStatusDropdown
               , Ports.loginResult LoginResult
               , WebSocket.listen (TriageData.feedUrl model.host) NewFeed
               ]
        )


hearingDropdownSubscriptions : Model -> List (Sub Msg)
hearingDropdownSubscriptions model =
    model.hearingsDropdownStates
        |> Dict.toList
        |> List.map
            (\( caseNumber, state ) ->
                Dropdown.subscriptions state (ToggleHearingDropdown caseNumber)
            )
