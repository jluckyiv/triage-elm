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
    , notes : WebData (List Note)
    , searchBoxValue : String
    , statusDropdownState : Dropdown.State
    , user : Maybe Msal.User
    , userLoginStatus : String
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


init : ( Model, Cmd Msg )
init =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
        ( { currentDateString = ""
          , departmentDropdownState = Dropdown.initialState
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
          , notes = RemoteData.Loading
          , searchBoxValue = ""
          , statusDropdownState = Dropdown.initialState
          , user = Nothing
          , userLoginStatus = "Login"
          }
        , Cmd.batch
            [ navbarCmd, Task.perform ReceiveDate Date.now, requestTodaysEvents, requestTodaysNotes ]
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
        |> RemoteData.fromList
        |> RemoteData.map (List.concat)
        |> RemoteData.withDefault []
        |> List.map (\h -> ( h.caseNumber, Dropdown.initialState ))
        |> Dict.fromList


initNoteBoxValues : Dict Department HearingResponse -> Dict CaseNumber String
initNoteBoxValues departmentHearings =
    departmentHearings
        |> Dict.values
        |> RemoteData.fromList
        |> RemoteData.map (List.concat)
        |> RemoteData.withDefault []
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
    | NewEvents String
    | NewFeed String
    | NewNotes String
    | OnEventSave (Result Http.Error Event)
    | OnNoteSave (Result Http.Error Note)
    | ReceiveDate Date.Date
    | ReceiveEvents (WebData (List Event))
    | ReceiveHearings Department HearingResponse
    | ReceiveNotes (WebData (List Note))
    | RequestEvents String
    | RequestTodaysEvents
    | RequestNotes String
    | RequestTodaysNotes
    | RequestHearings
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

        AddEvent hearing event ->
            case model.user of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( model, saveEventCmd (Disposition.createEventFromAction user hearing event) )

        AddNote hearing ->
            let
                user_ =
                    model.user |> Maybe.map .id |> Maybe.withDefault "0"

                note =
                    Note
                        hearing.caseId
                        "subject"
                        (model.noteBoxValues
                            |> Dict.get hearing.caseNumber
                            |> Maybe.withDefault ""
                        )
                        user_
                        Nothing

                noteBoxValues =
                    model.noteBoxValues
                        |> Dict.update hearing.caseNumber (\_ -> Just "")
            in
                ( { model
                    | noteBoxValues = noteBoxValues

                    {- TODO: update UI with "Saving" -}
                  }
                , postNoteCmd note
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
            ( { model | userLoginStatus = "Logging in" }, Msal.login value )

        LoginResult value ->
            let
                user =
                    (Result.toMaybe (Json.Decode.decodeValue Msal.decodeUser value))
            in
                ( { model | user = user, userLoginStatus = "Logout" }, Cmd.none )

        Logout value ->
            ( { model | user = Nothing, userLoginStatus = "Login" }, Msal.logout value )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        NewEvents json ->
            let
                events =
                    Json.Decode.decodeString TriageData.decodeEvents json
            in
                case events of
                    Ok events ->
                        ( { model | events = RemoteData.Success events }, Cmd.none )

                    Err error ->
                        let
                            _ =
                                Debug.log "error" error
                        in
                            ( model, Cmd.none )

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
                                Debug.log "error" error
                        in
                            ( model, Cmd.none )

        NewNotes json ->
            let
                notes =
                    Json.Decode.decodeString TriageData.decodeNotes json
            in
                case notes of
                    Ok notes ->
                        ( { model | notes = RemoteData.Success notes }, Cmd.none )

                    Err error ->
                        let
                            _ =
                                Debug.log "error" error
                        in
                            ( model, Cmd.none )

        OnEventSave (Ok event) ->
            Debug.log "Ok" ( model, Cmd.none )

        OnEventSave (Err error) ->
            Debug.log "Err" ( model, Cmd.none )

        OnNoteSave (Ok event) ->
            Debug.log "Ok" ( model, Cmd.none )

        OnNoteSave (Err error) ->
            Debug.log "Err" ( model, Cmd.none )

        ReceiveDate date ->
            let
                currentDateString =
                    dateToDateString date
            in
                ( { model | currentDateString = currentDateString }
                , Cmd.batch <| requestHearings currentDateString
                )

        ReceiveEvents response ->
            ( { model | events = response }, Cmd.none )

        ReceiveHearings department response ->
            let
                hearings =
                    (receiveHearings model department response)
                        |> Dict.map (\_ hs -> RemoteData.map filterTriageHearings hs)
            in
                ( { model
                    | hearings = hearings
                    , hearingsDropdownStates = initHearingDropdownStates hearings
                    , noteBoxValues = initNoteBoxValues hearings
                  }
                , Cmd.none
                )

        ReceiveNotes response ->
            ( { model | notes = response }, Cmd.none )

        RequestEvents dateIsoString ->
            ( { model | events = RemoteData.Loading }, Cmd.batch [ requestEvents dateIsoString ] )

        RequestTodaysEvents ->
            ( { model | events = RemoteData.Loading }, requestTodaysEvents )

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
                , Cmd.batch
                    (requestHearings currentDateString)
                )

        RequestNotes dateIsoString ->
            ( { model | notes = RemoteData.Loading }, Cmd.batch [ requestNotes dateIsoString ] )

        RequestTodaysNotes ->
            ( { model | notes = RemoteData.Loading }, requestTodaysNotes )

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


postEventCmd : Event -> Cmd Msg
postEventCmd event =
    TriageData.postEventRequest event
        |> Http.send OnEventSave


postNoteCmd : Note -> Cmd Msg
postNoteCmd note =
    TriageData.postNoteRequest note
        |> Http.send OnNoteSave


toggleDropdownForCaseNumber : Model -> CaseNumber -> Dropdown.State -> Dict CaseNumber Dropdown.State
toggleDropdownForCaseNumber model caseNumber state =
    model.hearingsDropdownStates
        |> Dict.update caseNumber (\_ -> Just state)


notesForHearing : Model -> Hearing -> WebData (List Note)
notesForHearing model hearing =
    RemoteData.map (List.filter (\n -> n.matterId == hearing.caseId)) model.notes


maybeLastEventForHearing : Model -> Hearing -> Maybe Event
maybeLastEventForHearing model hearing =
    RemoteData.map
        (List.filter
            (\e -> e.matterId == hearing.caseId)
            >> List.Extra.last
        )
        model.events
        |> RemoteData.toMaybe
        |> Maybe.Extra.join


dropdownStateForCaseNumber : Model -> CaseNumber -> Dropdown.State
dropdownStateForCaseNumber model caseNumber =
    model.hearingsDropdownStates
        |> Dict.get caseNumber
        |> Maybe.withDefault Dropdown.initialState


dateToDateString : Date.Date -> String
dateToDateString date =
    Moment.format [ Moment.YearNumberCapped, Moment.MonthFixed, Moment.DayOfMonthFixed ] date


receiveHearings : Model -> Department -> WebData (List Hearing) -> Dict Department HearingResponse
receiveHearings model department response =
    model.hearings
        |> Dict.update department (\_ -> Just response)


requestEvents : String -> Cmd Msg
requestEvents dateString =
    TriageData.decodeEvents
        |> Http.get (TriageData.getEventsUrl ++ dateString)
        |> RemoteData.sendRequest
        |> Cmd.map ReceiveEvents


requestTodaysEvents : Cmd Msg
requestTodaysEvents =
    TriageData.decodeEvents
        |> Http.get TriageData.getEventsUrl
        |> RemoteData.sendRequest
        |> Cmd.map ReceiveEvents


requestNotes : String -> Cmd Msg
requestNotes dateString =
    TriageData.decodeNotes
        |> Http.get (TriageData.getNotesUrl ++ dateString)
        |> RemoteData.sendRequest
        |> Cmd.map ReceiveNotes


requestTodaysNotes : Cmd Msg
requestTodaysNotes =
    TriageData.decodeNotes
        |> Http.get TriageData.getNotesUrl
        |> RemoteData.sendRequest
        |> Cmd.map ReceiveNotes


saveEventRequest : Event -> Http.Request Event
saveEventRequest event =
    Http.request
        { body = TriageData.encodeEvent event |> Http.jsonBody
        , expect = Http.expectJson TriageData.decodeEvent
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = TriageData.postEventUrl
        , withCredentials = False
        }


saveEventCmd : Event -> Cmd Msg
saveEventCmd event =
    saveEventRequest event
        |> Http.send OnEventSave


requestHearings : DateString -> List (Cmd Msg)
requestHearings currentDateString =
    let
        departments =
            [ "F201", "F301", "F401", "F402", "F501", "F502" ]
    in
        List.map (\d -> requestHearingsForDepartment ( currentDateString, d )) departments


requestHearingsForDepartment : ( DateString, Department ) -> Cmd Msg
requestHearingsForDepartment ( date, department ) =
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
            scheduledEventName |> String.toUpper |> String.contains "CUST"

        hasVisit =
            scheduledEventName |> String.toUpper |> String.contains "VISIT"

        hasSupp =
            scheduledEventName |> String.toUpper |> String.contains "SUPP"
    in
        hasCust || hasVisit || hasSupp


filterNotModHearings : List Hearing -> List Hearing
filterNotModHearings hearings =
    hearings
        |> List.Extra.filterNot (\h -> h.scheduledEventName |> String.toUpper |> String.contains "CHANGE")
        |> List.Extra.filterNot (\h -> h.scheduledEventName |> String.toUpper |> String.contains "MODIFICATION")


filterUniqueCaseNumberHearings : List Hearing -> List Hearing
filterUniqueCaseNumberHearings hearings =
    List.Extra.uniqueBy (\h -> h.caseNumber) hearings


filterNotDualRepresentedHearings : List Hearing -> List Hearing
filterNotDualRepresentedHearings hearings =
    hearings


filterByDepartment : Model -> Dict String HearingResponse -> Dict String HearingResponse
filterByDepartment model hearings =
    let
        filter =
            model.departmentFilter
    in
        case filter of
            All ->
                hearings

            _ ->
                hearings
                    |> Dict.filter
                        (\department _ ->
                            department == (toString filter)
                        )


isTriageDisposition : TriageData.Event -> Bool
isTriageDisposition event =
    event.category == "Transition" && event.subject == "Triage"


filterByStatus : Model -> List Hearing -> List Hearing
filterByStatus model hearings =
    let
        filter =
            model.statusFilter
    in
        case filter of
            Any ->
                hearings

            Pending ->
                hearings
                    |> List.filter
                        (\hearing ->
                            hearing
                                |> maybeLastEventForHearing model
                                |> Maybe.map
                                    (\event ->
                                        event.category /= "Disposition" && event.subject /= "Triage"
                                    )
                                |> Maybe.withDefault True
                        )

            CCRC ->
                hearings
                    |> List.filter
                        (\hearing ->
                            hearing
                                |> maybeLastEventForHearing model
                                |> Maybe.map
                                    (\event ->
                                        event
                                            |> toString
                                            |> String.contains "CCRC"
                                    )
                                |> Maybe.withDefault False
                        )

            DCSS ->
                hearings
                    |> List.filter
                        (\hearing ->
                            hearing
                                |> maybeLastEventForHearing model
                                |> Maybe.map
                                    (\event ->
                                        event
                                            |> toString
                                            |> String.contains "DCSS"
                                    )
                                |> Maybe.withDefault False
                        )

            Triage ->
                hearings
                    |> List.filter
                        (\hearing ->
                            hearing
                                |> maybeLastEventForHearing model
                                |> Maybe.map
                                    (\event ->
                                        event.category /= "Disposition" && event.subject == "Triage"
                                    )
                                |> Maybe.withDefault False
                        )

            Eligible ->
                hearings
                    |> List.filter
                        (\hearing ->
                            hearing
                                |> maybeLastEventForHearing model
                                |> Maybe.map
                                    (\event ->
                                        not (String.contains "Ineligible" event.action)
                                    )
                                |> Maybe.withDefault False
                        )



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
                , authButton model
                , hearingsGrid model
                ]
            ]


authButton : Model -> Html Msg
authButton model =
    case model.user of
        Nothing ->
            div []
                [ Button.button
                    [ Button.primary
                    , Button.onClick (Login (Json.Encode.null))
                    ]
                    [ text model.userLoginStatus ]
                , p [ class "text-primary" ] [ text "To update cases, use your court login ID, e.g., 'jsmith@riverside.courts.ca.gov'" ]
                ]

        Just user_ ->
            Button.button
                [ Button.primary
                , Button.onClick (Logout (Json.Encode.null))
                ]
                [ text (model.userLoginStatus ++ " " ++ user_.givenName) ]


navbar : Model -> Html Msg
navbar model =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.dark
        |> Navbar.brand [ href "#" ] [ text "Triage" ]
        -- |> Navbar.items
        --     [ Navbar.itemLink [ href "#home" ] [ text "Home" ]
        --     , Navbar.itemLink [ href "#link" ] [ text "Link" ]
        --     ]
        |> Navbar.customItems
            [ dateSearchForm model ]
        |> Navbar.view model.navbarState


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
                |> filterByDepartment model
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
        data =
            hearing
                |> notesForHearing model
    in
        case data of
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

            _ ->
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
    maybeLastEventForHearing model hearing
        |> Maybe.map
            (Disposition.createActionFromEvent
                >> Disposition.createStateFromAction
            )
        |> Maybe.withDefault Disposition.Initial


hearingDropdownItems : { b | user : a } -> Hearing -> Disposition.Action -> Dropdown.DropdownItem Msg
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


main : Program Never Model Msg
main =
    Html.program
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
               , Msal.loginResult LoginResult
               , WebSocket.listen TriageData.feedUrl NewFeed
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
