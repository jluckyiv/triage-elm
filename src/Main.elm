module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing
import CaseManagementData exposing (Hearing)
import Date
import Html exposing (Html, text, div, span, p, ul, li, h1)
import Html.Attributes exposing (class, id, src, type_, placeholder, href, attribute, value)
import Html.Events exposing (onClick)
import Http
import List.Extra
import Maybe.Extra
import Moment
import RemoteData exposing (WebData)
import Task


---- MODEL ----


type alias Model =
    { currentDateString : DateString
    , departmentDropdownState : Dropdown.State
    , departmentFilter : Department
    , departments : List Department
    , hearings : List ( DepartmentString, WebData (List Hearing) )
    , hearingsDropdownStates : List ( CaseNumber, Dropdown.State )
    , navbarState : Navbar.State
    , searchBoxValue : String
    , statusDropdownState : Dropdown.State
    }


type alias DateString =
    String


type alias CaseNumber =
    String


type Department
    = All
    | F201
    | F301
    | F401
    | F402
    | F501
    | F502


type alias DepartmentString =
    String


type alias FullName =
    String


type alias Language =
    String


hearingsUrl : ( DateString, Department ) -> String
hearingsUrl ( date, department ) =
    "https://cbmdev.riverside.courts.ca.gov/Hearing/FL/" ++ date ++ "/" ++ (toString department)


init : ( Model, Cmd Msg )
init =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
        ( { currentDateString = ""
          , departmentDropdownState = Dropdown.initialState
          , departmentFilter = All
          , departments = [ All, F201, F301, F401, F402, F501, F502 ]
          , hearings = initHearings
          , hearingsDropdownStates = []
          , navbarState = navbarState
          , searchBoxValue = ""
          , statusDropdownState = Dropdown.initialState
          }
        , Cmd.batch [ navbarCmd, Task.perform ReceiveDate Date.now ]
        )


initHearings : List ( DepartmentString, WebData (List Hearing) )
initHearings =
    [ ( toString F201, RemoteData.Loading )
    , ( toString F301, RemoteData.Loading )
    , ( toString F401, RemoteData.Loading )
    , ( toString F402, RemoteData.Loading )
    , ( toString F501, RemoteData.Loading )
    , ( toString F502, RemoteData.Loading )
    ]


initHearingDropdownStates : List ( DepartmentString, WebData (List Hearing) ) -> List ( CaseNumber, Dropdown.State )
initHearingDropdownStates departmentHearings =
    departmentHearings
        |> List.map (Tuple.second)
        |> List.concatMap
            (\v ->
                case v of
                    RemoteData.Success hearings ->
                        hearings |> List.map (\hearing -> ( hearing.caseNumber, Dropdown.initialState ))

                    _ ->
                        []
            )



---- UPDATE ----


type Msg
    = NoOp
    | FilterDepartment Department
    | FilterStatus String
    | InitHearingDropdown
    | NavbarMsg Navbar.State
    | ReceiveDate Date.Date
    | ReceiveHearings Department (WebData (List Hearing))
    | RequestHearings
    | ToggleHearingDropdown String Dropdown.State
    | ToggleDepartmentDropdown Dropdown.State
    | ToggleStatusDropdown Dropdown.State
    | UpdateCurrentDateString DateString
    | UpdateSearchBoxValue String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InitHearingDropdown ->
            let
                hearingsDropdownStates =
                    initHearingDropdownStates model.hearings
            in
                ( { model | hearingsDropdownStates = hearingsDropdownStates }, Cmd.none )

        FilterDepartment department ->
            ( { model | departmentFilter = department }
            , Cmd.none
            )

        FilterStatus status ->
            let
                _ =
                    Debug.log "filter status" status
            in
                ( model, Cmd.none )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        ReceiveDate date ->
            let
                currentDateString =
                    dateToDateString date
            in
                ( { model | currentDateString = currentDateString }
                , Cmd.batch <| requestHearings currentDateString
                )

        ReceiveHearings department response ->
            let
                hearings =
                    receiveHearings model department response
            in
                ( { model
                    | hearings = hearings
                    , hearingsDropdownStates = initHearingDropdownStates hearings
                  }
                , Cmd.none
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
                , Cmd.batch
                    (requestHearings currentDateString)
                )

        ToggleHearingDropdown caseNumber state ->
            let
                hearingsDropdownStates =
                    mapHearingDropdownStates model caseNumber state
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

        UpdateSearchBoxValue string ->
            ( { model | searchBoxValue = string }, Cmd.none )



---- HELPERS ----


mapHearingDropdownStates : Model -> CaseNumber -> Dropdown.State -> List ( CaseNumber, Dropdown.State )
mapHearingDropdownStates model caseNumber state =
    model.hearingsDropdownStates
        |> List.map
            (\( cn, s ) ->
                if caseNumber == cn then
                    ( cn, state )
                else
                    ( cn, s )
            )


toggleHearingDropdown : Model -> Hearing -> Dropdown.State
toggleHearingDropdown model hearing =
    model.hearingsDropdownStates
        |> List.filter (\( caseNumber, _ ) -> caseNumber == hearing.caseNumber)
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault Dropdown.initialState


dateToDateString : Date.Date -> String
dateToDateString date =
    Moment.format [ Moment.YearNumberCapped, Moment.MonthFixed, Moment.DayOfMonthFixed ] date


receiveHearings : Model -> Department -> WebData (List Hearing) -> List ( DepartmentString, WebData (List Hearing) )
receiveHearings model department response =
    case department of
        All ->
            model.hearings

        _ ->
            updateHearings department response model.hearings


updateHearings : Department -> response -> List ( DepartmentString, response ) -> List ( DepartmentString, response )
updateHearings department response hearings =
    List.map
        (\( dept, hs ) ->
            if toString department == dept then
                ( dept, response )
            else
                ( dept, hs )
        )
        hearings


requestHearings : DateString -> List (Cmd Msg)
requestHearings currentDateString =
    [ requestDepartmentHearings ( currentDateString, F201 )
    , requestDepartmentHearings ( currentDateString, F301 )
    , requestDepartmentHearings ( currentDateString, F401 )
    , requestDepartmentHearings ( currentDateString, F402 )
    , requestDepartmentHearings ( currentDateString, F501 )
    , requestDepartmentHearings ( currentDateString, F502 )
    ]


requestDepartmentHearings : ( DateString, Department ) -> Cmd Msg
requestDepartmentHearings ( date, department ) =
    CaseManagementData.hearingsDecoder
        |> Http.get (hearingsUrl ( date, department ))
        |> RemoteData.sendRequest
        |> Cmd.map (ReceiveHearings department)



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
                    , p [ class "lead text-primary" ]
                        [ text ("Hearings for " ++ model.currentDateString)
                        ]
                    ]
                , hearingsGrid model
                ]
            ]


navbar : Model -> Html Msg
navbar model =
    Navbar.config NavbarMsg
        |> Navbar.withAnimation
        |> Navbar.dark
        |> Navbar.brand [ href "#" ] [ text "Triage" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#home" ] [ text "Home" ]
            , Navbar.itemLink [ href "#link" ] [ text "Link" ]
            ]
        |> Navbar.customItems
            [ dateSearchForm model ]
        |> Navbar.view model.navbarState


dateSearchForm : Model -> Navbar.CustomItem Msg
dateSearchForm model =
    Navbar.formItem []
        [ Input.text
            [ Input.attrs [ placeholder ("Ex: " ++ model.currentDateString), value model.searchBoxValue ]
            , Input.onInput UpdateSearchBoxValue
            ]
        , Button.button
            [ Button.outlineLight
            , Button.attrs [ Spacing.ml2Sm ]
            , Button.onClick RequestHearings
            ]
            [ text "Search" ]
        ]


hearingsGrid : Model -> Html Msg
hearingsGrid model =
    let
        hearings =
            case model.departmentFilter of
                All ->
                    model.hearings

                department ->
                    model.hearings
                        |> List.filter (\( d, _ ) -> d == toString department)
    in
        Grid.container []
            ([ Grid.row
                [ Row.topLg ]
                [ Grid.col [ Col.xs1, Col.textAlign Text.alignXsLeft ] [ departmentDropdown model ]
                , Grid.col [ Col.xs2, Col.middleXs ] [ text "Case Number" ]
                , Grid.col [ Col.xs1, Col.middleXs ] [ text "Interp" ]
                , Grid.col [ Col.xs3, Col.middleXs ] [ text "Petitioner" ]
                , Grid.col [ Col.xs3, Col.middleXs ] [ text "Respondent" ]
                , Grid.col [ Col.xs1, Col.middleXs ] [ statusDropdown model ]
                ]
             ]
                ++ List.concatMap (departmentRow model) hearings
            )


departmentRow : Model -> ( DepartmentString, WebData (List Hearing) ) -> List (Html Msg)
departmentRow model departmentHearings =
    case departmentHearings of
        ( departmentString, RemoteData.NotAsked ) ->
            [ Grid.row [ Row.middleLg ]
                [ Grid.col [] [ text "Pending" ] ]
            ]

        ( departmentString, RemoteData.Loading ) ->
            [ Grid.row [ Row.middleLg ]
                [ Grid.col [] [ text "Loading" ] ]
            ]

        ( departmentString, RemoteData.Success hearings ) ->
            hearingRows model hearings departmentString

        ( departmentString, RemoteData.Failure e ) ->
            [ Grid.row [ Row.middleLg ]
                [ Grid.col [] [ text <| departmentString ++ ": " ++ (toString e) ]
                ]
            ]


hearingRows : Model -> List Hearing -> DepartmentString -> List (Html Msg)
hearingRows model hearings departmentString =
    let
        hearingRows =
            hearings
                |> List.filter (\h -> String.startsWith "HREO" h.scheduledEventType)
                |> List.Extra.uniqueBy (\h -> h.caseNumber)
    in
        case hearingRows of
            [] ->
                [ Grid.row [ Row.middleLg ]
                    [ Grid.col [ Col.attrs [ class "text-muted" ] ]
                        [ text (departmentString ++ " has no RFOs on calendar.") ]
                    ]
                ]

            _ ->
                List.map (hearingRow model) hearingRows


hearingRow : Model -> CaseManagementData.Hearing -> Html Msg
hearingRow model hearing =
    Grid.row [ Row.middleLg ]
        [ Grid.col [ Col.xs1 ] [ text hearing.department ]
        , Grid.col [ Col.xs2 ] [ text hearing.caseNumber ]
        , interpreterCol hearing.interpreter
        , petitionerCol hearing.parties
        , respondentCol hearing.parties
        , Grid.col [] [ hearingDropdown model hearing ]
        , Grid.colBreak []
        , Grid.col [ Col.xs1 ] []
        , Grid.col [ Col.xs3 ] [ p [ class "text-muted" ] [ text "history here" ] ]
        , Grid.col [] [ p [ class "text-muted" ] [ text "notes here" ] ]
        ]


interpreterCol : Maybe (List CaseManagementData.Interpreter) -> Grid.Column msg
interpreterCol interpreter =
    let
        col c t =
            Grid.col [ Col.xs1, Col.attrs [ class c ] ] [ text t ]
    in
        case interpreter of
            Nothing ->
                col "text-muted" "None"

            Just interpreters ->
                col "text-primary" (interpreters |> interpreterLanguages |> String.left 8)


interpreterLanguages : List { a | language : String } -> String
interpreterLanguages interpreters =
    (interpreters
        |> List.map (\i -> i.language)
        |> String.join ", "
    )


petitionerCol : List CaseManagementData.Party -> Grid.Column msg
petitionerCol parties =
    let
        petitionerName =
            parties
                |> maybePetitioner
                |> firstAndLastName
    in
        Grid.col [ Col.xs3 ] [ text petitionerName ]


respondentCol : List CaseManagementData.Party -> Grid.Column msg
respondentCol parties =
    let
        respondentName =
            parties
                |> maybeRespondent
                |> firstAndLastName
    in
        Grid.col [ Col.xs3 ] [ text respondentName ]


maybePetitioner : List { a | id : comparable, organizationName : Maybe String, partyType : String } -> Maybe { a | id : comparable, partyType : String, organizationName : Maybe String }
maybePetitioner parties =
    parties
        |> filterParty [ "Other", "Mother", "Wife", "Protected Person", "Petitioner" ]
        |> filterNotCountyOfRiverside
        |> List.head


filterNotCountyOfRiverside : List { a | organizationName : Maybe String } -> List { a | organizationName : Maybe String }
filterNotCountyOfRiverside parties =
    parties
        |> List.Extra.filterNot
            (\{ organizationName } ->
                organizationName
                    |> Maybe.withDefault ""
                    |> String.toUpper
                    |> String.contains "COUNTY OF RIVERSIDE"
            )


maybeRespondent : List { a | id : comparable, partyType : String } -> Maybe { a | id : comparable, partyType : String }
maybeRespondent parties =
    parties
        |> filterParty [ "Respondent", "Father", "Husband", "Restrained Person" ]
        |> List.head


filterParty : List a -> List { b | id : comparable, partyType : a } -> List { b | id : comparable, partyType : a }
filterParty partyList parties =
    parties
        |> List.filter (\{ partyType } -> List.member partyType partyList)
        |> List.Extra.uniqueBy (\{ id } -> id)


firstAndLastName : Maybe { a | firstName : Maybe String, lastName : Maybe String } -> String
firstAndLastName maybeNames =
    maybeNames
        |> Maybe.map (\p -> [ p.firstName, p.lastName ])
        |> Maybe.map (Maybe.Extra.combine)
        |> Maybe.Extra.join
        |> Maybe.map (String.join " ")
        |> Maybe.withDefault "Person Doe"


hearingDropdown : Model -> Hearing -> Html Msg
hearingDropdown model hearing =
    Dropdown.dropdown
        (toggleHearingDropdown model hearing)
        { options = []
        , toggleMsg = ToggleHearingDropdown hearing.caseNumber
        , toggleButton =
            Dropdown.toggle [ Button.light ] [ text "Action" ]
        , items =
            [ Dropdown.buttonItem [ onClick (NoOp) ] [ text "Action1" ]
            , Dropdown.buttonItem [ onClick (NoOp) ] [ text "Action2" ]
            , Dropdown.buttonItem [ onClick (NoOp) ] [ text "Action3" ]
            , Dropdown.buttonItem [ onClick (NoOp) ] [ text "Action4" ]
            ]
        }


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
                List.map (\d -> Dropdown.buttonItem [ onClick (FilterDepartment d) ] [ text (toString d) ]) model.departments
            }


statusDropdown : Model -> Html Msg
statusDropdown model =
    -- needs dropdownstate, buttontext, buttoncolor, menuitems
    Dropdown.dropdown
        model.statusDropdownState
        { options = []
        , toggleMsg = ToggleStatusDropdown
        , toggleButton =
            Dropdown.toggle [ Button.light ] [ text "Filter" ]
        , items =
            [ Dropdown.buttonItem [ onClick (FilterStatus "Action1") ] [ text "Action1" ]
            , Dropdown.buttonItem [ onClick (FilterStatus "Action2") ] [ text "Action2" ]
            , Dropdown.buttonItem [ onClick (FilterStatus "Action3") ] [ text "Action3" ]
            , Dropdown.buttonItem [ onClick (FilterStatus "Action4") ] [ text "Action4" ]
            ]
        }



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
               ]
        )


hearingDropdownSubscriptions : Model -> List (Sub Msg)
hearingDropdownSubscriptions model =
    model.hearingsDropdownStates
        |> List.map (\( caseNumber, state ) -> Dropdown.subscriptions state (ToggleHearingDropdown caseNumber))
