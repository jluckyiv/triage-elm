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
import Html.Attributes exposing (class, id, src, type_, placeholder, href, attribute)
import Html.Events exposing (onClick)
import Http
import List.Extra
import Maybe.Extra
import Moment
import RemoteData exposing (WebData)
import Task


-- TODO: dynamic department filter button
---- MODEL ----


type alias Model =
    { actionDropdownStates : HearingDropdownStates
    , dateString : DateString
    , departmentDropdownState : Dropdown.State
    , departmentFilter : Department
    , departments : List Department
    , hearings : List ( DepartmentString, WebData (List Hearing) )
    , interpreterDropdownState : Dropdown.State
    , navbarState : Navbar.State
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


type alias HearingDropdownStates =
    List ( CaseNumber, Dropdown.State )


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
        ( { departmentDropdownState = Dropdown.initialState
          , departmentFilter = All
          , departments = [ All, F201, F301, F401, F402, F501, F502 ]
          , interpreterDropdownState = Dropdown.initialState
          , actionDropdownStates = []
          , navbarState = navbarState
          , statusDropdownState = Dropdown.initialState
          , hearings = initHearings
          , dateString = ""
          }
        , Cmd.batch [ navbarCmd, Task.perform ReceiveDate Date.now ]
        )


initActionButtons : { d | hearings : List { b | caseNumber : a }, actionDropdownStates : c } -> { d | hearings : List { b | caseNumber : a }, actionDropdownStates : List ( a, Dropdown.State ) }
initActionButtons model =
    let
        actionDropdownStates =
            List.map (\h -> ( h.caseNumber, Dropdown.initialState )) model.hearings
    in
        { model | actionDropdownStates = actionDropdownStates }


initActionDropdownStates : a -> List b
initActionDropdownStates departmentHearings =
    []


initHearings : List ( DepartmentString, WebData (List Hearing) )
initHearings =
    [ ( toString F201, RemoteData.Loading )
    , ( toString F301, RemoteData.Loading )
    , ( toString F401, RemoteData.Loading )
    , ( toString F402, RemoteData.Loading )
    , ( toString F501, RemoteData.Loading )
    , ( toString F502, RemoteData.Loading )
    ]



---- UPDATE ----


type Msg
    = NoOp
    | FilterDepartment Department
    | FilterInterpreter String
    | FilterStatus String
    | InitActionDropdown
    | NavbarMsg Navbar.State
    | ReceiveDate Date.Date
    | ReceiveHearings Department (WebData (List Hearing))
    | RequestHearings
    | ToggleActionDropdown String Dropdown.State
    | ToggleDepartmentDropdown Dropdown.State
    | ToggleInterpreterDropdown Dropdown.State
    | ToggleStatusDropdown Dropdown.State
    | UpdateDateString DateString


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InitActionDropdown ->
            let
                actionDropdownStates =
                    initActionDropdownStates model.hearings
            in
                ( { model | actionDropdownStates = actionDropdownStates }, Cmd.none )

        FilterDepartment department ->
            ( { model | departmentFilter = department }
            , Cmd.none
            )

        FilterInterpreter language ->
            let
                _ =
                    Debug.log "filter language" language
            in
                ( model, Cmd.none )

        FilterStatus status ->
            let
                _ =
                    Debug.log "filter status" status
            in
                ( model, Cmd.none )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )

        ReceiveDate date ->
            ( model, Cmd.batch <| requestHearings (dateToDateString date) )

        ReceiveHearings department response ->
            ( receiveHearings model department response, Cmd.none )

        RequestHearings ->
            ( { model
                | hearings = initHearings
              }
            , Cmd.batch
                (requestHearings model.dateString)
            )

        ToggleActionDropdown caseNumber state ->
            let
                actionDropdownStates =
                    mapActionDropdownStates model caseNumber state
            in
                ( { model | actionDropdownStates = actionDropdownStates }, Cmd.none )

        ToggleDepartmentDropdown state ->
            ( { model | departmentDropdownState = state }
            , Cmd.none
            )

        ToggleInterpreterDropdown state ->
            ( { model | interpreterDropdownState = state }
            , Cmd.none
            )

        ToggleStatusDropdown state ->
            ( { model | statusDropdownState = state }
            , Cmd.none
            )

        UpdateDateString dateString ->
            ( { model | dateString = dateString }, Cmd.none )



---- HELPERS ----


mapActionDropdownStates : Model -> CaseNumber -> Dropdown.State -> List ( CaseNumber, Dropdown.State )
mapActionDropdownStates model caseNumber state =
    model.actionDropdownStates
        |> List.map
            (\( cn, s ) ->
                if cn == caseNumber then
                    ( caseNumber, state )
                else
                    ( cn, s )
            )


toggleActionDropdown : Model -> Hearing -> Dropdown.State
toggleActionDropdown model hearing =
    model.actionDropdownStates
        |> List.filter (\( caseNumber, state ) -> caseNumber == hearing.caseNumber)
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault Dropdown.initialState


dateToDateString : Date.Date -> String
dateToDateString date =
    Moment.format [ Moment.YearNumberCapped, Moment.MonthFixed, Moment.DayOfMonthFixed ] date


receiveHearings : Model -> Department -> WebData (List Hearing) -> Model
receiveHearings model department response =
    case department of
        All ->
            model

        _ ->
            let
                hearings =
                    List.map
                        (\( key, value ) ->
                            if key == toString department then
                                ( key, response )
                            else
                                ( key, value )
                        )
                        model.hearings
            in
                { model
                    | hearings = hearings
                }


requestHearings : DateString -> List (Cmd Msg)
requestHearings dateString =
    [ requestDepartmentHearings ( dateString, F201 )
    , requestDepartmentHearings ( dateString, F301 )
    , requestDepartmentHearings ( dateString, F401 )
    , requestDepartmentHearings ( dateString, F402 )
    , requestDepartmentHearings ( dateString, F501 )
    , requestDepartmentHearings ( dateString, F502 )
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
                    [ h1 [] [ text "Triage Elm" ]
                    , p [ class "lead" ]
                        [ text "Making Triage Functional"
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
            , Navbar.dropdown
                { id = "navbarDropdown"
                , toggle = Navbar.dropdownToggle [] [ text "Nav" ]
                , items =
                    [ Navbar.dropdownHeader [ text "Heading" ]
                    , Navbar.dropdownItem
                        [ href "#drop1" ]
                        [ text "Drop item 1" ]
                    , Navbar.dropdownItem
                        [ href "#drop2" ]
                        [ text "Drop item 2" ]
                    , Navbar.dropdownDivider
                    , Navbar.dropdownItem
                        [ href "#drop3" ]
                        [ text "Drop item 3" ]
                    ]
                }
            ]
        |> Navbar.customItems
            [ Navbar.formItem []
                [ Input.text [ Input.attrs [ placeholder "YYYYMMDD" ], Input.onInput UpdateDateString ]
                , Button.button
                    [ Button.outlineLight
                    , Button.attrs [ Spacing.ml2Sm ]
                    , Button.onClick RequestHearings
                    ]
                    [ text "Search" ]
                ]
            ]
        |> Navbar.view model.navbarState


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
                [ Row.leftXs ]
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
            [ Grid.row []
                [ Grid.col [] [ text "Pending" ] ]
            ]

        ( departmentString, RemoteData.Loading ) ->
            [ Grid.row []
                [ Grid.col [] [ text "Loading" ] ]
            ]

        ( departmentString, RemoteData.Success hearings ) ->
            hearingRows model hearings departmentString

        ( departmentString, RemoteData.Failure e ) ->
            [ Grid.row []
                [ Grid.col [] [ text <| "Department " ++ departmentString ++ " had the following error " ++ (toString e) ]
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
                [ Grid.row [] [ Grid.col [ Col.attrs [ class "text-muted" ] ] [ text (departmentString ++ " has no RFOs on calendar.") ] ] ]

            _ ->
                List.map (hearingRow model) hearingRows


hearingRow : Model -> CaseManagementData.Hearing -> Html Msg
hearingRow model hearing =
    Grid.row []
        [ Grid.col [ Col.xs1 ] [ text hearing.department ]
        , Grid.col [ Col.xs2 ] [ text hearing.caseNumber ]
        , interpreterCol hearing.interpreter
        , petitionerCol hearing.parties
        , respondentCol hearing.parties
        , Grid.col [] [ actionDropdown model hearing ]
        , Grid.colBreak []
        ]


interpreterCol : Maybe (List CaseManagementData.Interpreter) -> Grid.Column msg
interpreterCol interpreter =
    case interpreter of
        Nothing ->
            Grid.col [ Col.xs1, Col.attrs [ class "text-muted" ] ] [ text "None" ]

        Just interpreters ->
            let
                languages =
                    interpreters |> List.map (\i -> i.language) |> String.join ", " |> String.left 8
            in
                Grid.col [ Col.xs1, Col.attrs [ class "text-primary" ] ] [ text languages ]


petitionerCol : List CaseManagementData.Party -> Grid.Column msg
petitionerCol parties =
    let
        petitioner =
            parties
                |> List.filter (\p -> String.contains "Other" p.partyType || p.partyType == "Mother" || p.partyType == "Wife" || p.partyType == "Protected Person" || p.partyType == "Petitioner")
                |> List.Extra.uniqueBy (\p -> p.id)
                |> List.filter (\p -> not <| String.contains "COUNTY OF RIVERSIDE" (String.toUpper <| Maybe.withDefault "" p.organizationName))
                |> List.head
                |> Maybe.map (\p -> [ p.firstName, p.lastName ])
                |> Maybe.map (Maybe.Extra.combine)
                |> Maybe.Extra.join
                |> Maybe.map (String.join " ")
                |> Maybe.withDefault "Person Doe"
    in
        Grid.col [ Col.xs3 ] [ text petitioner ]


respondentCol : List CaseManagementData.Party -> Grid.Column msg
respondentCol parties =
    let
        respondent =
            parties
                |> List.filter (\p -> p.partyType == "Respondent" || p.partyType == "Father" || p.partyType == "Husband" || p.partyType == "Restrained Person")
                |> List.head
                |> Maybe.map (\p -> [ p.firstName, p.lastName ])
                |> Maybe.map (Maybe.Extra.combine)
                |> Maybe.Extra.join
                |> Maybe.map (String.join " ")
                |> Maybe.withDefault "Person Doe"
    in
        Grid.col [ Col.xs3 ] [ text respondent ]


actionDropdown : Model -> Hearing -> Html Msg
actionDropdown model hearing =
    Dropdown.dropdown
        (toggleActionDropdown model hearing)
        { options = []
        , toggleMsg = ToggleActionDropdown hearing.caseNumber
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


interpreterDropdown : Model -> Html Msg
interpreterDropdown model =
    Dropdown.dropdown
        model.interpreterDropdownState
        { options = []
        , toggleMsg = ToggleInterpreterDropdown
        , toggleButton = Dropdown.toggle [ Button.light ] [ text "Interp" ]
        , items =
            [ Dropdown.buttonItem [ onClick (FilterInterpreter "Spanish") ] [ text "Spanish" ]
            , Dropdown.buttonItem [ onClick (FilterInterpreter "None") ] [ text "None" ]
            ]
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
        (actionDropdownSubscriptions model
            ++ [ Navbar.subscriptions model.navbarState NavbarMsg
               , Dropdown.subscriptions model.departmentDropdownState ToggleDepartmentDropdown
               , Dropdown.subscriptions model.interpreterDropdownState ToggleInterpreterDropdown
               , Dropdown.subscriptions model.statusDropdownState ToggleStatusDropdown
               ]
        )


actionDropdownSubscriptions : Model -> List (Sub Msg)
actionDropdownSubscriptions model =
    model.actionDropdownStates
        |> List.map (\( caseNumber, state ) -> Dropdown.subscriptions state (ToggleActionDropdown caseNumber))
