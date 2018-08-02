module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar
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
    { dateString : DateString
    , departmentDropdownState : Dropdown.State
    , departmentFilter : Department
    , departments : List Department
    , hearings : List ( DepartmentString, WebData (List Hearing) )
    , interpreterDropdownState : Dropdown.State
    , hearingsDropdownStates : HearingDropdownStates
    , navbarState : Navbar.State
    , statusDropdownState : Dropdown.State
    }


type alias DateString =
    String


type alias DepartmentString =
    String


type Department
    = All
    | F201
    | F301
    | F401
    | F402
    | F501
    | F502


type alias Language =
    String


type alias CaseNumber =
    String


type alias FullName =
    String


type alias HearingDropdownStates =
    List ( CaseNumber, Dropdown.State )


resetHearings : List ( DepartmentString, WebData (List Hearing) )
resetHearings =
    [ ( toString F201, RemoteData.Loading )
    , ( toString F301, RemoteData.Loading )
    , ( toString F401, RemoteData.Loading )
    , ( toString F402, RemoteData.Loading )
    , ( toString F501, RemoteData.Loading )
    , ( toString F502, RemoteData.Loading )
    ]


initActionButtons : { d | hearings : List { b | caseNumber : a }, hearingsDropdownStates : c } -> { d | hearings : List { b | caseNumber : a }, hearingsDropdownStates : List ( a, Dropdown.State ) }
initActionButtons model =
    let
        hearingsDropdownStates =
            List.map (\h -> ( h.caseNumber, Dropdown.initialState )) model.hearings
    in
        { model | hearingsDropdownStates = hearingsDropdownStates }


init : ( Model, Cmd Msg )
init =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        hearings =
            []

        hearingsDropdownStates =
            List.map (\m -> ( m.caseNumber, Dropdown.initialState )) hearings
    in
        ( { departmentDropdownState = Dropdown.initialState
          , departmentFilter = All
          , departments = [ All, F201, F301, F401, F402, F501, F502 ]
          , interpreterDropdownState = Dropdown.initialState
          , hearingsDropdownStates = hearingsDropdownStates
          , navbarState = navbarState
          , statusDropdownState = Dropdown.initialState
          , hearings = resetHearings
          , dateString = ""
          }
        , Cmd.batch [ navbarCmd, Task.perform ReceiveDate Date.now ]
        )


hearingsUrl : ( DateString, Department ) -> String
hearingsUrl ( date, department ) =
    "https://cbmdev.riverside.courts.ca.gov/Hearing/FL/" ++ date ++ "/" ++ (toString department)


requestHearings : ( DateString, Department ) -> Cmd Msg
requestHearings ( date, department ) =
    CaseManagementData.hearingsDecoder
        |> Http.get (hearingsUrl ( date, department ))
        |> RemoteData.sendRequest
        |> Cmd.map (ReceiveHearings department)


requestAllHearings : DateString -> List (Cmd Msg)
requestAllHearings dateString =
    [ requestHearings ( dateString, F201 )
    , requestHearings ( dateString, F301 )
    , requestHearings ( dateString, F401 )
    , requestHearings ( dateString, F402 )
    , requestHearings ( dateString, F501 )
    , requestHearings ( dateString, F502 )
    ]


receiveHearings : Model -> Department -> WebData (List Hearing) -> Model
receiveHearings model department response =
    case department of
        All ->
            model

        _ ->
            { model
                | hearings =
                    List.map
                        (\( key, value ) ->
                            if key == toString department then
                                ( key, response )
                            else
                                ( key, value )
                        )
                        model.hearings
            }



---- UPDATE ----


type Msg
    = NoOp
    | ToggleDropdown String Dropdown.State
    | DepartmentDropdownToggle Dropdown.State
    | InterpreterDropdownToggle Dropdown.State
    | InitActionButtons HearingDropdownStates
    | StatusDropdownToggle Dropdown.State
    | FilterDepartment Department
    | FilterInterpreter String
    | FilterStatus String
    | NavbarMsg Navbar.State
    | RequestHearings
    | ReceiveHearings Department (WebData (List Hearing))
    | ReceiveDate Date.Date
    | UpdateDateString DateString


mapHearingsDropdownStates : Model -> CaseNumber -> Dropdown.State -> List ( CaseNumber, Dropdown.State )
mapHearingsDropdownStates model caseNumber state =
    model.hearingsDropdownStates
        |> List.map
            (\( cn, s ) ->
                if cn == caseNumber then
                    ( caseNumber, state )
                else
                    ( cn, s )
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleDropdown caseNumber state ->
            let
                hearingsDropdownStates =
                    mapHearingsDropdownStates model caseNumber state
            in
                ( { model | hearingsDropdownStates = hearingsDropdownStates }, Cmd.none )

        DepartmentDropdownToggle state ->
            ( { model | departmentDropdownState = state }
            , Cmd.none
            )

        InterpreterDropdownToggle state ->
            ( { model | interpreterDropdownState = state }
            , Cmd.none
            )

        StatusDropdownToggle state ->
            ( { model | statusDropdownState = state }
            , Cmd.none
            )

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

        RequestHearings ->
            ( { model
                | hearings = resetHearings
              }
            , Cmd.batch
                (requestAllHearings model.dateString)
            )

        UpdateDateString dateString ->
            ( { model | dateString = dateString }, Cmd.none )

        ReceiveDate date ->
            ( model, Cmd.batch <| requestAllHearings (toDateString date) )

        ReceiveHearings department response ->
            ( receiveHearings model department response, Cmd.none )

        InitActionButtons hearingDropdownStates ->
            ( model, Cmd.none )


toDateString : Date.Date -> String
toDateString date =
    Moment.format [ Moment.YearNumberCapped, Moment.MonthFixed, Moment.DayOfMonthFixed ] date



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        hearings =
            model.hearings
    in
        div []
            [ viewNavbar model
            , div [ attribute "role" "main", class "container" ]
                [ div [ class "starter-template" ]
                    [ h1 [] [ text "Triage Elm" ]
                    , p [ class "lead" ]
                        [ text "Making Triage Functional"
                        ]
                    ]
                , viewActionTable model
                ]
            ]


viewNavbar : Model -> Html Msg
viewNavbar model =
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


viewActionTable : Model -> Html Msg
viewActionTable model =
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
                []
                [ Grid.col [ Col.xs1, Col.middleMd ] [ departmentDropdown model ]
                , Grid.col [ Col.xs2 ] [ text "Case Number" ]
                , Grid.col [ Col.xs1 ] [ text "Interp" ]
                , Grid.col [ Col.xs3 ] [ text "Petitioner" ]
                , Grid.col [ Col.xs3 ] [ text "Respondent" ]
                , Grid.col [ Col.xs1 ] [ statusDropdown model ]
                ]
             ]
                ++ List.concatMap (viewDepartmentRow model) hearings
            )


viewDepartmentRow : Model -> ( DepartmentString, WebData (List Hearing) ) -> List (Html Msg)
viewDepartmentRow model departmentHearings =
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
            viewHearingRows model hearings departmentString

        ( departmentString, RemoteData.Failure e ) ->
            [ Grid.row []
                [ Grid.col [] [ text <| "Department " ++ departmentString ++ " had the following error " ++ (toString e) ]
                ]
            ]


viewHearingRows : Model -> List Hearing -> DepartmentString -> List (Html Msg)
viewHearingRows model hearings departmentString =
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
                List.map (viewHearingRow model) hearingRows


viewHearingRow : Model -> CaseManagementData.Hearing -> Html Msg
viewHearingRow model hearing =
    Grid.row []
        [ Grid.col [ Col.xs1 ] [ text hearing.department ]
        , Grid.col [ Col.xs2 ] [ text hearing.caseNumber ]
        , viewInterpreter hearing.interpreter
        , viewPetitionerOrOtherParty hearing.parties
        , viewRespondent hearing.parties
        , Grid.col [ Col.xs1 ] [ text "action" ]
        , Grid.colBreak []

        -- , Grid.col [] [ actionDropdown model matter ]
        ]


viewInterpreter : Maybe (List CaseManagementData.Interpreter) -> Grid.Column msg
viewInterpreter interpreter =
    case interpreter of
        Nothing ->
            Grid.col [ Col.xs1, Col.attrs [ class "text-muted" ] ] [ text "None" ]

        Just interpreters ->
            let
                languages =
                    interpreters |> List.map (\i -> i.language) |> String.join ", " |> String.left 8
            in
                Grid.col [ Col.xs1, Col.attrs [ class "text-primary" ] ] [ text languages ]


viewPetitionerOrOtherParty : List CaseManagementData.Party -> Grid.Column msg
viewPetitionerOrOtherParty parties =
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


viewRespondent : List CaseManagementData.Party -> Grid.Column msg
viewRespondent parties =
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


actionDropdownState : Model -> Hearing -> Dropdown.State
actionDropdownState model hearing =
    model.hearingsDropdownStates
        |> List.filter (\( caseNumber, state ) -> caseNumber == hearing.caseNumber)
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault Dropdown.initialState


actionDropdown : Model -> Hearing -> Html Msg
actionDropdown model hearing =
    Dropdown.dropdown
        (actionDropdownState model hearing)
        { options = []
        , toggleMsg = ToggleDropdown hearing.caseNumber
        , toggleButton =
            Dropdown.toggle [ Button.light ] [ text "Choose Action" ]
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
            , toggleMsg = DepartmentDropdownToggle
            , toggleButton = Dropdown.toggle [ Button.light ] [ text label ]
            , items =
                List.map (\d -> Dropdown.buttonItem [ onClick (FilterDepartment d) ] [ text (toString d) ]) model.departments
            }


interpreterDropdown : Model -> Html Msg
interpreterDropdown model =
    Dropdown.dropdown
        model.interpreterDropdownState
        { options = []
        , toggleMsg = InterpreterDropdownToggle
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
        , toggleMsg = StatusDropdownToggle
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


hearingsDropdownSubscriptions : Model -> List (Sub Msg)
hearingsDropdownSubscriptions model =
    model.hearingsDropdownStates
        |> List.map (\( caseNumber, state ) -> Dropdown.subscriptions state (ToggleDropdown caseNumber))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        (hearingsDropdownSubscriptions model
            ++ [ Navbar.subscriptions model.navbarState NavbarMsg
               , Dropdown.subscriptions model.departmentDropdownState DepartmentDropdownToggle
               , Dropdown.subscriptions model.interpreterDropdownState InterpreterDropdownToggle
               , Dropdown.subscriptions model.statusDropdownState StatusDropdownToggle
               ]
        )
