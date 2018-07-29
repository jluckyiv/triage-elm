module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Form.Input as Input
import Bootstrap.Navbar as Navbar
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (Html, text, div, h1, img, i, nav, p, br, button, label, a, input, form, li, ul, span)
import Html.Attributes exposing (class, src, type_, placeholder, href, attribute, id)
import Html.Events exposing (onClick)


---- MODEL ----


type alias Model =
    { navbarState : Navbar.State
    , actionDropdownState : Dropdown.State
    , departmentDropdownState : Dropdown.State
    , departmentFilter : Department
    , departments : List Department
    , interpreterDropdownState : Dropdown.State
    , statusDropdownState : Dropdown.State
    , matters : List Matter
    }


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


type alias Matter =
    { department : Department
    , interpreter : Language
    , caseNumber : CaseNumber
    , petitioner : FullName
    , respondent : FullName
    }


init : ( Model, Cmd Msg )
init =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg

        matters =
            [ { department = F201, interpreter = "None", caseNumber = "RIF1700174", petitioner = "Beatrice Phelps", respondent = "Shannon Terry" }
            , { department = F201, interpreter = "None", caseNumber = "RIF1800505", petitioner = "Bethany Oliver", respondent = "Al Bailey" }
            , { department = F201, interpreter = "None", caseNumber = "RIF1700371", petitioner = "Harry Mendoza", respondent = "Lynette Perry" }
            , { department = F301, interpreter = "None", caseNumber = "RIF1800524", petitioner = "Teresa Stevens", respondent = "Daniel Bishara" }
            , { department = F301, interpreter = "None", caseNumber = "RIF1700867", petitioner = "Kelly Clayton", respondent = "Julian Roberson" }
            , { department = F401, interpreter = "None", caseNumber = "RIF1700273", petitioner = "Leona Jackson", respondent = "Hazel Wood" }
            , { department = F401, interpreter = "None", caseNumber = "RIF1800578", petitioner = "Ramona Hudson", respondent = "Terry Arnold" }
            , { department = F401, interpreter = "None", caseNumber = "RIF1800475", petitioner = "Faith Reyes", respondent = "Herman Barber" }
            , { department = F402, interpreter = "None", caseNumber = "RIF1700123", petitioner = "Terence Chavez", respondent = "Jeannette Mitchell" }
            , { department = F402, interpreter = "None", caseNumber = "RIF1800836", petitioner = "Tracy Fields", respondent = "Kent Osborne" }
            , { department = F402, interpreter = "None", caseNumber = "RIF1800183", petitioner = "Daniel Sherman", respondent = "Pam Moss" }
            , { department = F501, interpreter = "None", caseNumber = "RIF1700678", petitioner = "Javier Roy", respondent = "Clifford Sutton" }
            , { department = F501, interpreter = "None", caseNumber = "RIF1600977", petitioner = "Colleen Padilla", respondent = "Israel Miles" }
            , { department = F501, interpreter = "None", caseNumber = "RIF1700081", petitioner = "Cynthia Collier", respondent = "Jesse Moody" }
            , { department = F502, interpreter = "None", caseNumber = "RIF1600118", petitioner = "Robert Ruiz", respondent = "Margaret Fields" }
            , { department = F502, interpreter = "None", caseNumber = "RIF1800229", petitioner = "Jodi Flores", respondent = "Wendell Moody" }
            , { department = F502, interpreter = "None", caseNumber = "RIF1800181", petitioner = "Moses Moss", respondent = "Carrie Matthews" }
            ]
    in
        ( { navbarState = navbarState
          , actionDropdownState = Dropdown.initialState
          , departmentDropdownState = Dropdown.initialState
          , departmentFilter = All
          , departments = [ All, F201, F301, F401, F402, F501, F502 ]
          , interpreterDropdownState = Dropdown.initialState
          , statusDropdownState = Dropdown.initialState
          , matters = matters
          }
        , navbarCmd
        )



---- UPDATE ----


type Msg
    = NoOp
    | ActionButtonItemMsg
    | ActionDropdownToggleMsg Dropdown.State
    | DepartmentDropdownToggleMsg Dropdown.State
    | InterpreterDropdownToggleMsg Dropdown.State
    | StatusDropdownToggleMsg Dropdown.State
    | FilterDepartment Department
    | FilterInterpreter String
    | FilterStatus String
    | NavbarMsg Navbar.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ActionDropdownToggleMsg state ->
            ( { model | actionDropdownState = state }
            , Cmd.none
            )

        ActionButtonItemMsg ->
            ( model, Cmd.none )

        DepartmentDropdownToggleMsg state ->
            ( { model | departmentDropdownState = state }
            , Cmd.none
            )

        InterpreterDropdownToggleMsg state ->
            ( { model | interpreterDropdownState = state }
            , Cmd.none
            )

        StatusDropdownToggleMsg state ->
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



---- VIEW ----


view : Model -> Html Msg
view model =
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
                [ Input.text [ Input.attrs [ placeholder "search for something" ] ]
                , Button.button
                    [ Button.outlineLight
                    , Button.attrs [ Spacing.ml2Sm ]
                    ]
                    [ text "Search" ]
                ]
            ]
        |> Navbar.view model.navbarState


viewActionTable : Model -> Html Msg
viewActionTable model =
    let
        matters =
            case model.departmentFilter of
                All ->
                    model.matters

                department ->
                    List.filter (\m -> m.department == department) model.matters
    in
        Table.table
            { options = [ Table.hover ]
            , thead =
                Table.simpleThead
                    [ Table.th [] [ departmentDropdown model ]
                    , Table.th [] [ interpreterDropdown model ]
                    , Table.th [] [ text "Case Number" ]
                    , Table.th [] [ text "Petitioner" ]
                    , Table.th [] [ text "Respondent" ]
                    , Table.th [] [ statusDropdown model ]
                    ]
            , tbody =
                Table.tbody [] (List.map viewActionRow matters)
            }


departmentDropdown : Model -> Html Msg
departmentDropdown model =
    let
        label =
            "Department: " ++ toString (model.departmentFilter)
    in
        Dropdown.dropdown
            model.departmentDropdownState
            { options = []
            , toggleMsg = DepartmentDropdownToggleMsg
            , toggleButton = Dropdown.toggle [ Button.light ] [ text label ]
            , items =
                List.map (\d -> Dropdown.buttonItem [ onClick (FilterDepartment d) ] [ text (toString d) ]) model.departments
            }


interpreterDropdown : Model -> Html Msg
interpreterDropdown model =
    Dropdown.dropdown
        model.interpreterDropdownState
        { options = []
        , toggleMsg = InterpreterDropdownToggleMsg
        , toggleButton = Dropdown.toggle [ Button.light ] [ text "Interpreter" ]
        , items =
            [ Dropdown.buttonItem [ onClick (FilterInterpreter "Spanish") ] [ text "Spanish" ]
            , Dropdown.buttonItem [ onClick (FilterInterpreter "Other") ] [ text "Other" ]
            ]
        }


statusDropdown : Model -> Html Msg
statusDropdown model =
    -- needs dropdownstate, buttontext, buttoncolor, menuitems
    Dropdown.dropdown
        model.statusDropdownState
        { options = []
        , toggleMsg = StatusDropdownToggleMsg
        , toggleButton =
            Dropdown.toggle [ Button.light ] [ text "Status" ]
        , items =
            [ Dropdown.buttonItem [ onClick (FilterStatus "Status1") ] [ text "Status1" ]
            , Dropdown.buttonItem [ onClick (FilterStatus "Status2") ] [ text "Status2" ]
            ]
        }


viewActionRow : Matter -> Table.Row Msg
viewActionRow matter =
    Table.tr []
        [ Table.td [] [ text (toString matter.department) ]
        , Table.td [] [ text matter.interpreter ]
        , Table.td [] [ text matter.caseNumber ]
        , Table.td [] [ text matter.petitioner ]
        , Table.td [] [ text matter.respondent ]
        , Table.td [] [ viewActionButton "action" ]
        ]


viewActionButton : String -> Html Msg
viewActionButton label =
    -- needs dropdownstate, buttontext, buttoncolor, menuitems
    Dropdown.dropdown
        Dropdown.initialState
        { options = []
        , toggleMsg = ActionDropdownToggleMsg
        , toggleButton = Dropdown.toggle [ Button.warning ] [ text label ]
        , items = []
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
        [ Navbar.subscriptions model.navbarState NavbarMsg
        , Dropdown.subscriptions model.actionDropdownState ActionDropdownToggleMsg
        , Dropdown.subscriptions model.departmentDropdownState DepartmentDropdownToggleMsg
        , Dropdown.subscriptions model.interpreterDropdownState InterpreterDropdownToggleMsg
        ]
