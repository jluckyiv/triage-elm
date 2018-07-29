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
    , matters : List Matter
    }


type alias Matter =
    { department : String
    , interpreter : String
    , caseNumber : String
    , petitioner : String
    , respondent : String
    }


type alias ActionButton =
    { state : Dropdown.State
    , msg : Msg
    , text : String
    , items : List ActionButtonItem
    }


type alias ActionButtonItem =
    { text : String
    , data : String
    }


init : ( Model, Cmd Msg )
init =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
        ( { navbarState = navbarState
          , matters =
                [ { department = "F201", interpreter = "None", caseNumber = "RIF1234567", petitioner = "John Doe", respondent = "Jane Doe" }
                , { department = "F301", interpreter = "None", caseNumber = "RIF1234568", petitioner = "Ron Roe", respondent = "Reyna Roe" }
                ]
          }
        , navbarCmd
        )



---- UPDATE ----


type Msg
    = NoOp
      -- | ActionItemMsg String
    | ActionButtonMsg Dropdown.State
    | ActionButtonItemMsg
    | NavbarMsg Navbar.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ActionButtonItemMsg ->
            ( model, Cmd.none )

        ActionButtonMsg _ ->
            ( model, Cmd.none )

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Navbar.config NavbarMsg
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
        , div [ attribute "role" "main", class "container" ]
            [ div [ class "starter-template" ]
                [ img [ src "/logo.svg", class "logo" ] []
                , h1 [] [ text "Triage Elm" ]
                , p [ class "lead" ]
                    [ text "Making Triage Functional"
                    ]
                ]
            , viewActionTable model.matters
            ]
        ]


viewSorterButton : Html Msg
viewSorterButton =
    Dropdown.dropdown
        Dropdown.initialState
        { options = []
        , toggleMsg = ActionButtonMsg
        , toggleButton = Dropdown.toggle [ Button.warning ] [ text "action button" ]
        , items = [ viewActionButtonItem ]
        }


viewActionTable : List Matter -> Html Msg
viewActionTable matters =
    Table.table
        { options = [ Table.hover ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ viewFilterButton "Dept" ]
                , Table.th [] [ viewFilterButton "Interpreter" ]
                , Table.th [] [ text "Case Number" ]
                , Table.th [] [ text "Petitioner" ]
                , Table.th [] [ text "Respondent" ]
                , Table.th [] [ viewFilterButton "Status" ]
                ]
        , tbody =
            Table.tbody [] (List.map viewActionRow matters)
        }


viewFilterButton : String -> Html Msg
viewFilterButton label =
    -- needs dropdownstate, buttontext, buttoncolor, menuitems
    Dropdown.dropdown
        Dropdown.initialState
        { options = []
        , toggleMsg = ActionButtonMsg
        , toggleButton = Dropdown.toggle [ Button.light ] [ text label ]
        , items = [ viewActionButtonItem ]
        }


viewActionRow : Matter -> Table.Row Msg
viewActionRow matter =
    Table.tr []
        [ Table.td [] [ text matter.department ]
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
        , toggleMsg = ActionButtonMsg
        , toggleButton = Dropdown.toggle [ Button.warning ] [ text label ]
        , items = []
        }


viewActionButtonItem : Dropdown.DropdownItem Msg
viewActionButtonItem =
    Dropdown.buttonItem [ onClick ActionButtonItemMsg ] [ text "action" ]



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

        -- , Dropdown.subscriptions model.navDropdownState DropdownMsg
        ]
