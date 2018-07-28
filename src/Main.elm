module Main exposing (..)

import Html exposing (Html, text, div, h1, img, i, nav, p, br, button, label, a, input, form, li, ul, span)
import Html.Attributes exposing (class, src, type_, placeholder, href, attribute, id)


---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ nav [ class "navbar navbar-expand-md navbar-dark bg-dark fixed-top" ]
            [ a [ class "navbar-brand", href "#" ]
                [ text "Navbar" ]
            , button [ class "navbar-toggler", type_ "button", attribute "data-toggle" "collapse", attribute "data-target" "#navbarsExampleDefault", attribute "aria-controls" "navbarsExampleDefault", attribute "aria-expanded" "false", attribute "aria-label" "Toggle navigation" ]
                [ span [ class "navbar-toggler-icon" ]
                    []
                ]
            , div [ class "collapse navbar-collapse", id "navbarsExampleDefault" ]
                [ ul [ class "navbar-nav mr-auto" ]
                    [ li [ class "nav-item active" ]
                        [ a [ class "nav-link", href "#" ]
                            [ text "Home"
                            , span
                                [ class "sr-only" ]
                                [ text "(current)" ]
                            ]
                        ]
                    , li [ class "nav-item" ]
                        [ a [ class "nav-link", href "#" ]
                            [ text "Link" ]
                        ]
                    , li [ class "nav-item" ]
                        [ a [ class "nav-link disabled", href "#" ]
                            [ text "Disabled" ]
                        ]
                    , li [ class "nav-item dropdown" ]
                        [ a [ class "nav-link dropdown-toggle", href "#", id "dropdown01", attribute "data-toggle" "dropdown", attribute "aria-haspopup" "true", attribute "aria-expanded" "false" ]
                            [ text "Dropdown" ]
                        , div [ class "dropdown-menu", attribute "aria-labelledby" "dropdown01" ]
                            [ a [ class "dropdown-item", href "#" ]
                                [ text "Action" ]
                            , a [ class "dropdown-item", href "#" ]
                                [ text "Another action" ]
                            , a [ class "dropdown-item", href "#" ]
                                [ text "Something else here" ]
                            ]
                        ]
                    ]
                , form [ class "form-inline my-2 my-lg-0" ]
                    [ input [ class "form-control mr-sm-2", type_ "text", placeholder "Search", attribute "aria-label" "Search" ]
                        []
                    , button [ class "btn btn-outline-success my-2 my-sm-0", type_ "submit" ]
                        [ text "Search" ]
                    ]
                ]
            ]
        , div [ attribute "role" "main", class "container" ]
            [ div [ class "starter-template" ]
                [ img [ src "/logo.svg", class "logo" ] []
                , h1 [] [ text "Bootstrap starter template" ]
                , p [ class "lead" ]
                    [ text "Use this document as a way to quickly start any new project."
                    , br [] []
                    , text "All you get is this text and a mostly barebones HTML document."
                    ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
