module Main exposing (..)

import Html exposing (Html, text, div, h1, img, i)
import Html.Attributes exposing (class, src)
import Bootstrap.Grid as Grid


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
        [ Grid.container []
            [ Grid.row []
                [ Grid.col []
                    [ img [ src "/logo.svg" ] [] ]
                ]
            , Grid.row []
                [ Grid.col []
                    [ text "The future home of Elm Triage"
                    , i [ class "fa fa-arrow-up" ] []
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
