module Main exposing (..)

import Browser
import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

import Bootstrap.Button as Button


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL

type alias Model =
    { state : List Int
    , turn : Int
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( initModel, Cmd.none )

initModel : Model
initModel =
    Model initState 1

initState : List Int
initState =
    List.repeat 9 0



-- UPDATE

type Msg
    = MakeMove Int
    | ResetGame

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MakeMove i ->
            (
                { model
                    | state = updateState i model.turn model.state
                    , turn = updateTurn model.turn
                }
            , Cmd.none
            )

        ResetGame ->
            ( initModel, Cmd.none )

updateState : Int -> Int -> List Int -> List Int
updateState i turn state =
    List.indexedMap (replaceByIndex i turn) state

updateTurn : Int -> Int
updateTurn turn =
    if turn == 1 then 2 else 1

replaceByIndex : Int -> Int -> Int -> Int -> Int
replaceByIndex target newVal i oldVal =
    if target == i then newVal else oldVal


-- VIEW

view : Model -> Html Msg
view model =
    Grid.container []
        [ Grid.row []
            [ Grid.col [] [ text "hello" ]
            ]
        , viewBoard model.state
        , Button.button
            [ Button.attrs [ onClick ResetGame ] ]
            [ text "Reset game" ]
        ]

viewBoard : List Int -> Html Msg
viewBoard state =
    Grid.row []
        (List.indexedMap viewCell state)

viewCell : Int -> Int -> Grid.Column Msg
viewCell i val =
    if val == 1 then
        Grid.col [ Col.xs4 ] [ text "1" ]
    else if val == 2 then
        Grid.col [ Col.xs4 ] [ text "2" ]
    else
        Grid.col
            [ Col.xs4
            , Col.attrs [ onClick (MakeMove i) ]
            ]
            [ text "0" ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
