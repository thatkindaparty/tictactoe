port module Main exposing (..)

import Browser
import Html exposing (Html, text, div, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

import Bootstrap.Button as Button
import Bootstrap.Utilities.Spacing as Spacing

import Json.Encode as E
import Json.Decode as D

port pullState : (D.Value -> msg) -> Sub msg
port pushState : E.Value -> Cmd msg

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

stateFromJson : D.Decoder State
stateFromJson =
    D.map2 State
        (D.field "plays" (D.list D.int))
        (D.field "turn" D.int)

stateToJson : State -> E.Value
stateToJson state =
    E.object
        [ ("plays", E.list E.int state.plays)
        , ("turn", E.int state.turn)
        ]


-- MODEL

type alias Model =
    { state : State
    , errMsg : String
    }

type alias State =
    { plays : List Int
    , turn : Int
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( initModel, Cmd.none )

initModel : Model
initModel =
    Model initState ""

initState : State
initState =
    State (List.repeat 9 0) 1



-- UPDATE

type Msg
    = MakeMove Int
    | ResetGame
    | PullState D.Value

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MakeMove i ->
            let
                newState = updateState i model.state
            in
                ( { model | state = newState }
                , pushState (stateToJson newState)
                )

        ResetGame ->
            ( initModel
            , pushState (stateToJson initState)
            )

        PullState value ->
            case D.decodeValue stateFromJson value of
                Ok state ->
                    ( { model | state = state }
                    , Cmd.none
                    )

                Err e ->
                    ( { model | errMsg = "Error in updating the game" }
                    , Cmd.none
                    )


updateState : Int -> State -> State
updateState i state =
    { state
        | plays = List.indexedMap (replaceByIndex i state.turn) state.plays
        , turn = updateTurn state.turn
    }

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
            [ Grid.col
                [ Col.attrs [ Spacing.mt2, Spacing.mb2 ] ]
                [ viewTurn model.state.turn ]
            ]
        , viewBoard model.state.plays
        , Button.button
            [ Button.attrs [ onClick ResetGame ] ]
            [ text "Reset game" ]
        ]

viewTurn : Int -> Html Msg
viewTurn turn =
    if turn == 1 then
        span [ style "color" "red" ] [ text "red's turn" ]
    else
        span [ style "color" "blue" ] [ text "blue's turn" ]

viewBoard : List Int -> Html Msg
viewBoard plays =
    div [ style "width" "225px" ]
        (List.indexedMap viewCell plays)

viewCell : Int -> Int -> Html Msg
viewCell i val =
    if val == 1 then
        div [ style "display" "inline-block"
            , style "border" "1px solid black"
            , style "margin-right" "5px"
            , style "width" "70px"
            , style "height" "70px"
            , style "background-color" "red"
            ] []
    else if val == 2 then
        div [ style "display" "inline-block"
            , style "border" "1px solid black"
            , style "margin-right" "5px"
            , style "width" "70px"
            , style "height" "70px"
            , style "background-color" "blue"
            ] []
    else
        div [ style "display" "inline-block"
            , style "border" "1px solid black"
            , style "margin-right" "5px"
            , style "width" "70px"
            , style "height" "70px"
            , onClick (MakeMove i)
            ] []


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    pullState PullState
