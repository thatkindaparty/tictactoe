port module Main exposing (..)

import Browser
import Html exposing (Html, text, div, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

import Bootstrap.Alert as Alert
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
    , winner : Int
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
    Model initState "" 0

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
                newState =
                    if model.winner == 0 then
                        updateState i model.state
                    else
                        model.state
            in
                (
                    { model
                        | state = newState
                        , winner = getWinner newState
                    }
                , pushState (stateToJson newState)
                )

        ResetGame ->
            ( initModel
            , pushState (stateToJson initState)
            )

        PullState value ->
            case D.decodeValue stateFromJson value of
                Ok state ->
                    (
                        { model
                            | state = state
                            , winner = getWinner state
                        }
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
        , turn = nextTurn state.turn
    }

nextTurn : Int -> Int
nextTurn turn =
    if turn == 1 then -1 else 1

replaceByIndex : Int -> Int -> Int -> Int -> Int
replaceByIndex target newVal i oldVal =
    if target == i then newVal else oldVal


getWinner : State -> Int
getWinner state =
    if hasWinner state.plays then
        nextTurn state.turn
    else if List.all ((/=) 0) state.plays then
        2 -- draw
    else
        0

hasWinner : List Int -> Bool
hasWinner plays =
    let
        lines =
            [ [0, 1, 2] , [3, 4, 5] , [6, 7, 8]
            , [0, 3, 6] , [1, 4, 7] , [2, 5, 8]
            , [0, 4, 8] , [2, 4, 6]
            ]
    in
        List.map (checkLine plays) lines
            |> List.any identity

checkLine : List Int -> List Int -> Bool
checkLine plays line =
    List.indexedMap (keepIndices line) plays
        |> List.sum
        |> abs
        |> (==) 3

keepIndices : List Int -> Int -> Int -> Int
keepIndices idxs i val =
    if List.member i idxs then
        val
    else
        0



-- VIEW

view : Model -> Html Msg
view model =
    Grid.container []
        [ Grid.row []
            [ Grid.col
                [ Col.attrs [ Spacing.my2 ] ]
                [ viewTurn model.state.turn ]
            ]
        , viewBoard model.state.plays
        , viewWinner model.winner
        , viewErrMsg model.errMsg
        , Button.button
            [ Button.attrs [ onClick ResetGame ] ]
            [ text "Reset game" ]
        ]

viewErrMsg : String -> Html Msg
viewErrMsg errMsg =
    if String.isEmpty errMsg then
        div [] []
    else
        Alert.simpleDanger [ Spacing.my2 ] [ text errMsg ]

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

viewWinner : Int -> Html Msg
viewWinner winner =
    if winner == 1 then
        div [] [ text "red won!" ]
    else if winner == -1 then
        div [] [ text "blue won!" ]
    else if winner == 2 then
        div [] [ text "It's a draw!" ]
    else
        div [] []

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
    else if val == -1 then
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
