module App exposing (..)

import Block exposing (Block)
import Board exposing (Board)
import Dict exposing (Dict)
import Html exposing (..)
-- import Html.Attributes exposing (type_)
-- import Html.Events exposing (onClick, onMouseDown)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tetromino exposing (Tetromino)
import Time exposing (Time, second, millisecond)
import Keyboard
import Random.Array exposing (..)
import Random
import Array exposing (Array)


-- import Debug exposing (log)


type alias Model =
    { board : Board
    , falling : Tetromino
    , bag : List Tetromino
    , score : Int
    , gamestate : Gamestate
    }


type alias Location =
    ( Int, Int )

type Gamestate 
    = Stopped
    | Playing
    | Gameover

bagSampler : Random.Generator (Maybe Tetromino)
bagSampler = sample Tetromino.tetrominos

initialBagSampler : Random.Generator (Array.Array (Maybe Tetromino))
initialBagSampler = Random.Array.array 5 bagSampler

model : Model
model =
    { board = Dict.empty --Board.testBoard
    , falling = Tetromino.i
    , bag = []
    , score = 0
    , gamestate = Stopped
    }

init : (Model, Cmd Msg)
init =
    (model, Random.generate InitBag initialBagSampler)
--   ({model | bag = Random.Array.array 5 bagSampler}, Cmd.none)



-- Update

type Msg
    = Tick Time
    | KeyMsg Keyboard.KeyCode
    | NewBagTetromino (Maybe Tetromino)
    | InitBag (Array (Maybe Tetromino))


fall : Tetromino -> Tetromino
fall tetromino =
    { tetromino | location = sumLocation tetromino.location ( 0, -1 ) }


moveRight : Tetromino -> Tetromino
moveRight tetromino =
    { tetromino | location = sumLocation tetromino.location ( 1, 0 ) }


moveLeft : Tetromino -> Tetromino
moveLeft tetromino =
    { tetromino | location = sumLocation tetromino.location ( -1, 0 ) }


sumLocation : Location -> Location -> Location
sumLocation ( c1, r1 ) ( c2, r2 ) =
    ( c1 + c2, r1 + r2 )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            if Board.gameOver model.board then
                ( { model | gamestate = Gameover }, Cmd.none )
            else if Board.isValid model.board (fall model.falling) then
                ({ model | falling = fall model.falling }, Cmd.none)
            else
                let
                    (score, newBoard) = Board.addTetromino model.board model.falling |> Board.clearLines
                in                    
                ({ model
                    | board = newBoard
                    , falling = Maybe.withDefault Tetromino.i (List.head model.bag)
                    , bag = Maybe.withDefault [Tetromino.i] (List.tail model.bag)
                    , score = model.score + score
                }, Random.generate NewBagTetromino bagSampler)

        KeyMsg code ->
            if model.gamestate == Playing then
                case code of
                    39 ->
                        if Board.isValid model.board (moveRight model.falling) then
                            ({ model | falling = moveRight model.falling }, Cmd.none)
                        else
                            (model, Cmd.none)
                    37 ->
                        if Board.isValid model.board (moveLeft model.falling) then
                            ({ model | falling = moveLeft model.falling }, Cmd.none)
                        else
                            (model, Cmd.none)
                    40 ->
                        if Board.isValid model.board (fall model.falling) then
                            ({ model | falling = fall model.falling }, Cmd.none)
                        else
                            (model, Cmd.none)
                    90 ->
                        ({ model | falling = Board.rotateTetromino model.board -1 model.falling }, Cmd.none)
                    88 ->
                        ({ model | falling = Board.rotateTetromino model.board 1 model.falling }, Cmd.none)
                    _ ->
                        (model, Cmd.none)
            else
                case code of
                    32 ->
                        ({ model | gamestate = Playing, board = Dict.empty }, Cmd.none)
                    _ ->
                        (model, Cmd.none)

        NewBagTetromino newBagTetromino ->
            let
                tetromino =
                    Maybe.withDefault Tetromino.i newBagTetromino
            in
            ({ model | bag = model.bag ++ [tetromino]}, Cmd.none)
        InitBag initialBag ->
            let
                bag = List.map (Maybe.withDefault Tetromino.i) (Array.toList initialBag)
            in
            ({ model | bag = bag }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        tickSub = 
            if model.gamestate == Playing then
                [Time.every (75 * millisecond) Tick]
            else
                []
    in
    Sub.batch 
        (tickSub ++ [Keyboard.downs KeyMsg])


-- View


renderBoard : Board -> Svg msg
renderBoard board =
    let
        toRect ( col, row ) block =
            Block.render block ( col, row )
    in
    Svg.g [] (Board.background :: Dict.values (Dict.map toRect board))


view : Model -> Html Msg
view model =
    let
        w =
            Block.size * Board.cols |> toString

        h =
            Block.size * Board.rows |> toString
    in
    div []
        [ svg
            [ width w, height h, viewBox ("0 0 " ++ (Block.size * Board.cols |> toString) ++ " " ++ (Block.size * Board.rows |> toString)) ]
            ([ renderBoard model.board ] ++ [Tetromino.render model.falling])
        , div []
            [ h3 [] [Html.text "Score"]
            , Html.text (toString model.score)
            ]
        , div []
            [ h3 [] [Html.text "Gamestate"]
            , Html.text (toString model.gamestate)
            ]
        , div []
            [ h3 [] [Html.text "Bag"]
            , Html.text (toString (List.map (\tetromino -> tetromino.t) model.bag))
            ]
        ]


main : Program Never Model Msg
main =
    Html.program 
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions }

