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


-- import Debug exposing (log)


type alias Model =
    { board : Board
    , falling : Tetromino
    , bag : List Tetromino
    , score: Int
    }


type alias Location =
    ( Int, Int )


model : Model
model =
    { board = Dict.empty --Board.testBoard
    , falling = Tetromino.i
    , bag = [ Tetromino.o            
            , Tetromino.t
            , Tetromino.s
            , Tetromino.z
            , Tetromino.j
            , Tetromino.l
            ]
    , score = 0
    }

init : (Model, Cmd Msg)
init =
  (model, Cmd.none)



-- Update

type Msg
    = Tick Time
    | KeyMsg Keyboard.KeyCode


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
            if Board.isValid model.board (fall model.falling) then
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
                }, Cmd.none)

        KeyMsg code ->
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
                90 ->
                    if Board.isValid model.board (Tetromino.rotate -1 model.falling) then
                        ({ model | falling = Tetromino.rotate -1 model.falling }, Cmd.none)
                    else
                        (model, Cmd.none)
                88 ->
                    if Board.isValid model.board (Tetromino.rotate 1 model.falling) then
                        ({ model | falling = Tetromino.rotate 1 model.falling }, Cmd.none)
                    else
                        (model, Cmd.none)
                _ ->
                    (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
        [ Time.every (100 * millisecond) Tick
        , Keyboard.downs KeyMsg]


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
            [ Html.text (toString model.score)
            ]
        ]


main : Program Never Model Msg
main =
    Html.program 
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions }

