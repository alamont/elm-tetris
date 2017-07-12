module Board exposing (..)

import Block exposing (Block)
import Color exposing (Color)
import Dict exposing (Dict)
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tetromino exposing (Tetromino)


type alias Location =
    ( Int, Int )


type alias Board =
    Dict Location Block


cols : Int
cols =
    10


rows : Int
rows =
    22


baseLineScores : Dict Int Int
baseLineScores =
    Dict.fromList
        [ ( 1, 40 )
        , ( 2, 100 )
        , ( 3, 300 )
        , ( 4, 1200 )
        ]


testBoard : Board
testBoard =
    Dict.fromList
        -- [ ( ( 20, 0 ), Block Color.red )
        [ ( ( 20, 1 ), Block Color.green )
        , ( ( 20, 2 ), Block Color.green )
        , ( ( 20, 3 ), Block Color.green )
        , ( ( 20, 4 ), Block Color.green )
        , ( ( 20, 5 ), Block Color.green )
        , ( ( 20, 6 ), Block Color.green )
        , ( ( 20, 7 ), Block Color.green )
        , ( ( 20, 8 ), Block Color.green )
        , ( ( 20, 9 ), Block Color.green )

        -- , ( ( 21, 0 ), Block Color.red )
        , ( ( 21, 1 ), Block Color.green )
        , ( ( 21, 2 ), Block Color.green )
        , ( ( 21, 3 ), Block Color.green )
        , ( ( 21, 4 ), Block Color.green )
        , ( ( 21, 5 ), Block Color.green )
        , ( ( 21, 6 ), Block Color.green )
        , ( ( 21, 7 ), Block Color.green )
        , ( ( 21, 8 ), Block Color.green )
        , ( ( 21, 9 ), Block Color.green )
        ]


background : Svg msg
background =
    let
        w =
            Block.size * cols |> toString

        h =
            Block.size * rows |> toString
    in
    rect
        [ x "0"
        , y "0"
        , width w
        , height h
        ]
        []


isValid : Board -> Tetromino -> Bool
isValid board tetromino =
    not (isOutOfBounds board tetromino) && not (isIntersecting board tetromino)


isOutOfBounds : Board -> Tetromino -> Bool
isOutOfBounds board tetromino =
    let
        ( col, row ) =
            tetromino.location

        outOfBounds ( blockCol, blockRow ) =
            ((blockRow + row) < 0) || ((blockCol + col) >= cols) || ((blockRow + row) > 26) || ((blockCol + col) < 0)
    in
    List.foldl (||) False (List.map outOfBounds tetromino.shape)


isIntersecting : Board -> Tetromino -> Bool
isIntersecting board tetromino =
    let
        ( col, row ) =
            tetromino.location

        blockLocation ( blockCol, blockRow ) =
            ( blockCol + col, blockRow + row )

        boardSet =
            Set.fromList (Dict.keys board)

        tetrominoSet =
            Set.fromList (List.map blockLocation tetromino.shape)
    in
    not (Set.isEmpty (Set.intersect boardSet tetrominoSet))


addTetromino : Board -> Tetromino -> Board
addTetromino board tetromino =
    let
        ( col, row ) =
            tetromino.location

        blockWithLocation block ( blockCol, blockRow ) =
            ( ( blockCol + col, blockRow + row ), block )
    in
    Dict.union (Dict.fromList (List.map (blockWithLocation tetromino.block) tetromino.shape)) board


checkRow : Int -> Board -> Bool
checkRow row board =
    let
        blocks =
            Dict.filter (\( _, r ) _ -> r == row) board
    in
    Dict.size blocks == cols


clearRow : Int -> Board -> Board
clearRow row board =
    let
        shift ( c, r ) block newBoard =
            if r > row then
                Dict.insert ( c, r - 1 ) block newBoard
            else if r < row then
                Dict.insert ( c, r ) block newBoard
            else
                newBoard
    in
    Dict.foldr shift Dict.empty board


clearLines : Board -> ( Int, Board )
clearLines board =
    let
        score n =
            Maybe.withDefault 0 (Dict.get n baseLineScores)

        clearLine row n newBoard =
            if row == rows then
                ( n, newBoard )
            else if checkRow row newBoard then
                clearLine row (n - 1) (clearRow row newBoard)
            else
                clearLine (row + 1) n newBoard

        ( n, newBoard ) =
            clearLine 0 0 board
    in
    ( score n, newBoard )
