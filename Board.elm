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


sumLocation : Location -> Location -> Location
sumLocation ( c1, r1 ) ( c2, r2 ) =
    ( c1 + c2, r1 + r2 )


isValid : Board -> Tetromino -> Bool
isValid board tetromino =
    not (isOutOfBounds tetromino) && not (isIntersecting board tetromino)


isOutOfBounds : Tetromino -> Bool
isOutOfBounds tetromino =
    let
        ( col, row ) =
            tetromino.location

        outOfBounds ( blockCol, blockRow ) =
            ((blockRow + row) < 0) || ((blockCol + col) >= cols) || ((blockRow + row) > 25) || ((blockCol + col) < 0)
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

gameOver : Board -> Bool
gameOver board = 
    List.foldl (||) False (Dict.values (Dict.map (\(col, row) _ -> row > rows) board))

rotateWithOffset : Int -> Location -> Tetromino -> Tetromino
rotateWithOffset rot offset tetromino =
    let
        newTetromino =
            Tetromino.rotate rot tetromino
    in
    { newTetromino | location = sumLocation newTetromino.location offset }


kickData_i =
    Dict.fromList
        [ ( (  0,  1 ), [ ( 0, 0 ), ( -2, 0 ), (  1, 0 ), ( -2, -1 ), (  1,  2 ) ] )
        , ( (  1,  0 ), [ ( 0, 0 ), (  2, 0 ), ( -1, 0 ), (  2,  1 ), ( -1, -2 ) ] )
        , ( (  1,  2 ), [ ( 0, 0 ), ( -1, 0 ), (  2, 0 ), ( -1,  2 ), (  2, -1 ) ] )
        , ( (  2,  1 ), [ ( 0, 0 ), (  1, 0 ), ( -2, 0 ), (  1, -2 ), ( -2,  1 ) ] )
        , ( (  2, -1 ), [ ( 0, 0 ), (  2, 0 ), ( -1, 0 ), (  2,  1 ), ( -1, -2 ) ] )
        , ( ( -1,  2 ), [ ( 0, 0 ), ( -2, 0 ), (  1, 0 ), ( -2, -1 ), (  1,  2 ) ] )
        , ( ( -1,  0 ), [ ( 0, 0 ), (  1, 0 ), ( -2, 0 ), (  1, -2 ), ( -2,  1 ) ] )
        , ( (  0, -1 ), [ ( 0, 0 ), ( -1, 0 ), (  2, 0 ), ( -1,  2 ), (  2, -1 ) ] )
        ]

kickData_other =
    Dict.fromList
        [ ( (  0,  1 ), [ ( 0, 0 ), ( -1, 0 ), ( -1,  1 ), ( 0,  2 ), ( -1, -2 ) ] )
        , ( (  1,  0 ), [ ( 0, 0 ), (  1, 0 ), (  1, -1 ), ( 0,  2 ), (  1,  2 ) ] )
        , ( (  1,  2 ), [ ( 0, 0 ), (  1, 0 ), (  1, -1 ), ( 0,  2 ), (  1,  2 ) ] )
        , ( (  2,  1 ), [ ( 0, 0 ), ( -1, 0 ), ( -1,  1 ), ( 0, -2 ), ( -1, -2 ) ] )
        , ( (  2, -1 ), [ ( 0, 0 ), (  1, 0 ), (  1,  1 ), ( 0, -2 ), (  1, -2 ) ] )
        , ( ( -1,  2 ), [ ( 0, 0 ), ( -1, 0 ), ( -1, -1 ), ( 0,  2 ), ( -1,  2 ) ] )
        , ( ( -1,  0 ), [ ( 0, 0 ), ( -1, 0 ), ( -1, -1 ), ( 0,  2 ), ( -1,  2 ) ] )
        , ( (  0, -1 ), [ ( 0, 0 ), (  1, 0 ), (  1,  1 ), ( 0, -2 ), (  1, -2 ) ] )
        ]


kickOffsets : ( Int, Int ) -> Tetromino -> List Location
kickOffsets key tetromino =
    let
        kickData =
            case tetromino.t of
                "i" ->
                    kickData_i

                _ ->
                    kickData_other
    in
    Maybe.withDefault [ ( 0, 0 ) ] (Dict.get key kickData)


rotateTetromino : Board -> Int -> Tetromino -> Tetromino
rotateTetromino board rot tetromino =
    let
        offsets =
            kickOffsets ( tetromino.rotation, Tetromino.addRotation tetromino.rotation rot ) tetromino

        testRotation offset =
            ( isValid board (rotateWithOffset rot offset tetromino), offset )

        validRotation =
            List.head (List.filter (\( valid, _ ) -> valid) (List.map testRotation offsets))
    in
    case validRotation of
        Just ( _, offset ) ->
            rotateWithOffset rot offset tetromino

        Nothing ->
            tetromino


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
                clearLine row (n + 1) (clearRow row newBoard)
            else
                clearLine (row + 1) n newBoard

        ( n, newBoard ) =
            clearLine 0 0 board
    in
    ( score n, newBoard )