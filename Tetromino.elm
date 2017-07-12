module Tetromino exposing (..)

import Block exposing (Block)
import Color exposing (Color)
import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Location =
    ( Int, Int )

type alias Tetromino =
    { shape : List Location
    , block : Block
    , location : Location
    , anchor : (Float, Float)
    }

i : Tetromino
i =
    { shape =
        [ ( 0, 2 ), (  1, 2 ), ( 2, 2 ), ( 3, 2 )
        ]
    , block = Block Color.lightBlue
    , location = ( 3, 23 )
    , anchor = ( 1.5, 1.5 )
    }
    
o : Tetromino
o =
    { shape =
        [ ( 1, 1 ), ( 2, 1 )
        , ( 1, 2 ), ( 2, 2 )
        ]
    , block = Block Color.yellow
    , location = ( 3, 22 )
    , anchor = ( 1.5, 1.5 )
    }

t : Tetromino
t =
    { shape =
        [            ( 1, 2 )
        , ( 0, 1 ), (  1, 1 ), ( 2, 1 )
        ]
    , block = Block Color.purple
    , location = ( 3, 22 )
    , anchor = ( 1, 1 )
    }

s : Tetromino
s =
    { shape =
        [           ( 1, 2 ), ( 2, 2 )
        , ( 0, 1 ), ( 1, 1 )                
        ]
    , block = Block Color.green
    , location = ( 3, 22 )
    , anchor = ( 1, 1 )
    }

z : Tetromino
z =
    { shape =
        [ ( 0, 2 ), ( 1, 2 )
        ,           ( 1, 1 ), ( 2, 1 )
        ]
    , block = Block Color.red
    , location = ( 3, 22 )
    , anchor = ( 1, 1 )
    }

j : Tetromino
j =
    { shape =
        [ ( 0, 2 )
        , ( 0, 1 ), ( 1, 1 ), ( 2, 1 )
        ]
    , block = Block Color.darkBlue
    , location = ( 3, 22 )
    , anchor = ( 1, 1 )
    }

l: Tetromino
l =
    { shape =
        [                     ( 2, 2 )
        , ( 0, 1 ), ( 1, 1 ), ( 2, 1 )
        ]
    , block = Block Color.orange
    , location = ( 3, 22 )
    , anchor = ( 1, 1 )
    }

kickLeft : Tetromino -> Tetromino
kickLeft tetromino =
    let
        (col, row) = tetromino.location
        maxCol = List.maximum (List.map (\(_, c) -> c + col) tetromino.shape)
        shiftCols = (Maybe.withDefault 0 maxCol) - 9
    in
        if shiftCols > 0 then
            { tetromino | location = (row, col - shiftCols)}
        else
            tetromino

kickRight : Tetromino -> Tetromino
kickRight tetromino =
    let
        (col, row) = tetromino.location
        minCol = List.minimum (List.map (\(c, _) -> c + col) tetromino.shape)
        shiftCols = (Maybe.withDefault 0 minCol)
    in
        if shiftCols < 0 then
            { tetromino | location = (row, col - shiftCols)}
        else
            tetromino


-- Should use SRS instead (https://tetris.wiki/SRS)
rotate : Int -> Tetromino -> Tetromino
rotate d tetromino =
    let
        (cc, rc) = tetromino.anchor        
        rotLoc (col, row) =
            ( round (-((toFloat row) - rc) * (toFloat d) + cc)
            , round ( ((toFloat col) - cc) * (toFloat d) + rc)
            )
        newShape = List.map rotLoc tetromino.shape
    in
        { tetromino | shape = newShape } |> kickLeft |> kickRight
    

render : Tetromino -> Svg msg
render tetromino =
    let 
        ( col, row ) =
            tetromino.location
        blockLocation (blockCol, blockRow) = ( (blockCol + col), (blockRow + row))
    in
        Svg.g 
            [] (List.map (Block.render tetromino.block) (List.map blockLocation tetromino.shape))