module Tetromino exposing (..)

import Block exposing (Block)
import Color exposing (Color)
import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Location =
    ( Int, Int )

-- jlstz_wallkick_data = 

type alias Tetromino =
    { shape : List Location
    , block : Block
    , location : Location
    , anchor : (Float, Float)
    , rotation : Int
    , t : String
    }

i : Tetromino
i =
    { shape =
        [ ( 0, 2 ), (  1, 2 ), ( 2, 2 ), ( 3, 2 )
        ]
    , block = Block Color.lightBlue
    , location = ( 3, 23 )
    , rotation = 0
    , anchor = ( 1.5, 1.5 )
    , t = "i"
    }
    
o : Tetromino
o =
    { shape =
        [ ( 1, 1 ), ( 2, 1 )
        , ( 1, 2 ), ( 2, 2 )
        ]
    , block = Block Color.yellow
    , location = ( 3, 22 )
    , rotation = 0
    , anchor = ( 1.5, 1.5 )
    , t = "o"
    }

t : Tetromino
t =
    { shape =
        [            ( 1, 2 )
        , ( 0, 1 ), (  1, 1 ), ( 2, 1 )
        ]
    , block = Block Color.purple
    , location = ( 3, 22 )
    , rotation = 0
    , anchor = ( 1, 1 )
    , t = "t"
    }

s : Tetromino
s =
    { shape =
        [           ( 1, 2 ), ( 2, 2 )
        , ( 0, 1 ), ( 1, 1 )                
        ]
    , block = Block Color.green
    , location = ( 3, 22 )
    , rotation = 0
    , anchor = ( 1, 1 )
    , t = "s"
    }

z : Tetromino
z =
    { shape =
        [ ( 0, 2 ), ( 1, 2 )
        ,           ( 1, 1 ), ( 2, 1 )
        ]
    , block = Block Color.red
    , location = ( 3, 22 )
    , rotation = 0
    , anchor = ( 1, 1 )
    , t = "z"
    }

j : Tetromino
j =
    { shape =
        [ ( 0, 2 )
        , ( 0, 1 ), ( 1, 1 ), ( 2, 1 )
        ]
    , block = Block Color.darkBlue
    , location = ( 3, 22 )
    , rotation = 0
    , anchor = ( 1, 1 )
    , t = "j"
    }

l: Tetromino
l =
    { shape =
        [                     ( 2, 2 )
        , ( 0, 1 ), ( 1, 1 ), ( 2, 1 )
        ]
    , block = Block Color.orange
    , location = ( 3, 22 )
    , rotation = 0
    , anchor = ( 1, 1 )
    , t = "l"
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

-- wallKick : Tetromino -> Tetromino
-- wallKick tetromino =
    

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
        { tetromino | shape = newShape
                    , rotation = addRotation tetromino.rotation d } |> kickLeft |> kickRight
    

addRotation : Int -> Int -> Int
addRotation orientation rot =
    let 
        newRotation = orientation + rot
    in
        if (newRotation >= 3) then
            -1
        else if (newRotation <= -2) then
            1
        else
            newRotation
            

render : Tetromino -> Svg msg
render tetromino =
    let 
        ( col, row ) =
            tetromino.location
        blockLocation (blockCol, blockRow) = ( (blockCol + col), (blockRow + row))
    in
        Svg.g 
            [] (List.map (Block.render tetromino.block) (List.map blockLocation tetromino.shape))