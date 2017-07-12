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
        [ (  1, 0 )
        , (  0, 0 )
        , ( -1, 0 )
        , ( -2, 0 )
        ]
    , block = Block Color.lightBlue
    , location = ( 0, 4 )
    , anchor = ( 0, 0 )
    }
    
o : Tetromino
o =
    { shape =
        [ ( 0, 0 ), ( 1, 0 )
        , ( 0, 1 ), ( 1, 1 )
        ]
    , block = Block Color.yellow
    , location = ( 0, 4 )
    , anchor = ( 0.5, 0.5 )
    }

t : Tetromino
t =
    { shape =
        [            ( -1, 0 )
        , ( 0, -1 ), (  0, 0 ), ( 0, 1 )
        ]
    , block = Block Color.purple
    , location = ( 0, 4 )
    , anchor = ( 0, 0 )
    }

s : Tetromino
s =
    { shape =
        [             (  0, 0 ), ( 0, 1 )
        , ( -1, -1 ), ( -1, 0 )                
        ]
    , block = Block Color.green
    , location = ( 0, 4 )
    , anchor = ( 0, 0 )
    }

z : Tetromino
z =
    { shape =
        [ ( 0, -1 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( -1, 1 )
        ]
    , block = Block Color.red
    , location = ( 0, 4 )
    , anchor = ( 0, 0 )
    }

j : Tetromino
j =
    { shape =
        [ ( -1, -1 )
        , (  0, -1 ), ( 0, 0 ), ( 0, 1 )
        ]
    , block = Block Color.darkBlue
    , location = ( 0, 4 )
    , anchor = ( 0, 0 )
    }

l: Tetromino
l =
    { shape =
        [                      ( -1, 1 )
        , ( 0, -1 ), ( 0, 0 ), (  0, 1 )
        ]
    , block = Block Color.orange
    , location = ( 0, 4 )
    , anchor = ( 0, 0 )
    }

rotate : Int -> Tetromino -> Tetromino
rotate d tetromino =
    let
        (rc, cc) = tetromino.anchor        
        rotLoc (row, col) =
            ( round (-((toFloat col) - cc) * (toFloat d) + rc)
            , round ( ((toFloat row) - rc) * (toFloat d) + cc)
            )
        newShape = List.map rotLoc tetromino.shape
    in
        { tetromino | shape = newShape }
    

render : Tetromino -> Svg msg
render tetromino =
    let 
        ( row, col ) =
            tetromino.location
        blockLocation (blockRow, blockCol) = ((blockRow + row), (blockCol + col))
    in
        Svg.g 
            [] (List.map (Block.render tetromino.block) (List.map blockLocation tetromino.shape))