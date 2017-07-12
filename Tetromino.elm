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
    }

i : Tetromino
i =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( -2, 0 )
        ]
    , block = Block Color.lightBlue
    , location = ( 0, 0 )
    }
    
o : Tetromino
o =
    { shape =
        [ ( 0, 0 )
        , ( 0, 1 )
        , ( 1, 0 )
        , ( 1, 1 )
        ]
    , block = Block Color.yellow
    , location = ( 0, 0 )
    }

t : Tetromino
t =
    { shape =
        [ ( 0, -1 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( 0, 1 )
        ]
    , block = Block Color.purple
    , location = ( 0, 1 )
    }

render : Tetromino -> Svg msg
render tetromino =
    let 
        ( row, col ) =
            tetromino.location
        blockLocation (blockRow, blockCol) = ((blockRow + row), (blockCol + col))
    in
        Svg.g 
            [] (List.map (Block.render tetromino.block) (List.map blockLocation tetromino.shape))