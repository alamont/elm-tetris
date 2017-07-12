module Block exposing (..)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Block =
    { color : Color
    }


type alias Location =
    ( Int, Int )

cols : Int
cols =
    10


rows : Int
rows =
    22

size : Int
size =
    25

render : Block -> Location -> Svg msg
render block ( col, row ) =
    rect
        [ x (toString (col * size))
        , y (toString ((rows - row - 1) * size))
        , width (toString size)
        , height (toString size)
        , fill (colorToHex block.color)
        , stroke "black"
        ]
        []
