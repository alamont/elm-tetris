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


size : Int
size =
    25


render : Block -> Location -> Svg msg
render block ( blockRow, blockCol ) =
    rect
        [ x (toString (blockCol * size))
        , y (toString (blockRow * size))
        , width (toString size)
        , height (toString size)
        , fill (colorToHex block.color)
        , stroke "black"
        ]
        []
