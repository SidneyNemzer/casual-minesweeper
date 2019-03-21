module Point exposing (Point, fromTuple, toTuple)


type alias Point =
    { y : Int
    , x : Int
    }


toTuple : Point -> ( Int, Int )
toTuple point =
    ( point.y, point.x )


fromTuple : ( Int, Int ) -> Point
fromTuple ( y, x ) =
    Point y x
