module Point exposing (Point, toLocation, fromLocation)

import Matrix


type alias Point =
    { row : Int
    , column : Int
    }


toLocation : Point -> Matrix.Location
toLocation point =
    ( point.row, point.column )


fromLocation : Matrix.Location -> Point
fromLocation ( row, column ) =
    Point row column
