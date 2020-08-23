module Matrix exposing
    ( Matrix
    , decoder
    , empty
    , filter
    , fromList
    , get
    , height
    , indexedMap
    , map
    , repeat
    , set
    , toIndexed2dList
    , toIndexedArray
    , update
    , width
    )

{-| A matrix implemention for Elm. Internally it uses a single, flat array,
instead of a 2D array, for speed.

This module is based on eeue56/elm-flat-matrix, which has not been updated for
0.19.

<https://github.com/eeue56/elm-flat-matrix/blob/4.0.0/src/Matrix.elm>

-}

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder)
import Point exposing (Point)


{-| Matrix a has a given size, and data contained within
-}
type alias Matrix a =
    { size : ( Int, Int )
    , data : Array a
    }


{-| Create an empty matrix
-}
empty : Matrix a
empty =
    { size = ( 0, 0 ), data = Array.empty }


{-| Width of a given matrix
-}
width : Matrix a -> Int
width matrix =
    Tuple.first matrix.size


{-| Height of a given matrix
-}
height : Matrix a -> Int
height matrix =
    Tuple.second matrix.size


{-| Create a matrix of a given size `x y` with a default value of `v`
-}
repeat : Int -> Int -> a -> Matrix a
repeat x y v =
    { size = ( x, y )
    , data = Array.repeat (x * y) v
    }


{-| Decodes nested JSON arrays into a matrix
-}
decoder : Decoder a -> Decoder (Matrix a)
decoder valueDecoder =
    Decode.list (Decode.list valueDecoder)
        |> Decode.andThen
            (\list2d ->
                case fromList list2d of
                    Just matrix ->
                        Decode.succeed matrix

                    Nothing ->
                        Decode.fail "Failed to decode matrix because arrays are not consistently sized"
            )


{-| Create a matrix from a list of lists.
If the lists within the list are not consistently sized, return `Nothing`
Otherwise return a matrix with the size as the size of the outer and nested lists.
The outer list represents the y axis and inner lists represent the x axis.
Eg:

    [ [ { x = 0, y = 0 }, { x = 1, y = 0 }, { x = 2, y = 0 } ]
    , [ { x = 0, y = 1 }, { x = 1, y = 1 }, { x = 2, y = 1 } ]
    , [ { x = 0, y = 2 }, { x = 1, y = 2 }, { x = 2, y = 2 } ]
    ]

-}
fromList : List (List a) -> Maybe (Matrix a)
fromList list =
    let
        -- the number of elements in the top level list is taken as height
        height_ =
            List.length list

        -- the number of elements in the first element is taken as the width
        width_ =
            List.length <|
                case List.head list of
                    Just x ->
                        x

                    Nothing ->
                        []

        -- ensure that all "rows" are the same size
        allSame =
            List.isEmpty <| List.filter (\x -> List.length x /= width_) list
    in
    if not allSame then
        Nothing

    else
        Just { size = ( width_, height_ ), data = Array.fromList <| List.concat list }


{-| Get a value from a given `x y` and return `Just v` if it exists
Otherwise `Nothing`
-}
get : Point -> Matrix a -> Maybe a
get { x, y } matrix =
    let
        pos =
            (y * width matrix) + x
    in
    if (x < width matrix && x > -1) && (y < height matrix && y > -1) then
        Array.get pos matrix.data

    else
        Nothing


{-| Set a value at a given `i, j` in the matrix and return the new matrix
If the `i, j` is out of bounds then return the unmodified matrix
-}
set : Point -> a -> Matrix a -> Matrix a
set { x, y } v matrix =
    let
        pos =
            (y * Tuple.first matrix.size) + x
    in
    if (x < width matrix && x > -1) && (y < height matrix && y > -1) then
        { matrix | data = Array.set pos v matrix.data }

    else
        matrix


{-| Update an element at `x, y` with the given update function
If out of bounds, return the matrix unchanged
-}
update : Point -> (a -> a) -> Matrix a -> Matrix a
update point f matrix =
    case get point matrix of
        Nothing ->
            matrix

        Just v ->
            set point (f v) matrix


{-| Apply a function of every element in the matrix
-}
map : (a -> b) -> Matrix a -> Matrix b
map f matrix =
    { size = matrix.size, data = Array.map f matrix.data }


{-| Apply a function, taking the `x, y` of every element in the matrix
-}
indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap f matrix =
    let
        f_ i v =
            let
                x =
                    remainderBy (width matrix) i

                y =
                    i // width matrix
            in
            f x y v
    in
    { size = matrix.size
    , data = Array.fromList <| List.indexedMap f_ <| Array.toList matrix.data
    }


{-| Keep only elements that return `True` when passed to the given function f
-}
filter : (a -> Bool) -> Matrix a -> Array a
filter f matrix =
    Array.filter f matrix.data


{-| Convert a matrix to an indexed array
-}
toIndexedArray : Matrix a -> Array ( ( Int, Int ), a )
toIndexedArray matrix =
    (indexedMap (\x y v -> ( ( x, y ), v )) matrix).data


toIndexed2dList : Matrix a -> List (List ( ( Int, Int ), a ))
toIndexed2dList matrix =
    let
        ( width_, height_ ) =
            matrix.size
    in
    List.range 0 height_
        |> List.foldl
            (\y result ->
                (toIndexedArray matrix
                    |> Array.slice (width_ * y) (width_ * y + width_)
                    |> Array.toList
                )
                    :: result
            )
            []
