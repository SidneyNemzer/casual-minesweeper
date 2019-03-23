module Minefield exposing
    ( Config
    , GameState(..)
    , Minefield
    , generate
    , toggleFlag
    , uncover
    , undoMineUncover
    , view
    , viewEmpty
    )

import Array
import Css exposing (..)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Matrix exposing (Matrix)
import Point exposing (Point)
import Random exposing (Generator, Seed)
import Square exposing (ClickEvents, Content(..), Square, Visibility(..))



-- TYPES


type alias Minefield =
    Matrix Square


type alias Config =
    { start : Point
    , mines : Int
    , width : Int
    , height : Int
    , seed : Seed
    }


type GameState
    = Setup
    | Playing Minefield
    | EndWin Minefield
    | EndLose Point Minefield



-- CONVERSION


boolToInt : Bool -> Int
boolToInt bool =
    if bool then
        1

    else
        0



-- NAVIGATION


validSquare : Int -> Int -> Point -> Bool
validSquare width height point =
    (point.y > 0)
        && (point.y < height)
        && (point.x > 0)
        && (point.x < height)


validSquaresAround : Int -> Int -> Point -> Int
validSquaresAround width height point =
    let
        validSquareOffset y x =
            validSquare
                width
                height
                { point
                    | y = point.y + y
                    , x = point.x + x
                }
    in
    [ validSquareOffset -1 -1
    , validSquareOffset -1 0
    , validSquareOffset -1 1
    , validSquareOffset 0 -1
    , validSquareOffset 0 0
    , validSquareOffset 0 1
    , validSquareOffset 1 -1
    , validSquareOffset 1 0
    , validSquareOffset 1 1
    ]
        |> List.map boolToInt
        |> List.foldl (+) 0


isAdjacent : Point -> Point -> Bool
isAdjacent point1 point2 =
    let
        equalsWithOffset y x =
            point1
                == { point2
                    | y = point2.y + y
                    , x = point2.x + x
                   }
    in
    equalsWithOffset -1 -1
        || equalsWithOffset -1 0
        || equalsWithOffset -1 1
        || equalsWithOffset 0 -1
        || equalsWithOffset 0 0
        || equalsWithOffset 0 1
        || equalsWithOffset 1 -1
        || equalsWithOffset 1 0
        || equalsWithOffset 1 1


allSafeSquaresUncovered : Minefield -> Bool
allSafeSquaresUncovered minefield =
    Matrix.toIndexedArray minefield
        |> Array.toList
        |> List.any
            (\( _, square ) ->
                square.content /= Mine && square.visibility /= Uncovered
            )
        |> not



-- GENERATION


pointGenerator : Int -> Int -> Generator Point
pointGenerator width height =
    Random.map2 Point (Random.int 0 (height - 1)) (Random.int 0 (width - 1))


nextMine : Point -> Int -> Int -> Seed -> Minefield -> ( Point, Seed )
nextMine start width height oldSeed minefield =
    let
        ( point, seed ) =
            Random.step (pointGenerator width height) oldSeed

        square =
            Matrix.get point minefield
                |> Maybe.withDefault (Square Covered Empty)

        adjacentToStart =
            isAdjacent start point
    in
    if square.content == Mine || adjacentToStart then
        nextMine start width height seed minefield

    else
        ( point, seed )


insertMine : Point -> Minefield -> Minefield
insertMine point minefield =
    let
        incrementSquareOffset y x =
            Matrix.update
                { point
                    | y = point.y + y
                    , x = point.x + x
                }
                Square.increment
    in
    minefield
        |> incrementSquareOffset -1 -1
        |> incrementSquareOffset -1 0
        |> incrementSquareOffset -1 1
        |> incrementSquareOffset 0 -1
        |> Matrix.set point (Square Covered Mine)
        |> incrementSquareOffset 0 1
        |> incrementSquareOffset 1 -1
        |> incrementSquareOffset 1 0
        |> incrementSquareOffset 1 1


generateMine : Point -> Int -> Int -> Int -> Seed -> Minefield -> Minefield
generateMine start mines width height oldSeed oldMinefield =
    let
        ( point, seed ) =
            nextMine start width height oldSeed oldMinefield

        minefield =
            insertMine point oldMinefield
    in
    if mines > 1 then
        generateMine start (mines - 1) width height seed minefield

    else
        minefield


generate : Config -> Minefield
generate { start, mines, width, height, seed } =
    let
        safeSquares =
            validSquaresAround width height start

        area =
            width * height
    in
    Matrix.repeat width height (Square Covered Empty)
        |> generateMine start (min (area - safeSquares) mines) width height seed
        |> uncoverConnectedEmpty start



-- MANIPULATION


uncoverConnectedEmpty : Point -> Minefield -> Minefield
uncoverConnectedEmpty point minefield =
    let
        uncoverConnectedEmptyOffset y x =
            uncoverConnectedEmpty
                { point
                    | y = point.y + y
                    , x = point.x + x
                }

        maybeSquare =
            Matrix.get point minefield

        shouldUncover =
            maybeSquare
                |> Maybe.map Square.isCoveredSafe
                |> Maybe.withDefault False

        shouldUncoverMore =
            maybeSquare
                |> Maybe.map Square.isCoveredEmpty
                |> Maybe.withDefault False
    in
    if shouldUncoverMore then
        minefield
            |> uncoverConnectedEmptyOffset -1 -1
            |> uncoverConnectedEmptyOffset -1 0
            |> uncoverConnectedEmptyOffset -1 1
            |> uncoverConnectedEmptyOffset 0 -1
            |> Matrix.update
                point
                (\square -> { square | visibility = Uncovered })
            |> uncoverConnectedEmptyOffset 0 1
            |> uncoverConnectedEmptyOffset 1 -1
            |> uncoverConnectedEmptyOffset 1 0
            |> uncoverConnectedEmptyOffset 1 1

    else if shouldUncover then
        Matrix.update
            point
            (\square -> { square | visibility = Uncovered })
            minefield

    else
        minefield


uncover : Point -> Minefield -> GameState
uncover point oldMinefield =
    Matrix.get point oldMinefield
        |> Maybe.map
            (\oldSquare ->
                let
                    square =
                        Square.uncover oldSquare

                    minefield =
                        Matrix.set point square oldMinefield
                in
                if square.content == Mine then
                    EndLose point minefield

                else if allSafeSquaresUncovered minefield then
                    EndWin minefield

                else
                    Playing <| uncoverConnectedEmpty point oldMinefield
            )
        |> Maybe.withDefault (Playing oldMinefield)


toggleFlag : Point -> Minefield -> Minefield
toggleFlag point minefield =
    Matrix.update point Square.toggleFlag minefield


undoMineUncover : Point -> Minefield -> Minefield
undoMineUncover point minefield =
    Matrix.set
        point
        { content = Mine, visibility = Flagged }
        minefield



-- VIEW


viewEmpty : ClickEvents msg -> Int -> Int -> Html msg
viewEmpty clickEvents width height =
    Matrix.repeat width height (Square Covered Empty)
        |> view clickEvents


view : ClickEvents msg -> Minefield -> Html msg
view clickEvents minefield =
    Matrix.toIndexed2dList minefield
        |> List.map
            (List.map
                (\( ( x, y ), square ) ->
                    Square.view clickEvents (Point.Point y x) square
                )
            )
        |> List.map (div [ css [ whiteSpace noWrap ] ])
        |> div [ css [ textAlign center, overflow auto, padding (px 10) ] ]
