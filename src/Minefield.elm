module Minefield
    exposing
        ( Minefield
        , Config
        , GameState(..)
        , generate
        , uncover
        , toggleFlag
        , undoMineUncover
        , view
        , viewEmpty
        )

import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Css exposing (..)
import Random exposing (Generator, Seed)
import Matrix exposing (Matrix)
import Point exposing (Point)
import Square exposing (Square, Content(..), Visibility(..), ClickEvents)


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
    (point.row > 0)
        && (point.row < height)
        && (point.column > 0)
        && (point.column < height)


validSquaresAround : Int -> Int -> Point -> Int
validSquaresAround width height point =
    let
        validSquareOffset row column =
            validSquare
                width
                height
                { point
                    | row = point.row + row
                    , column = point.column + column
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
        equalsWithOffset row column =
            point1
                == { point2
                    | row = point2.row + row
                    , column = point2.column + column
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
    Matrix.toList minefield
        |> List.any
            (List.any
                (\square ->
                    square.content /= Mine && square.visibility /= Uncovered
                )
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
            Matrix.get (Point.toLocation point) minefield
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
        incrementSquareOffset row column =
            Matrix.update
                (Point.toLocation
                    { point
                        | row = point.row + row
                        , column = point.column + column
                    }
                )
                Square.increment
    in
        minefield
            |> incrementSquareOffset -1 -1
            |> incrementSquareOffset -1 0
            |> incrementSquareOffset -1 1
            |> incrementSquareOffset 0 -1
            |> Matrix.set (Point.toLocation point) (Square Covered Mine)
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
        if mines > 0 then
            generateMine start (mines - 1) width height seed minefield
        else
            minefield


generate : Config -> Minefield
generate { start, mines, width, height, seed } =
    let
        minimumArea =
            mines + validSquaresAround width height start

        area =
            width * height
    in
        Matrix.matrix height width (always (Square Covered Empty))
            |> generateMine start (min mines minimumArea) width height seed
            |> uncoverConnectedEmpty start



-- MANIPULATION


uncoverConnectedEmpty : Point -> Minefield -> Minefield
uncoverConnectedEmpty point minefield =
    let
        uncoverConnectedEmptyOffset row column =
            uncoverConnectedEmpty
                { point
                    | row = point.row + row
                    , column = point.column + column
                }

        maybeSquare =
            Matrix.get (Point.toLocation point) minefield

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
                    (Point.toLocation point)
                    (\square -> { square | visibility = Uncovered })
                |> uncoverConnectedEmptyOffset 0 1
                |> uncoverConnectedEmptyOffset 1 -1
                |> uncoverConnectedEmptyOffset 1 0
                |> uncoverConnectedEmptyOffset 1 1
        else if shouldUncover then
            Matrix.update
                (Point.toLocation point)
                (\square -> { square | visibility = Uncovered })
                minefield
        else
            minefield


uncover : Point -> Minefield -> GameState
uncover point oldMinefield =
    let
        location =
            Point.toLocation point
    in
        Matrix.get location oldMinefield
            |> Maybe.map
                (\oldSquare ->
                    let
                        square =
                            Square.uncover oldSquare

                        minefield =
                            Matrix.set location square oldMinefield
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
    Matrix.update (Point.toLocation point) Square.toggleFlag minefield


undoMineUncover : Point -> Minefield -> Minefield
undoMineUncover point minefield =
    Matrix.set
        (Point.toLocation point)
        { content = Mine, visibility = Flagged }
        minefield



-- VIEW


viewEmpty : ClickEvents msg -> Int -> Int -> Html msg
viewEmpty clickEvents width height =
    Matrix.matrix height width (always (Square Covered Empty))
        |> view clickEvents


view : ClickEvents msg -> Minefield -> Html msg
view clickEvents minefield =
    Matrix.mapWithLocation
        (\location ->
            Square.view
                clickEvents
                (Point.fromLocation location)
        )
        minefield
        |> Matrix.toList
        |> List.map (div [ css [ whiteSpace noWrap ] ])
        |> div [ css [ textAlign center, overflow auto, padding (px 10) ] ]
