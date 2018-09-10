module Data.Board exposing (Square(..), Board, Config, Point, generate, empty)

import Matrix exposing (Location, Matrix)
import Random exposing (Generator, Seed)


type alias Point =
    ( Int, Int )


type alias Config =
    { center : Maybe Point
    , mines : Int
    , width : Int
    , height : Int
    , seed : Seed
    }


type Square
    = Mine
    | Empty
    | Number1
    | Number2
    | Number3
    | Number4
    | Number5
    | Number6
    | Number7
    | Number8


type alias Board =
    Matrix Square


incrementSquare : Square -> Square
incrementSquare square =
    case square of
        Mine ->
            Mine

        Empty ->
            Number1

        Number1 ->
            Number2

        Number2 ->
            Number3

        Number3 ->
            Number4

        Number4 ->
            Number5

        Number5 ->
            Number6

        Number6 ->
            Number7

        Number7 ->
            Number8

        Number8 ->
            Number8


incrementSquareAt : Int -> Int -> Board -> Board
incrementSquareAt x y =
    Matrix.update ( y, x ) incrementSquare


setMineAt : Int -> Int -> Board -> Board
setMineAt x y =
    Matrix.update ( y, x ) (always Mine)


insertMine : Int -> Int -> Board -> Board
insertMine x y board =
    board
        |> incrementSquareAt (x - 1) (y - 1)
        |> incrementSquareAt x (y - 1)
        |> incrementSquareAt (x + 1) (y - 1)
        |> incrementSquareAt (x - 1) y
        |> setMineAt x y
        |> incrementSquareAt (x + 1) y
        |> incrementSquareAt (x - 1) (y + 1)
        |> incrementSquareAt x (y + 1)
        |> incrementSquareAt (x + 1) (y + 1)


nearCenter : Maybe Point -> Point -> Bool
nearCenter center point =
    case center of
        Just ( x, y ) ->
            (point == ( x - 1, y - 1 ))
                || (point == ( x, y - 1 ))
                || (point == ( x + 1, y - 1 ))
                || (point == ( x - 1, y ))
                || (point == ( x, y ))
                || (point == ( x + 1, y ))
                || (point == ( x - 1, y + 1 ))
                || (point == ( x, y + 1 ))
                || (point == ( x + 1, y + 1 ))

        Nothing ->
            False


nextMine : Maybe Point -> Seed -> Generator Location -> Board -> ( Location, Seed )
nextMine center seed locationGen board =
    let
        ( ( x, y ), newSeed ) =
            Random.step locationGen seed

        square =
            Matrix.get ( y, x ) board |> Maybe.withDefault Empty
    in
        if square == Mine || nearCenter center ( x, y ) then
            nextMine center newSeed locationGen board
        else
            ( ( x, y ), newSeed )


generateBoardHelper : Maybe Point -> Int -> Seed -> Generator Location -> Board -> Board
generateBoardHelper center mineCount seed nextSquare board =
    if mineCount >= 1 then
        let
            ( ( x, y ), newSeed ) =
                nextMine center seed nextSquare board

            newBoard =
                insertMine x y board
        in
            generateBoardHelper center (mineCount - 1) newSeed nextSquare newBoard
    else
        board


locationGenerator : Int -> Int -> Generator Location
locationGenerator width height =
    Random.pair (Random.int 0 <| width - 1) (Random.int 0 <| height - 1)


generate : Config -> Result String Board
generate { center, mines, width, height, seed } =
    let
        minimumArea =
            if center /= Nothing then
                mines + 9
            else
                mines
    in
        if width * height < minimumArea then
            Err <|
                toString minimumArea
                    ++ " squares are required but "
                    ++ toString width
                    ++ " by "
                    ++ toString height
                    ++ " squares only has an area of "
                    ++ toString (width * height)
        else
            Matrix.matrix height width (always Empty)
                |> generateBoardHelper center mines seed (locationGenerator width height)
                |> Ok


empty : Int -> Int -> Board
empty width height =
    Matrix.matrix height width (always Empty)
