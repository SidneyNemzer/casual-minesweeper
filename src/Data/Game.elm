module Data.Game
    exposing
        ( GameBoard
        , GameState(..)
        , Visibility(..)
        , generateBoard
        , emptyBoard
        , revealSquare
        , toggleFlag
        , undoUncover
        )

import Matrix exposing (Location, Matrix)
import Data.Board as Board exposing (Square(..), Point)


type Visibility
    = Covered
    | Visible
    | Flagged


type alias GameBoard =
    Matrix ( Square, Visibility )


type GameState
    = Setup
    | Playing GameBoard
    | EndWin GameBoard
    | EndLose Point GameBoard


generateBoard : Board.Config -> Result String GameBoard
generateBoard config =
    let
        board =
            Board.generate config
                |> Result.map
                    (Matrix.map (\square -> ( square, Covered )))
    in
        case config.center of
            Just center ->
                Result.map (revealConnectedSquares center) board

            Nothing ->
                board


revealConnectedSquares : Point -> GameBoard -> GameBoard
revealConnectedSquares ( x, y ) board =
    let
        maybeSquare =
            Matrix.get ( y, x ) board
    in
        case maybeSquare of
            Just ( Empty, Covered ) ->
                board
                    |> Matrix.update ( y, x ) (Tuple.mapSecond (always Visible))
                    |> revealConnectedSquares ( x - 1, y - 1 )
                    |> revealConnectedSquares ( x, y - 1 )
                    |> revealConnectedSquares ( x + 1, y - 1 )
                    |> revealConnectedSquares ( x - 1, y )
                    |> revealConnectedSquares ( x + 1, y )
                    |> revealConnectedSquares ( x - 1, y + 1 )
                    |> revealConnectedSquares ( x, y + 1 )
                    |> revealConnectedSquares ( x + 1, y + 1 )

            Just ( Number1, Covered ) ->
                Matrix.update ( y, x ) (Tuple.mapSecond (always Visible)) board

            Just ( Number2, Covered ) ->
                Matrix.update ( y, x ) (Tuple.mapSecond (always Visible)) board

            Just ( Number3, Covered ) ->
                Matrix.update ( y, x ) (Tuple.mapSecond (always Visible)) board

            Just ( Number4, Covered ) ->
                Matrix.update ( y, x ) (Tuple.mapSecond (always Visible)) board

            Just ( Number5, Covered ) ->
                Matrix.update ( y, x ) (Tuple.mapSecond (always Visible)) board

            Just ( Number6, Covered ) ->
                Matrix.update ( y, x ) (Tuple.mapSecond (always Visible)) board

            Just ( Number7, Covered ) ->
                Matrix.update ( y, x ) (Tuple.mapSecond (always Visible)) board

            Just ( Number8, Covered ) ->
                Matrix.update ( y, x ) (Tuple.mapSecond (always Visible)) board

            _ ->
                board


revealSquare : Point -> GameBoard -> GameState
revealSquare ( x, y ) oldBoard =
    -- TODO don't reveal flagged squares
    let
        board =
            Matrix.update
                ( y, x )
                (Tuple.mapSecond (always Visible))
                oldBoard
    in
        Matrix.get ( y, x ) board
            |> Maybe.map
                (\( square, _ ) ->
                    case square of
                        Mine ->
                            EndLose ( x, y ) board

                        Empty ->
                            Playing <| revealConnectedSquares ( x, y ) oldBoard

                        Number1 ->
                            Playing board

                        Number2 ->
                            Playing board

                        Number3 ->
                            Playing board

                        Number4 ->
                            Playing board

                        Number5 ->
                            Playing board

                        Number6 ->
                            Playing board

                        Number7 ->
                            Playing board

                        Number8 ->
                            Playing board
                )
            |> Maybe.withDefault (Playing oldBoard)


toggleFlag : Point -> GameBoard -> GameBoard
toggleFlag ( x, y ) gameBoard =
    Matrix.update
        ( y, x )
        (Tuple.mapSecond
            (\visibility ->
                case visibility of
                    Covered ->
                        Flagged

                    Visible ->
                        Visible

                    Flagged ->
                        Covered
            )
        )
        gameBoard


emptyBoard : Int -> Int -> GameBoard
emptyBoard width height =
    Board.empty width height
        |> Matrix.map (\square -> ( square, Covered ))


undoUncover : Point -> GameBoard -> GameBoard
undoUncover ( x, y ) gameBoard =
    Matrix.update
        ( y, x )
        (Tuple.mapSecond
            (always Covered)
        )
        gameBoard
