module Main exposing (main)

import Data.Board exposing (Point)
import Data.Game as Game exposing (GameBoard, GameState(..))
import Html
import Html.Styled exposing (Html, div, input, label, text, h1)
import Html.Styled.Attributes exposing (css, type_, value)
import Html.Styled.Events exposing (onInput, onClick)
import Css
    exposing
        ( Style
        , fontFamilies
        , qt
        , textAlign
        , center
        , letterSpacing
        , em
        , fontSize
        , px
        , color
        )
import Random exposing (Generator, Seed)
import View.Colors as Colors
import View.Game exposing (MouseButton(..))


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { seed : Int
    , mines : Int
    , width : Int
    , height : Int
    , state : GameState
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { seed = 100
      , mines = 100
      , width = 24
      , height = 15
      , state = Setup
      , error = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetSeed String
    | SetMines String
    | SetWidth String
    | SetHeight String
    | StartGame Point
    | ResetGame
    | SquareClicked Point MouseButton
    | UndoUncover


userInputToInt : Int -> String -> Int
userInputToInt default =
    String.toInt >> Result.withDefault default


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSeed userInput ->
            ( { model
                | seed = userInputToInt model.seed userInput
              }
            , Cmd.none
            )

        SetMines userInput ->
            ( { model
                | mines = userInputToInt model.mines userInput
              }
            , Cmd.none
            )

        SetWidth userInput ->
            ( { model
                | width = userInputToInt model.width userInput
              }
            , Cmd.none
            )

        SetHeight userInput ->
            ( { model
                | height = userInputToInt model.height userInput
              }
            , Cmd.none
            )

        StartGame point ->
            let
                generateBoard =
                    Game.generateBoard
                        { center = Just point
                        , mines = model.mines
                        , width = model.width
                        , height = model.height
                        , seed = Random.initialSeed model.seed
                        }
            in
                case generateBoard of
                    Ok board ->
                        ( { model | state = Playing board }, Cmd.none )

                    Err error ->
                        ( { model | error = Just error }, Cmd.none )

        ResetGame ->
            ( { model | state = Setup }, Cmd.none )

        SquareClicked point button ->
            case model.state of
                Playing board ->
                    ( { model
                        | state =
                            case button of
                                LeftClick ->
                                    Game.revealSquare point board

                                RightClick ->
                                    Game.toggleFlag point board
                                        |> Playing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UndoUncover ->
            case model.state of
                EndLose point board ->
                    ( { model
                        | state =
                            Playing <| Game.undoUncover point board
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- VIEW


viewNumberInput : String -> Int -> (String -> Msg) -> Html Msg
viewNumberInput labelText currentInput msg =
    div []
        [ label [] [ text labelText ]
        , input [ value (toString currentInput), onInput msg, type_ "number" ] []
        ]


button : Msg -> String -> Html Msg
button clickMsg content =
    Html.Styled.button
        [ onClick clickMsg ]
        [ text content ]


viewEmptyBoard : Model -> Html Msg
viewEmptyBoard model =
    Game.emptyBoard model.width model.height
        |> View.Game.board (Just (\point button -> StartGame point))


monospaceFont : Style
monospaceFont =
    fontFamilies [ qt "PT Mono", "monospace" ]


title : Html msg
title =
    h1
        [ css
            [ textAlign center
            , monospaceFont
            , letterSpacing (em 0.62)
            , fontSize (px 48)
            , color Colors.text
            ]
        ]
        [ text "Casual Minesweeper" ]


view : Model -> Html Msg
view model =
    let
        main =
            case model.state of
                Setup ->
                    [ title
                    , viewEmptyBoard model
                    , viewNumberInput "Seed" model.seed SetSeed
                    , viewNumberInput "Mines" model.mines SetMines
                    , viewNumberInput "Width" model.width SetWidth
                    , viewNumberInput "Height" model.height SetHeight
                    ]

                Playing gameBoard ->
                    [ title
                    , View.Game.board (Just SquareClicked) gameBoard
                    , button ResetGame "Reset"
                    ]

                EndWin gameBoard ->
                    [ title
                    , View.Game.board Nothing gameBoard
                    , button ResetGame "New Game"
                    ]

                EndLose point gameBoard ->
                    [ title
                    , View.Game.board Nothing gameBoard
                    , button ResetGame "New Game"
                    , button UndoUncover "Undo"
                    ]
    in
        div []
            ((model.error
                |> Maybe.map (text >> List.singleton >> div [] >> List.singleton)
                |> Maybe.withDefault []
             )
                ++ main
            )
