module Main exposing (main)

import Data.Board exposing (Point)
import Data.Game as Game exposing (GameBoard, GameState(..))
import Html
import Html.Styled exposing (Html, div, input, label, text, h1, span)
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
        , fontWeight
        , bold
        , displayFlex
        , flexDirection
        , column
        , position
        , absolute
        , left
        , pct
        , bottom
        , alignItems
        , width
        , height
        , borderRadius
        , hover
        , backgroundColor
        , cursor
        , pointer
        )
import Css.Transitions exposing (transition)
import Random exposing (Generator, Seed)
import Form exposing (Form)
import Form.Value exposing (Value)
import Form.View
import View.Colors as Colors
import View.Game exposing (MouseButton(..))
import View.Icons


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias SettingsValues =
    { seed : Value Float
    , mines : Value Float
    , width : Value Float
    , height : Value Float
    }


type alias SettingsForm =
    Form.View.Model SettingsValues


type alias Model =
    { seed : Int
    , mines : Int
    , width : Int
    , height : Int
    , settings : SettingsForm
    , showSettings : Bool
    , state : GameState
    , confirmingRestart : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { seed = 100
      , mines = 100
      , width = 24
      , height = 15
      , settings =
            Form.View.idle
                { seed = Form.Value.filled 100
                , mines = Form.Value.filled 100
                , width = Form.Value.filled 24
                , height = Form.Value.filled 15
                }
      , showSettings = False
      , state = Setup
      , confirmingRestart = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = StartGame Point
    | ResetGame
    | SquareClicked Point MouseButton
    | UndoUncover
    | ToggleSettings
    | SettingsChanged SettingsForm
    | RecreateBoard Int Int Int Int
    | CancelRestart


userInputToInt : Int -> String -> Int
userInputToInt default =
    String.toInt >> Result.withDefault default


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                        Debug.crash "Game.generateBoard should clamp arguments, caller should check arg range"

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

        ToggleSettings ->
            { model | showSettings = not model.showSettings } ! []

        SettingsChanged form ->
            { model | settings = form } ! []

        RecreateBoard seed mines width height ->
            case model.state of
                Setup ->
                    ( { model
                        | seed = seed
                        , mines = mines
                        , width = width
                        , height = height
                        , showSettings = False
                      }
                    , Cmd.none
                    )

                Playing _ ->
                    if model.confirmingRestart then
                        ( { model
                            | confirmingRestart = False
                            , seed = seed
                            , mines = mines
                            , width = width
                            , height = height
                            , showSettings = False
                            , state = Setup
                          }
                        , Cmd.none
                        )
                    else
                        ( { model | confirmingRestart = True }, Cmd.none )

                EndWin _ ->
                    ( { model
                        | seed = seed
                        , mines = mines
                        , width = width
                        , height = height
                        , state = Setup
                        , showSettings = False
                        , state = Setup
                      }
                    , Cmd.none
                    )

                EndLose _ _ ->
                    ( { model
                        | seed = seed
                        , mines = mines
                        , width = width
                        , height = height
                        , state = Setup
                        , showSettings = False
                        , state = Setup
                      }
                    , Cmd.none
                    )

        CancelRestart ->
            ( { model | confirmingRestart = False, showSettings = False }
            , Cmd.none
            )



-- VIEW


settingsForm : Form SettingsValues Msg
settingsForm =
    let
        seedField =
            Form.numberField
                { parser = round >> Ok
                , value = .seed
                , update = \v r -> { r | seed = v }
                , attributes =
                    { label = "SEED"
                    , placeholder = ""
                    , step = 1
                    , min = Nothing
                    , max = Nothing
                    }
                }

        minesField =
            Form.numberField
                { parser = round >> Ok
                , value = .mines
                , update = \v r -> { r | mines = v }
                , attributes =
                    { label = "MINES"
                    , placeholder = ""
                    , step = 1
                    , min = Just 1
                    , max = Nothing
                    }
                }

        widthField =
            Form.numberField
                { parser = round >> Ok
                , value = .width
                , update = \v r -> { r | width = v }
                , attributes =
                    { label = "WIDTH"
                    , placeholder = ""
                    , step = 1
                    , min = Just 1
                    , max = Nothing
                    }
                }

        heightField =
            Form.numberField
                { parser = round >> Ok
                , value = .height
                , update = \v r -> { r | height = v }
                , attributes =
                    { label = "HEIGHT"
                    , placeholder = ""
                    , step = 1
                    , min = Just 1
                    , max = Nothing
                    }
                }
    in
        Form.succeed RecreateBoard
            |> Form.append seedField
            |> Form.append minesField
            |> Form.append widthField
            |> Form.append heightField


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


sansFont : Style
sansFont =
    fontFamilies [ qt "PT Sans", "sans-serif" ]


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


viewMenuButton : Html Msg
viewMenuButton =
    div
        [ css
            [ displayFlex
            , flexDirection column
            , alignItems center
            , position absolute
            , left (pct 50)
            , bottom (px 20)
            , width (px 90)
            , height (px 90)
            , borderRadius (pct 50)
            , color Colors.text
            , cursor pointer
            , transition
                [ Css.Transitions.backgroundColor 200
                , Css.Transitions.color 200
                ]
            , hover
                [ backgroundColor Colors.black
                , color Colors.white
                ]
            ]
        , onClick ToggleSettings
        ]
        [ View.Icons.arrowUp
        , span
            [ css
                [ fontWeight bold
                , fontSize (px 24)
                , sansFont
                ]
            ]
            [ text "MENU" ]
        ]


viewSettings : Model -> Html Msg
viewSettings model =
    div []
        [ title
        , Html.Styled.fromUnstyled <|
            Form.View.asHtml
                { onChange = SettingsChanged
                , action =
                    case model.state of
                        Setup ->
                            "DONE"

                        Playing _ ->
                            if model.confirmingRestart then
                                "REALLY RESTART?"
                            else
                                "RESTART"

                        EndWin _ ->
                            "DONE"

                        EndLose _ _ ->
                            "DONE"
                , loading = ""
                , validation = Form.View.ValidateOnSubmit
                }
                settingsForm
                model.settings
        , if model.confirmingRestart then
            button CancelRestart "cancel"
          else
            text ""
        ]


view : Model -> Html Msg
view model =
    if model.showSettings then
        viewSettings model
    else
        div [] <|
            case model.state of
                Setup ->
                    [ title
                    , viewEmptyBoard model
                    , viewMenuButton
                    ]

                Playing gameBoard ->
                    [ title
                    , View.Game.board (Just SquareClicked) gameBoard
                    , button ResetGame "Reset"
                    , viewMenuButton
                    ]

                EndWin gameBoard ->
                    [ title
                    , View.Game.board Nothing gameBoard
                    , button ResetGame "New Game"
                    , viewMenuButton
                    ]

                EndLose point gameBoard ->
                    [ title
                    , View.Game.board Nothing gameBoard
                    , button ResetGame "New Game"
                    , button UndoUncover "Undo"
                    , viewMenuButton
                    ]
