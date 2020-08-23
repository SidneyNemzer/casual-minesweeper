module Main exposing (main)

import Browser
import Css exposing (..)
import Css.Transitions exposing (transition)
import Form exposing (Form)
import Form.Settings
import Form.View
import Html.Styled exposing (Html, a, div, h1, span, text)
import Html.Styled.Attributes as HA exposing (css, href)
import Html.Styled.Events exposing (onClick)
import Minefield exposing (GameState(..))
import Point exposing (Point)
import Random exposing (Seed)
import Square exposing (ClickEvents)
import Style
import View.Colors as Colors
import View.Icons


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Flags =
    { seed : Int }


type alias SettingsValues =
    { mines : Maybe Float
    , width : Maybe Float
    , height : Maybe Float
    }


type alias SettingsForm =
    Form.View.Model SettingsValues


type alias Model =
    { seed : Seed
    , mines : Int
    , width : Int
    , height : Int
    , settings : SettingsForm
    , showSettings : Bool
    , state : GameState
    , confirmingRestart : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init { seed } =
    ( { seed = Random.initialSeed seed
      , mines = 100
      , width = 24
      , height = 15
      , settings =
            Form.View.idle
                { mines = Just 100
                , width = Just 24
                , height = Just 15
                }
      , showSettings = False
      , state = Setup
      , confirmingRestart = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ResetGame
    | Uncover Point
    | Flag Point
    | UndoUncover
    | ToggleSettings
    | SettingsChanged SettingsForm
    | RecreateBoard Int Int Int
    | CancelRestart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetGame ->
            ( { model | state = Setup }, Cmd.none )

        Uncover point ->
            ( case model.state of
                Setup ->
                    let
                        ( gameState, nextSeed ) =
                            Minefield.generate
                                { start = point
                                , mines = model.mines
                                , width = model.width
                                , height = model.height
                                , seed = model.seed
                                }
                    in
                    { model | state = gameState, seed = nextSeed }

                Playing minefield ->
                    { model | state = Minefield.uncover point minefield }

                _ ->
                    model
            , Cmd.none
            )

        Flag point ->
            case model.state of
                Playing minefield ->
                    ( { model
                        | state =
                            Playing <| Minefield.toggleFlag point minefield
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UndoUncover ->
            case model.state of
                EndLose point minefield ->
                    ( { model
                        | state =
                            Playing <| Minefield.undoMineUncover point minefield
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ToggleSettings ->
            ( { model | showSettings = not model.showSettings }
            , Cmd.none
            )

        SettingsChanged form ->
            ( { model | settings = form }
            , Cmd.none
            )

        RecreateBoard mines width height ->
            case model.state of
                Setup ->
                    ( { model
                        | mines = mines
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
                        | mines = mines
                        , width = width
                        , height = height
                        , state = Setup
                        , showSettings = False
                      }
                    , Cmd.none
                    )

                EndLose _ _ ->
                    ( { model
                        | mines = mines
                        , width = width
                        , height = height
                        , state = Setup
                        , showSettings = False
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
        minesField =
            Form.numberField
                { parser = Maybe.map Basics.round >> Result.fromMaybe ""
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
                { parser = Maybe.map Basics.round >> Result.fromMaybe ""
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
                { parser = Maybe.map Basics.round >> Result.fromMaybe ""
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
        |> Form.append minesField
        |> Form.append widthField
        |> Form.append heightField


button : Msg -> String -> Html Msg
button clickMsg content =
    span
        [ css
            [ fontWeight bold
            , fontSize (px 24)
            , Style.sansFont
            , color Colors.text
            , cursor pointer
            ]
        , onClick clickMsg
        ]
        [ text content ]


lightButton : Msg -> String -> Html Msg
lightButton clickMsg content =
    span
        [ css
            [ fontWeight bold
            , fontSize (px 24)
            , Style.sansFont
            , color Colors.white
            , cursor pointer
            ]
        , onClick clickMsg
        ]
        [ text content ]


title : Css.Color -> Html msg
title textColor =
    h1
        [ css
            [ textAlign center
            , Style.monospaceFont
            , letterSpacing (em 0.62)
            , fontSize (px 48)
            , color textColor
            , fontWeight (int 300)
            ]
        ]
        [ text "Casual Minesweeper" ]


menuButtonLarge : Html Msg
menuButtonLarge =
    div
        [ css
            [ display inlineFlex
            , flexDirection column
            , alignItems center
            , borderRadius (pct 50)
            , width (px 90)
            , height (px 90)
            , color Colors.text
            , cursor pointer
            , transition
                [ Css.Transitions.backgroundColor 200
                , Css.Transitions.color 200
                , Css.Transitions.boxShadow 200
                ]
            , hover
                [ backgroundColor Colors.gray
                , color Colors.white
                , boxShadow5 zero zero (px 30) (px -5) Colors.gray
                ]
            ]
        , onClick ToggleSettings
        ]
        [ View.Icons.arrowUp
        , span
            [ css
                [ fontWeight bold
                , fontSize (px 24)
                , Style.sansFont
                ]
            ]
            [ text "MENU" ]
        ]


menuButtonSmall : Html Msg
menuButtonSmall =
    span
        [ css
            [ fontWeight bold
            , fontSize (px 24)
            , Style.sansFont
            , color Colors.text
            , cursor pointer
            ]
        , onClick ToggleSettings
        ]
        [ text "MENU" ]


viewSettings : Model -> Html Msg
viewSettings model =
    div
        [ css
            [ backgroundColor Colors.gray
            , height (pct 100)
            , overflow auto
            , displayFlex
            , flexDirection column
            , alignItems center
            ]
        ]
        [ title Colors.white
        , div
            [ css
                [ display inlineFlex
                , flexDirection column
                , alignItems stretch
                , justifyContent center
                , flexGrow (int 1)
                , flexShrink (int 0)
                , textAlign center
                ]
            ]
            [ div [ css [ marginBottom (px 20) ] ]
                [ Form.Settings.view
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
                ]
            , lightButton CancelRestart "CANCEL"
            ]
        , span
            [ css
                [ color Colors.white
                , Style.sansFont
                , marginBottom (px 10)
                ]
            ]
            [ text "AN "
            , a
                [ css [ color Colors.white ]
                , href "https://github.com/SidneyNemzer/casual-minesweeper"
                , HA.target "blank_"
                ]
                [ text "OPEN SOURCE PROJECT" ]
            , text " CREATED BY "
            , a
                [ css [ color Colors.white ]
                , href "https://github.com/SidneyNemzer"
                , HA.target "blank_"
                ]
                [ text "SIDNEY NEMZER" ]
            ]
        , span
            [ css
                [ color Colors.white
                , Style.sansFont
                , marginBottom (px 20)
                ]
            ]
            [ text "DESIGN AND ARTWORK BY "
            , a
                [ css [ color Colors.white ]
                , href "https://github.com/kevinbehan"
                , HA.target "blank_"
                ]
                [ text "KEVIN BEHAN" ]
            ]
        ]


events : ClickEvents Msg
events =
    { uncover = Uncover
    , toggleFlag = Flag
    }


buttons : List (Html Msg) -> Html Msg
buttons =
    div
        [ css
            [ position absolute
            , bottom (px 0)
            , left (px 0)
            , right (px 0)
            , marginBottom (px 30)
            , textAlign center
            ]
        ]


spanMarginRight : Html Msg -> Html Msg
spanMarginRight child =
    span [ css [ marginRight (px 20) ] ] [ child ]


view : Model -> Html Msg
view model =
    if model.showSettings then
        viewSettings model

    else
        div [] <|
            case model.state of
                Setup ->
                    [ title Colors.text
                    , Minefield.viewEmpty events model.width model.height
                    , buttons [ menuButtonLarge ]
                    ]

                Playing minefield ->
                    [ title Colors.text
                    , Minefield.view events minefield
                    , buttons
                        [ spanMarginRight (button ResetGame "RESTART")
                        , menuButtonSmall
                        ]
                    ]

                EndWin minefield ->
                    [ title Colors.text
                    , Minefield.viewMinesFlagged events minefield
                    , buttons
                        [ spanMarginRight (button ResetGame "NEW GAME")
                        , menuButtonSmall
                        ]
                    ]

                EndLose _ minefield ->
                    [ title Colors.text
                    , Minefield.view events minefield
                    , buttons
                        [ spanMarginRight (button ResetGame "NEW GAME")
                        , spanMarginRight (button UndoUncover "UNDO")
                        , menuButtonSmall
                        ]
                    ]
