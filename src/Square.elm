module Square
    exposing
        ( Square
        , Content(..)
        , Visibility(..)
        , ClickEvents
        , increment
        , toggleFlag
        , uncover
        , isExploded
        , isCoveredSafe
        , isCoveredEmpty
        , view
        )

import Char
import Json.Decode as Decode
import Html.Styled exposing (Html, Attribute, span, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Css exposing (..)
import Point exposing (Point)
import View.Colors as Colors


type Content
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


type Visibility
    = Covered
    | Flagged
    | Uncovered


type alias Square =
    { visibility : Visibility, content : Content }


incrementContent : Content -> Content
incrementContent square =
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


increment : Square -> Square
increment square =
    { square | content = incrementContent square.content }


toggleFlag : Square -> Square
toggleFlag square =
    { square
        | visibility =
            case square.visibility of
                Covered ->
                    Flagged

                Flagged ->
                    Covered

                Uncovered ->
                    Uncovered
    }


uncover : Square -> Square
uncover square =
    { square | visibility = Uncovered }


isExploded : Square -> Bool
isExploded square =
    square.visibility == Uncovered && square.content == Mine


isCoveredSafe : Square -> Bool
isCoveredSafe square =
    case ( square.content, square.visibility ) of
        ( Empty, Covered ) ->
            True

        ( Number1, Covered ) ->
            True

        ( Number2, Covered ) ->
            True

        ( Number3, Covered ) ->
            True

        ( Number4, Covered ) ->
            True

        ( Number5, Covered ) ->
            True

        ( Number6, Covered ) ->
            True

        ( Number7, Covered ) ->
            True

        ( Number8, Covered ) ->
            True

        _ ->
            False


isCoveredEmpty : Square -> Bool
isCoveredEmpty square =
    square.content == Empty && square.visibility == Covered



-- VIEW


type alias ClickEvents msg =
    { uncover : Point -> msg
    , toggleFlag : Point -> msg
    }


nonBreakingSpace : String
nonBreakingSpace =
    Char.fromCode 160 |> String.fromChar


onRightClick : msg -> Attribute msg
onRightClick msg =
    Html.Styled.Events.onWithOptions
        "contextmenu"
        { stopPropagation = True
        , preventDefault = True
        }
        (Decode.succeed msg)


squareBaseStyles : Style
squareBaseStyles =
    Css.batch
        [ display inlineFlex
        , alignItems center
        , justifyContent center
        , width (px 40)
        , height (px 40)
        , fontFamily Css.monospace
        , fontSize (px 20)
        , color Colors.red
        , border3 (px 1) Css.solid Colors.lightGray
        , boxShadow5 (px 0) (px 0) (px 5) (px 0) Colors.black
        ]


viewContent : Content -> Html msg
viewContent content =
    case content of
        Mine ->
            text "*"

        Empty ->
            text nonBreakingSpace

        Number1 ->
            text "1"

        Number2 ->
            text "2"

        Number3 ->
            text "3"

        Number4 ->
            text "4"

        Number5 ->
            text "5"

        Number6 ->
            text "6"

        Number7 ->
            text "7"

        Number8 ->
            text "8"


view : ClickEvents msg -> Point -> Square -> Html msg
view clickEvents point { visibility, content } =
    case visibility of
        Covered ->
            span
                [ css
                    [ squareBaseStyles
                    , backgroundColor Colors.white
                    , cursor Css.pointer
                    , active [ boxShadow none ]
                    ]
                , onClick (clickEvents.uncover point)
                , onRightClick (clickEvents.toggleFlag point)
                ]
                [ text nonBreakingSpace ]

        Flagged ->
            span
                [ css
                    [ squareBaseStyles
                    , backgroundColor Colors.white
                    , cursor Css.pointer
                    ]
                , onRightClick (clickEvents.toggleFlag point)
                ]
                [ text "F" ]

        Uncovered ->
            span
                [ css
                    [ squareBaseStyles
                    , backgroundColor Colors.black
                    ]
                ]
                [ viewContent content ]