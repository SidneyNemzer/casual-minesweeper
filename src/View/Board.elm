module View.Board exposing (view)

import Css
    exposing
        ( px
        , hex
        , em
        , border3
        , padding3
        , backgroundColor
        , fontWeight
        , fontFamily
        , fontSize
        , color
        , listStyle
        )
import Html
import Html.Styled exposing (Html, span, ol, li, text)
import Html.Styled.Attributes exposing (css)
import Matrix exposing (Location, Matrix)
import Data.Board exposing (Square(..), Board)
import View.Colors as Colors


squareWrapper : String -> Html msg
squareWrapper content =
    span
        [ css
            [ border3 (px 1) Css.solid Colors.lightGray
            , padding3 (px 1) (px 10) (px 3)
            , backgroundColor Colors.lightGray
            , fontWeight Css.bold
            , fontFamily Css.monospace
            , fontSize (em 3)
            , color Colors.red
            ]
        ]
        [ text content ]


viewSquare : Square -> Html msg
viewSquare square =
    let
        colored number =
            squareWrapper number
    in
        case square of
            Mine ->
                squareWrapper "*"

            Empty ->
                colored "0"

            Number1 ->
                colored "1"

            Number2 ->
                colored "2"

            Number3 ->
                colored "3"

            Number4 ->
                colored "4"

            Number5 ->
                colored "5"

            Number6 ->
                colored "6"

            Number7 ->
                colored "7"

            Number8 ->
                colored "8"


view : Board -> Html.Html msg
view board =
    Matrix.map viewSquare board
        |> Matrix.toList
        |> List.map (li [])
        |> ol [ css [ listStyle Css.none ] ]
        |> Html.Styled.toUnstyled
