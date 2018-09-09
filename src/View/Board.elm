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


squareWrapper : String -> Maybe Css.Color -> Html msg
squareWrapper content maybeColor =
    span
        [ css
            [ border3 (px 1) Css.solid Colors.black
            , padding3 (px 1) (px 10) (px 3)
            , backgroundColor Colors.lightGray
            , fontWeight Css.bold
            , fontFamily Css.monospace
            , fontSize (em 3)
            , color (Maybe.withDefault Colors.black maybeColor)
            ]
        ]
        [ text content ]


viewSquare : Square -> Html msg
viewSquare square =
    let
        colored number color =
            squareWrapper number (Just color)
    in
        case square of
            Mine ->
                squareWrapper "*" Nothing

            Empty ->
                colored "0" Colors.lightGray

            Number1 ->
                colored "1" Colors.blue

            Number2 ->
                colored "2" Colors.green

            Number3 ->
                colored "3" Colors.red

            Number4 ->
                colored "4" Colors.darkBlue

            Number5 ->
                colored "5" Colors.maroon

            Number6 ->
                colored "6" Colors.cyan

            Number7 ->
                colored "7" Colors.black

            Number8 ->
                colored "8" Colors.gray


view : Board -> Html.Html msg
view board =
    Matrix.map viewSquare board
        |> Matrix.toList
        |> List.map (li [])
        |> ol [ css [ listStyle Css.none ] ]
        |> Html.Styled.toUnstyled
