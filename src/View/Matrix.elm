module View.Matrix exposing (basic, interactive)

import Html
import Html.Styled exposing (Html, li, ol)
import Html.Styled.Attributes exposing (css)
import Css exposing (listStyle)
import Matrix exposing (Matrix)
import Data.Board exposing (Point)


type SquareView
    = Covered
    | Character String Css.Color
    | Empty


basic : (a -> Html msg) -> Matrix a -> Html.Html msg
basic view matrix =
    Matrix.map view matrix
        |> Matrix.toList
        |> List.map (li [])
        |> ol [ css [ listStyle Css.none ] ]
        |> Html.Styled.toUnstyled


interactive : (Point -> a -> Html msg) -> Matrix a -> Html.Html msg
interactive view matrix =
    Matrix.mapWithLocation (\( y, x ) -> view ( x, y )) matrix
        |> Matrix.toList
        |> List.map (li [])
        |> ol [ css [ listStyle Css.none ] ]
        |> Html.Styled.toUnstyled
