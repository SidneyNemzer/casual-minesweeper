module View.Icons exposing (arrowUp)

import Html.Styled exposing (Html)
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes
    exposing
        ( d
        , fill
        , height
        , stroke
        , strokeWidth
        , viewBox
        , width
        )


arrowUp : Html msg
arrowUp =
    svg [ width "42", height "42", viewBox "0 0 42 24", fill "none" ]
        [ path
            [ d "M1.84338 22.1964L20.9999 3.03992L40.1563 22.1964"
            , stroke "#616161"
            , strokeWidth "3"
            ]
            []
        ]
