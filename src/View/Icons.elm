module View.Icons exposing (arrowUp, flag)

import Css
import Html.Styled exposing (Html)
import Svg.Styled as Svg exposing (circle, ellipse, line, svg)
import Svg.Styled.Attributes exposing (..)


arrowUp : Html msg
arrowUp =
    svg [ width "42", height "42", viewBox "0 0 42 24", fill "none" ]
        [ Svg.path
            [ d "M1.84338 22.1964L20.9999 3.03992L40.1563 22.1964"
            , stroke "#616161"
            , strokeWidth "3"
            ]
            []
        ]


flag : Html msg
flag =
    svg [ css [ Css.height (Css.pct 100) ], viewBox "0 0 29 44", fill "none" ]
        [ line [ x1 "17.4349", y1 "2.98938", x2 "17.9236", y2 "42.5112", stroke "#616161", strokeLinecap "round" ] []
        , circle [ cx "17.4299", cy "2.49561", r "1.86682", fill "#616161" ] []
        , Svg.path [ d "M1.48326 13.6335C0.821197 13.2477 0.821195 12.2912 1.48326 11.9055L15.7062 3.61799C16.3728 3.22954 17.2096 3.71044 17.2096 4.48201L17.2096 21.0569C17.2096 21.8285 16.3728 22.3094 15.7062 21.921L1.48326 13.6335Z", fill "#F68460" ] []
        , ellipse [ cx "18.0078", cy "41.1642", rx "10.9569", ry "2.50587", fill "#616161" ] []
        , ellipse [ cx "18.0078", cy "39.8758", rx "8.80261", ry "2.01317", fill "#616161" ] []
        , line [ x1 "3.30283", y1 "12.9809", x2 "14.8769", y2 "6.02082", stroke "#EA734D", strokeLinecap "round" ] []
        , line [ x1 "3.23385", y1 "12.9555", x2 "14.9301", y2 "19.7084", stroke "#EA734D", strokeLinecap "round" ] []
        ]
