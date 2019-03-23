module View.Icons exposing (arrowUp, flag, mine)

import Css
import Html.Styled exposing (Html)
import Svg.Styled as Svg exposing (circle, ellipse, line, svg)
import Svg.Styled.Attributes exposing (..)


arrowUp : Html msg
arrowUp =
    svg
        [ width "42"
        , height "42"
        , viewBox "0 0 42 24"
        , fill "none"
        , Svg.Styled.Attributes.style "stroke: currentColor;"
        ]
        [ Svg.path
            [ d "M1.84338 22.1964L20.9999 3.03992L40.1563 22.1964"
            , strokeWidth "3"
            ]
            []
        ]


oneHundredPctHeight : Html.Styled.Attribute msg
oneHundredPctHeight =
    css [ Css.height (Css.pct 100) ]


flag : Html msg
flag =
    svg [ oneHundredPctHeight, viewBox "0 0 29 44", fill "none" ]
        [ line [ x1 "17.4349", y1 "2.98938", x2 "17.9236", y2 "42.5112", stroke "#616161", strokeLinecap "round" ] []
        , circle [ cx "17.4299", cy "2.49561", r "1.86682", fill "#616161" ] []
        , Svg.path [ d "M1.48326 13.6335C0.821197 13.2477 0.821195 12.2912 1.48326 11.9055L15.7062 3.61799C16.3728 3.22954 17.2096 3.71044 17.2096 4.48201L17.2096 21.0569C17.2096 21.8285 16.3728 22.3094 15.7062 21.921L1.48326 13.6335Z", fill "#F68460" ] []
        , ellipse [ cx "18.0078", cy "41.1642", rx "10.9569", ry "2.50587", fill "#616161" ] []
        , ellipse [ cx "18.0078", cy "39.8758", rx "8.80261", ry "2.01317", fill "#616161" ] []
        , line [ x1 "3.30283", y1 "12.9809", x2 "14.8769", y2 "6.02082", stroke "#EA734D", strokeLinecap "round" ] []
        , line [ x1 "3.23385", y1 "12.9555", x2 "14.9301", y2 "19.7084", stroke "#EA734D", strokeLinecap "round" ] []
        ]


mine : Html msg
mine =
    svg [ oneHundredPctHeight, viewBox "0 0 36 36", fill "none" ]
        [ circle [ cx "17.7398", cy "17.9135", r "14.7439", fill "#616161" ] []
        , ellipse [ cx "17.7399", cy "18.1262", rx "4.61615", ry "17.5395", fill "#616161" ] []
        , ellipse [ cx "17.7399", cy "18.1263", rx "4.61615", ry "17.5395", transform "rotate(-90 17.7399 18.1263)", fill "#616161" ] []
        , ellipse [ cx "17.6981", cy "18", rx "4.61615", ry "17.5395", transform "rotate(-120 17.6981 18)", fill "#616161" ] []
        , ellipse [ rx "4.61615", ry "17.5395", transform "matrix(0.5 -0.866025 -0.866025 -0.5 17.6981 18)", fill "#616161" ] []
        , Svg.path [ d "M7.26685 16.7338C7.06946 13.4439 10.3899 6.95947 17.2592 6.95947", stroke "#A49797", strokeLinecap "round" ] []
        ]
