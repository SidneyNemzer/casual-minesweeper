module View.Colors exposing (black, blackString, gray, lightGray, red, text, white)

import Css exposing (hex)


lightGray : Css.Color
lightGray =
    hex "#8F8F8F"


red : Css.Color
red =
    hex "#E65A5A"


blackString : String
blackString =
    "#000000"


black : Css.Color
black =
    hex blackString


gray : Css.Color
gray =
    hex "#616161"


white : Css.Color
white =
    hex "#ffffff"


text : Css.Color
text =
    hex "#545454"
