module Style exposing
    ( monospaceFont
    , sansFont
    )

import Css exposing (..)


monospaceFont : Style
monospaceFont =
    fontFamilies [ qt "PT Mono", "monospace" ]


sansFont : Style
sansFont =
    fontFamilies [ qt "PT Sans", "sans-serif" ]
