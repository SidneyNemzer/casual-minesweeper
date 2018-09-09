module View.Game exposing (MouseButton(..), board)

import Json.Decode as Decode
import Css
    exposing
        ( px
        , hex
        , em
        , width
        , height
        , border3
        , padding3
        , backgroundColor
        , fontWeight
        , fontFamily
        , fontSize
        , color
        , listStyle
        , cursor
        , display
        , inlineFlex
        , alignItems
        , justifyContent
        , center
        )
import Html
import Html.Styled exposing (Html, span, text, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Char
import Matrix exposing (Location, Matrix)
import Data.Board exposing (Square(..), Board, Point)
import View.Colors as Colors
import Data.Game as Game exposing (Visibility(..))


type MouseButton
    = LeftClick
    | RightClick


nonBreakingSpace : String
nonBreakingSpace =
    Char.fromCode 160 |> String.fromChar


foldMaybeToList : (a -> b) -> Maybe a -> List b
foldMaybeToList attr maybeValue =
    Maybe.map (attr >> List.singleton) maybeValue
        |> Maybe.withDefault []


onRightClick : msg -> Html.Styled.Attribute msg
onRightClick msg =
    Html.Styled.Events.onWithOptions
        "contextmenu"
        { stopPropagation = True
        , preventDefault = True
        }
        (Decode.succeed msg)


squareWrapper : String -> Css.Color -> Maybe (MouseButton -> msg) -> Bool -> Html msg
squareWrapper content textColor clickMsg raised =
    span
        ([ css
            ([ backgroundColor <|
                if raised then
                    Colors.white
                else
                    Colors.black
             , display inlineFlex
             , alignItems center
             , justifyContent center
             , width (px 40)
             , height (px 40)
             , fontFamily Css.monospace
             , fontSize (px 20)
             , color textColor
             , border3 (px 1) Css.solid Colors.lightGray
             , case clickMsg of
                Just _ ->
                    cursor Css.pointer

                Nothing ->
                    cursor Css.default
             ]
                ++ if raised then
                    [ Css.property
                        "box-shadow"
                        (Colors.blackString ++ " 0 0 5px 0")
                    ]
                   else
                    []
            )
         ]
            ++ foldMaybeToList (\msg -> onClick <| msg LeftClick) clickMsg
            ++ foldMaybeToList (\msg -> onRightClick <| msg RightClick) clickMsg
        )
        [ text content ]


viewSquare : Maybe (Point -> MouseButton -> msg) -> Location -> ( Square, Visibility ) -> Html msg
viewSquare pointClickMsg ( y, x ) ( square, visible ) =
    let
        clickMsg =
            Maybe.map (\msg -> msg ( x, y )) pointClickMsg

        staticSquare number color =
            squareWrapper number color Nothing False
    in
        case visible of
            Covered ->
                squareWrapper nonBreakingSpace Colors.white clickMsg True

            Visible ->
                case square of
                    Mine ->
                        squareWrapper "*" Colors.red Nothing False

                    Empty ->
                        squareWrapper nonBreakingSpace Colors.red Nothing False

                    Number1 ->
                        squareWrapper "1" Colors.red Nothing False

                    Number2 ->
                        squareWrapper "2" Colors.red Nothing False

                    Number3 ->
                        squareWrapper "3" Colors.red Nothing False

                    Number4 ->
                        squareWrapper "4" Colors.red Nothing False

                    Number5 ->
                        squareWrapper "5" Colors.red Nothing False

                    Number6 ->
                        squareWrapper "6" Colors.red Nothing False

                    Number7 ->
                        squareWrapper "7" Colors.red Nothing False

                    Number8 ->
                        squareWrapper "8" Colors.red Nothing False

            Flagged ->
                squareWrapper "F" Colors.red clickMsg True


board : Maybe (Point -> MouseButton -> msg) -> Game.GameBoard -> Html.Html msg
board clickMsg gameBoard =
    Matrix.mapWithLocation (viewSquare clickMsg) gameBoard
        |> Matrix.toList
        |> List.map (div [])
        |> div [ css [] ]
        |> Html.Styled.toUnstyled
