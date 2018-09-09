module View.Game exposing (MouseButton(..), board)

import Json.Decode as Decode
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
        , cursor
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
squareWrapper content setColor clickMsg raised =
    span
        ([ css
            ([ padding3 (px 1) (px 10) (px 3)
             , backgroundColor Colors.lightGray
             , fontWeight Css.bold
             , fontFamily Css.monospace
             , fontSize (em 3)
             , color setColor
             , border3 (px 1) Css.solid Colors.gray
             ]
                ++ [ case clickMsg of
                        Just _ ->
                            cursor Css.pointer

                        Nothing ->
                            cursor Css.default
                   ]
                ++ if raised then
                    [ Css.property "box-shadow" "inset #7b7b7b -3px -3px 0px 1px, inset white 3px 3px 0px 2px" ]
                   else
                    []
            )
         ]
            ++ foldMaybeToList (\msg -> onClick <| msg LeftClick) clickMsg
            ++ foldMaybeToList (\msg -> onRightClick <| msg RightClick) clickMsg
        )
        [ text content ]


viewSquare : Maybe (Point -> MouseButton -> msg) -> Location -> ( Square, Visibility ) -> Html msg
viewSquare clickMsg ( y, x ) ( square, visible ) =
    let
        staticSquare number color =
            squareWrapper number color Nothing False
    in
        case visible of
            Covered ->
                squareWrapper nonBreakingSpace Colors.gray (Maybe.map (\msg -> msg ( x, y )) clickMsg) True

            Visible ->
                case square of
                    Mine ->
                        staticSquare "*" Colors.black

                    Empty ->
                        staticSquare nonBreakingSpace Colors.lightGray

                    Number1 ->
                        staticSquare "1" Colors.blue

                    Number2 ->
                        staticSquare "2" Colors.green

                    Number3 ->
                        staticSquare "3" Colors.red

                    Number4 ->
                        staticSquare "4" Colors.darkBlue

                    Number5 ->
                        staticSquare "5" Colors.maroon

                    Number6 ->
                        staticSquare "6" Colors.cyan

                    Number7 ->
                        staticSquare "7" Colors.black

                    Number8 ->
                        staticSquare "8" Colors.gray

            Flagged ->
                squareWrapper "F" Colors.red (Maybe.map (\msg -> msg ( x, y )) clickMsg) True


board : Maybe (Point -> MouseButton -> msg) -> Game.GameBoard -> Html.Html msg
board clickMsg gameBoard =
    Matrix.mapWithLocation (viewSquare clickMsg) gameBoard
        |> Matrix.toList
        |> List.map (div [ css [ Css.marginTop (px 6) ] ])
        |> div [ css [] ]
        |> Html.Styled.toUnstyled
