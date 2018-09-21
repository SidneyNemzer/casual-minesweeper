module Form.Settings exposing (view)

import Html.Styled as Html exposing (Html, Attribute, button, text, div, input, label, span)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Css exposing (..)
import Form exposing (Form)
import Form.Error exposing (Error)
import Form.View exposing (ViewConfig, Model, FormConfig, State(..), NumberFieldConfig)
import Style
import View.Colors as Colors


errorMessage : String -> Html msg
errorMessage error =
    div [] [ text error ]


form : FormConfig msg (Html msg) -> Html msg
form { onSubmit, state, action, loading, fields } =
    let
        onSubmitEvent =
            case onSubmit of
                Just msg ->
                    [ Events.onSubmit msg ]

                Nothing ->
                    []
    in
        Html.form
            ([ css
                [ display inlineFlex
                , flexDirection column
                , alignItems stretch
                ]
             ]
                ++ onSubmitEvent
            )
            (List.concat
                [ fields
                , [ case state of
                        Error error ->
                            errorMessage error

                        _ ->
                            text ""
                  , button
                        [ Attributes.type_ "submit"
                        , Attributes.disabled (onSubmit == Nothing)
                        , css
                            [ color Colors.white
                            , backgroundColor Colors.lightGray
                            , border (px 0)
                            , fontSize (px 24)
                            ]
                        ]
                        [ if state == Loading then
                            text loading
                          else
                            text action
                        ]
                  ]
                ]
            )


fromString : (String -> Maybe a) -> Maybe a -> String -> Maybe a
fromString parse currentValue input =
    if String.isEmpty input then
        Nothing
    else
        parse input
            |> Maybe.map Just
            |> Maybe.withDefault currentValue


maybeIf : Bool -> Maybe a -> Maybe a
maybeIf bool maybe =
    if bool then
        maybe
    else
        Nothing


errorToString : Error -> String
errorToString error =
    ""


withMaybeAttribute :
    (a -> Attribute msg)
    -> Maybe a
    -> List (Attribute msg)
    -> List (Attribute msg)
withMaybeAttribute toAttribute maybeValue attrs =
    Maybe.map (toAttribute >> (\attr -> attr :: attrs)) maybeValue
        |> Maybe.withDefault attrs


numberField : NumberFieldConfig msg -> Html msg
numberField { onChange, onBlur, disabled, value, error, showError, attributes } =
    -- span []
    --     [ span
    span
        [ css
            [ displayFlex
            , justifyContent spaceBetween
            ]
        ]
        [ label
            [ css
                [ color Colors.white
                , Style.sansFont
                , fontSize (px 36)
                , fontWeight bold
                , marginRight (px 20)
                ]
            ]
            [ text attributes.label ]
        , input
            ([ Events.onInput (fromString (String.toFloat >> Result.toMaybe) value >> onChange)
             , Attributes.disabled disabled
             , Attributes.value (value |> Maybe.map toString |> Maybe.withDefault "")
             , Attributes.placeholder attributes.placeholder
             , Attributes.type_ "number"
             , Attributes.step (toString attributes.step)
             , css
                [ backgroundColor Colors.gray
                , color Colors.white
                , outline none
                , border (px 0)
                , borderBottom3 (px 3) solid Colors.white
                , fontSize (px 36)
                , width (pct 40)
                ]
             ]
                |> withMaybeAttribute (toString >> Attributes.max) attributes.max
                |> withMaybeAttribute (toString >> Attributes.min) attributes.min
                |> withMaybeAttribute Events.onBlur onBlur
            )
            []
        ]



-- , error
--     |> maybeIf showError
--     |> Maybe.map (errorToString >> errorMessage)
--     |> Maybe.withDefault (text "")
-- ]


view : ViewConfig values msg -> Form values msg -> Model values -> Html msg
view =
    Form.View.custom
        { form = form
        , textField = \_ -> text ""
        , emailField = \_ -> text ""
        , passwordField = \_ -> text ""
        , searchField = \_ -> text ""
        , textareaField = \_ -> text ""
        , numberField = numberField
        , rangeField = \_ -> text ""
        , checkboxField = \_ -> text ""
        , radioField = \_ -> text ""
        , selectField = \_ -> text ""
        , group = div []
        }
