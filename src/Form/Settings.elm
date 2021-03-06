module Form.Settings exposing (view)

import Css exposing (..)
import Form exposing (Form)
import Form.View exposing (FormConfig, Model, NumberFieldConfig, State(..), ViewConfig)
import Html.Styled as Html exposing (Attribute, Html, button, div, input, label, span, text)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
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
        onSubmitEvent
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
                        , height (px 80)
                        , outline none
                        , cursor pointer
                        , width (pct 100)
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
    let
        errorStyles =
            if error /= Nothing && showError then
                [ borderBottom3 (px 3) solid Colors.red ]

            else
                []
    in
    span
        [ css
            [ displayFlex
            , justifyContent spaceBetween
            , marginBottom (px 50)
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
            ([ Events.onInput (fromString String.toFloat value >> onChange)
             , Attributes.disabled disabled
             , Attributes.value (value |> Maybe.map String.fromFloat |> Maybe.withDefault "")
             , Attributes.placeholder attributes.placeholder
             , Attributes.type_ "number"
             , Attributes.step (String.fromFloat attributes.step)
             , css
                ([ backgroundColor Colors.gray
                 , color Colors.white
                 , outline none
                 , border (px 0)
                 , borderBottom3 (px 3) solid Colors.white
                 , fontSize (px 36)
                 , width (px 200)
                 ]
                    ++ errorStyles
                )
             ]
                |> withMaybeAttribute (String.fromFloat >> Attributes.max) attributes.max
                |> withMaybeAttribute (String.fromFloat >> Attributes.min) attributes.min
                |> withMaybeAttribute Events.onBlur onBlur
            )
            []

        -- TODO show error message?
        ]


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
        , formList = \_ -> text ""
        , formListItem = \_ -> text ""
        , section = \_ _ -> text ""
        , group = div []
        }
