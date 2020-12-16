module Inputs exposing
    ( input
    , datePicker
    )

import Element exposing 
    ( Element 
    , width 
    , fill
    )
import Element.Input exposing
    ( text 
    , labelLeft 
    )
import Date exposing 
    ( Date 
    , format
    )
import Element.Border as Border
import Styles as Styles
import DatePicker as DatePicker exposing 
    ( ChangeEvent (..)
    , defaultSettings
    )
import Maybe exposing 
    ( map
    , withDefault
    )

input : Element msg -> String -> (String -> msg) -> Element msg
input label val msg =
    text 
        [ width fill 
        , Border.color <| Styles.theme.primary_light_4
        ] 
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = labelLeft [] <| label
        }

dateFormat : String
dateFormat = "dd-MM-YYYY"

datePicker : Element msg -> Maybe Date -> ( ChangeEvent -> msg ) -> DatePicker.Model -> Element msg 
datePicker label value msg model =
    let toString = format dateFormat
        dateAsString = withDefault "" <| map toString value
    in DatePicker.input 
        []
        { onChange = msg 
        , selected = value
        , text = dateAsString
        , label = labelLeft [] <| label 
        , placeholder = Just <| Element.Input.placeholder [] <| Element.text dateFormat 
        , settings = defaultSettings 
        , model = model 
        }

