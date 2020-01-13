module Inputs exposing
    ( input 
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

input : Element msg -> String -> (String -> msg) -> Element msg
input label val msg =
    text 
        [ width fill 
        ] 
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = labelLeft [] <| label
        }