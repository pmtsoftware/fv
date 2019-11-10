import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Material.Typography as Typography
import Material.Button exposing (buttonConfig, raisedButton)

main =
    Browser.sandbox { init = 0, update = update, view = view }


type Msg = Inc | Dec

update msg model = 
    case msg of 
        Inc -> model + 1
        Dec -> model - 1

view model = 
    div [ Typography.typography ]
        [ raisedButton { buttonConfig | onClick = Just Dec } "Decrement"
        , div [] [ text (String.fromInt model) ]
        , raisedButton { buttonConfig | onClick = Just Inc } "Increment"
        ]