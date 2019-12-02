module Settings 
    exposing 
        ( Model
        , Msg
        , default
        , view
        , update
        )

import Html exposing (Html, div)
import Material.TextField exposing (textField, textFieldConfig)

type alias Model =
    { name : String
    , street : String
    , zip_code : String
    , city : String
    , account : String }

default : Model
default = 
    { name = ""
    , street = ""
    , zip_code = ""
    , city = ""
    , account = "00000000000000000000000000"
    }

type Msg 
    = NameChanged String 
    | StreetChanged String 
    | ZipCodeChanged String 
    | CityChanged String
    | AccountChanged String 

update : Msg -> Model -> Model
update msg settings 
    = case msg of
        NameChanged val -> { settings | name = val }
        _ -> settings
            
view : Model -> Html Msg
view settings = 
    div []
        [ textField
            { textFieldConfig
                | label = Just "Nazwa"
                , value = Just settings.name
                , onInput = Just NameChanged 
            }
        ]