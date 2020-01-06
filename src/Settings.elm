module Settings 
    exposing 
        ( Model
        , Msg
        , default
        , view
        , update
        )

import Element exposing 
    ( Element
    , column 
    )
import Element.Input exposing 
    ( text
    , labelLeft 
    )

type alias Model =
    { name : String
    , street : String
    , zip_code : String
    , city : String
    , account : String 
    , vatin : String
    }

default : Model
default = 
    { name = ""
    , street = ""
    , zip_code = ""
    , city = ""
    , account = ""
    , vatin = ""
    }

type Msg 
    = NameChanged String 
    | StreetChanged String 
    | ZipCodeChanged String 
    | CityChanged String
    | AccountChanged String 
    | VatinChanged String

update : Msg -> Model -> Model
update msg settings 
    = case msg of
        NameChanged val -> { settings | name = val }
        StreetChanged val -> { settings | street = val }
        ZipCodeChanged val -> { settings | zip_code = val }
        CityChanged val -> { settings | city = val }
        AccountChanged val -> { settings | account = val }
        VatinChanged val -> { settings | vatin = val }

input : String -> String -> (String -> msg) -> Element msg
input caption val msg =
    text [] 
        { onChange = msg
        , text = val
        , placeholder = Nothing
        , label = labelLeft [] <| Element.text caption
        }

nameField : Model -> Element Msg
nameField  model = 
    input "Nazwa" model.name NameChanged

streetField : Model -> Element Msg
streetField model = 
    input "Ulica" model.street StreetChanged

zipCodeField : Model -> Element Msg
zipCodeField model = 
    input "Kod pocztowy" model.zip_code ZipCodeChanged

cityField : Model -> Element Msg
cityField model = 
    input "Miejscowosc" model.city CityChanged

accountField : Model -> Element Msg
accountField model = 
    input "Numer konta bankowego" model.account AccountChanged

vatinField : Model -> Element Msg
vatinField model = 
    input "VAT" model.vatin VatinChanged

form : Model -> Element Msg
form settings = 
    column []
        [ nameField settings
        , streetField settings
        , zipCodeField settings
        , cityField settings
        , vatinField settings
        , accountField settings
        ] 

view : Model -> Element Msg
view settings = form settings