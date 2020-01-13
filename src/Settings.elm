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
    , el 
    , width 
    , px 
    , spacing 
    , padding 
    )
import Element.Input exposing 
    ( text
    , labelLeft 
    )
import Inputs exposing 
    ( input 
    )

type alias Model =
    { name : String
    , address1 : String
    , address2 : String
    , account : String 
    , vatin : String
    }

default : Model
default = 
    { name = ""
    , address1 = ""
    , address2 = ""
    , account = ""
    , vatin = ""
    }

type Msg 
    = NameChanged String 
    | Address1Changed String 
    | Address2Changed String 
    | AccountChanged String 
    | VatinChanged String

update : Msg -> Model -> Model
update msg settings 
    = case msg of
        NameChanged val -> { settings | name = val }
        Address1Changed val -> { settings | address1 = val }
        Address2Changed val -> { settings | address2 = val }
        AccountChanged val -> { settings | account = val }
        VatinChanged val -> { settings | vatin = val }

label : String -> Element msg 
label caption =
    el 
        [ width <| px 260
        , Element.centerY
        ] 
        <| Element.text caption

nameField : Model -> Element Msg
nameField  model = 
    let l = label "Nazwa"
    in input l model.name NameChanged

address1Field : Model -> Element Msg
address1Field model = 
    let l = label "Adres 1"
    in input l model.address1 Address1Changed

address2Field : Model -> Element Msg
address2Field model = 
    let l = label "Adres 2"
    in input l model.address2 Address2Changed

accountField : Model -> Element Msg
accountField model = 
    let l = label "Numer konta bankowego"
    in input l model.account AccountChanged

vatinField : Model -> Element Msg
vatinField model = 
    let l = label "VAT"
    in input l model.vatin VatinChanged

form : Model -> Element Msg
form settings = 
    column 
        [ spacing 16
        , padding 16
        ]
        [ nameField settings
        , address1Field settings
        , address2Field settings
        , vatinField settings
        , accountField settings
        ] 

view : Model -> Element Msg
view settings = form settings