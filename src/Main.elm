module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Material.Typography as Typography
import Material.Button exposing (buttonConfig, raisedButton)

main =
    Browser.sandbox { init = 0, update = update, view = view }

type alias Settings =
    { name : String
    , street : String
    , zip_code : String
    , city : String
    , account : String }

type alias Buyer = 
    { name : String
    , vatin : String {- NIP in Poland -}
    , address : String
    , zip_code : String
    , city : String }

type alias Invoice = 
    { number : String
    , issue_date : String
    , supply_date : String
    , gross_value : Float
    , net_value : Float
    , remarks : String
    , items : List InvoiceItems }

type alias InvoiceItems =
    { name : String
    , seq : Int
    , quantity : String
    , unit : String 
    , vat_rate : Float
    , gross_price : Float
    , net_price : Float
    , gross_value : Float
    , net_value : Float }

type Msg = Inc | Dec

update msg model = 
    case msg of 
        Inc -> model + 11
        Dec -> model - 1

view model = 
    div [ Typography.typography ]
        [ raisedButton { buttonConfig 
                            | onClick = Just Dec
                            , icon = Just "favorite" } 
                        "Decrement"
        , div [] [ text (String.fromInt model) ]
        , raisedButton { buttonConfig 
                            | onClick = Just Inc } 
                        "Increment"
        ]