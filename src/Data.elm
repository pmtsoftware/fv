module Data exposing (..)

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