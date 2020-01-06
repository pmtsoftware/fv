module Invoices exposing 
    ( Model
    , Msg
    , view
    , update
    , default
    )

import Html
import Time exposing 
    ( Posix
    , now
    , posixToMillis
    )
import Dict exposing 
    ( Dict
    , insert
    , empty
    , toList
    , get 
    )
import Task exposing 
    ( perform 
    )
import List exposing 
    ( map 
    )
import Element exposing 
    ( Element
    , row 
    , column
    , spacing 
    , padding
    , text
    )
import Element.Input as Input exposing 
    ( button
    , labelLeft
    )

type alias Buyer = 
    { name : String
    , vatin : String {- NIP in Poland -}
    , address1 : String
    , address2 : String
    }

type alias Seller = 
    { name : String
    , vatin : String {- NIP in Poland -}
    , address1 : String
    , address2 : String
    }

type alias Invoice =
    { number : String
    , key : Key
    , issue_date : String
    , supply_date : String
    , gross_value : Float
    , net_value : Float
    , remarks : String
    , items : List String 
    , buyer : Buyer
    , seller : Seller
    }

type ViewMode = Catalog | Zoom Int

type alias Key = Int

type alias Store a = Dict Key a

type alias Model =
    { docs : Store Invoice 
    , mode : ViewMode 
    }

default : Model
default = 
    { docs = empty
    , mode = Catalog }

type Msg
    = AddNewClicked 
    | AddNew Posix
    | GoToZoom Key
    | NumberFieldChanged Key String
    | IssueDateFieldChanged Key String
    | SupplyDateFieldChanged Key String
    | RemarksFieldChanged Key String
    | BuyerNameFieldChanged Key String
    | BuyerVatinFieldChanged Key  String
    | BuyerAddress1FieldChanged Key String
    | BuyerAddress2FieldChanged Key String

newInvoice : Int -> Invoice
newInvoice timestamp = 
    { number = "Faktura VAT" 
    , key = timestamp
    , issue_date = ""
    , supply_date = ""
    , gross_value = 0
    , net_value = 0
    , remarks = ""
    , items = [] 
    , buyer = 
        { name = ""
        , vatin = ""
        , address1 = ""
        , address2 = ""
        }
    , seller = 
        { name = ""
        , vatin = ""
        , address1 = ""
        , address2 = ""
        }
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model
    = case msg of
        AddNewClicked -> ( model, perform AddNew now )
        AddNew time -> 
            let timestamp = posixToMillis time
                newDoc = newInvoice timestamp
            in ( { model 
                        | docs = insert timestamp newDoc model.docs
                        , mode = Zoom timestamp
                 } , Cmd.none )
        GoToZoom id -> ( model, Cmd.none )
        NumberFieldChanged key value -> ( model, Cmd.none )
        IssueDateFieldChanged key value -> ( model, Cmd.none )
        SupplyDateFieldChanged key value -> ( model, Cmd.none )
        RemarksFieldChanged key value -> ( model, Cmd.none )
        BuyerNameFieldChanged key value -> ( model, Cmd.none )
        BuyerVatinFieldChanged key  value -> ( model, Cmd.none )
        BuyerAddress1FieldChanged key value -> ( model, Cmd.none )
        BuyerAddress2FieldChanged key value -> ( model, Cmd.none )

            
catalog : Store Invoice -> Element Msg
catalog docs = 
    column []
        [ addNewButton 
        , listOfDocs docs
        ]

listOfDocs : Store Invoice -> Element Msg
listOfDocs store =
    let 
        listOfPairs = toList store
        docs = map ( \(_, v) -> v ) listOfPairs
    in column [ padding 16, spacing 8 ] <| map docToHtml docs

docToHtml : Invoice -> Element Msg
docToHtml doc =
    let caption = doc.number ++ String.fromInt doc.key
    in text caption

addNewButton : Element Msg
addNewButton = button [] 
    { onPress = Just AddNewClicked
    , label = text "Nowa faktura" 
    }
    

noField : Invoice -> Key -> Element Msg
noField inv key =
    --let msg = NumberFieldChanged key
    Input.text [] 
        { onChange = NumberFieldChanged key 
        , text =inv.number 
        , placeholder = Nothing 
        , label = labelLeft [] <| text "Nr faktury" }

form : Invoice -> Key -> Element Msg
form doc key = 
    column [] 
        [ noField doc key ]

zoom : Store Invoice -> Key -> Element Msg
zoom dict key =
    let maybeDoc = get key dict 
    in case maybeDoc of
        Nothing -> text "error"
        Just doc -> form doc key 

view : Model -> Element Msg
view model
    = case model.mode of
        Catalog -> catalog model.docs 
        Zoom key -> zoom model.docs key 
            
    