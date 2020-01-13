module Invoices exposing 
    ( Model
    , Msg
    , view
    , setCatalogMode
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
    , width 
    , fill 
    , px 
    , el 
    , centerX
    )
import Element.Input as Input exposing 
    ( button
    , labelLeft
    , labelHidden
    )
import Element.Font as Font
import Entity as Entity
import Styles exposing 
    ( space 
    )

type alias Invoice =
    { number : String
    , key : Key
    , issue_date : String
    , supply_date : String
    , gross_value : Float
    , net_value : Float
    , remarks : String
    , items : List String 
    , buyer : Entity.Model
    , seller : Entity.Model
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
    | BuyerChanged Key Entity.Msg
    | SellerChanged Key Entity.Msg

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
    , buyer = Entity.default
    , seller = Entity.default 
    }

setter : Invoice -> Maybe Invoice -> Maybe Invoice 
setter newDoc maybeDoc = Maybe.map ( \_ -> newDoc ) maybeDoc 

updateInvoice : Key -> Invoice -> Store Invoice -> Store Invoice 
updateInvoice key doc store =
    let set = setter doc
    in Dict.update key set store 

setCatalogMode : Model -> Model 
setCatalogMode model =
    { model | mode = Catalog }

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
        BuyerChanged key innerMsg -> 
            let maybeDoc = get key model.docs
            in case maybeDoc of
                Just doc -> 
                    let ( updatedBuyer, outMsg ) = Entity.update innerMsg doc.buyer
                        updatedDoc = { doc | buyer = updatedBuyer }
                    in ( { model | docs = updateInvoice key updatedDoc model.docs }, Cmd.map ( BuyerChanged key ) outMsg )
                Nothing -> ( model, Cmd.none )
        SellerChanged key innerMsg -> 
            let maybeDoc = get key model.docs
            in case maybeDoc of
                Just doc -> 
                    let ( updatedSeller, outMsg ) = Entity.update innerMsg doc.seller
                        updatedDoc = { doc | seller = updatedSeller }
                    in ( { model | docs = updateInvoice key updatedDoc model.docs }, Cmd.map ( SellerChanged key ) outMsg )
                Nothing -> ( model, Cmd.none )
            
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
    Input.text [] 
        { onChange = NumberFieldChanged key 
        , text =inv.number 
        , placeholder = Nothing 
        , label = labelHidden "Nr faktury" }

form : Invoice -> Key -> Element Msg
form doc key = 
    let buyerMap = Element.map <| BuyerChanged key 
        buyer = buyerMap <| Entity.view doc.buyer
        sellerMap = Element.map <| SellerChanged key 
        seller = sellerMap <| Entity.view doc.seller
    in column 
        [ spacing space.small 
        , padding space.large
        , width <| px 960
        , centerX
        ] 
        [ noField doc key 
        , row 
            [ width fill
            ]
            [ entityView "Kupujacy" buyer 
            , entityView "Sprzedajacy" seller
            ] 
        ]

entityView : String -> Element Msg -> Element Msg
entityView title innerView =
    column 
        [ padding <| space.normal
        , spacing <| space.small 
        , width Element.fill ] 
        [ el [ Font.bold ] <| text title
        , innerView
        ]

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
            
    