module Invoices exposing 
    ( Model
    , Msg
    , view
    , setCatalogMode
    , update
    , default
    )

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
    , Column 
    , table
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
import Inputs exposing 
    ( input 
    )
import Element.Input as Input exposing 
    ( button
    , labelLeft
    , labelHidden
    )
import Element.Font as Font
import Element.Background as Bg
import Element.Border as Border 
import Styles exposing 
    ( space 
    , theme 
    , font 
    )
import Data as Data exposing 
    ( Folder 
    , DocId 
    , DocItemId 
    , Doc
    , DocItem
    )

type ViewMode = Catalog | Zoom Data.DocId

type alias Model =
    { folder : Data.Folder 
    , mode : ViewMode 
    }

default : Model
default = 
    { folder = Data.createFolder
    , mode = Catalog }

type Msg
    = AddNewClicked 
    | AddNewItemButtonClicked DocId
    | AddNewDoc Posix
    | AddNewDocItem DocId Posix
    | InvoiceClicked DocId
    | NumberFieldChanged DocId String
    | IssueDateFieldChanged DocId String
    | SupplyDateFieldChanged DocId String
    | RemarksFieldChanged DocId String
    | BuyerNameChanged DocId String
    | BuyerAddress1Changed DocId String 
    | BuyerAddress2Changed DocId String 
    | BuyerVatinChanged DocId String 
    | SellerNameChanged DocId String
    | SellerAddress1Changed DocId String 
    | SellerAddress2Changed DocId String 
    | SellerVatinChanged DocId String 
    | ItemQtyChanged DocItemId String
    | ItemNameChanged DocItemId String
    | ItemVatRateChanged DocItemId String
    | ItemNetPriceChanged DocItemId String
    | ItemUnitChanged DocItemId String 
    | ItemAskedForDelete


setCatalogMode : Model -> Model 
setCatalogMode model =
    { model | mode = Catalog }

handleDocUpdate : Model -> Result Data.Error ( Folder, a ) -> ( Model, Cmd Msg )
handleDocUpdate model result =
    case result of
            Result.Ok ( newFolder, _) -> 
                ( { model | folder = newFolder }, Cmd.none )
            Result.Err _ -> ( model, Cmd.none )

handleDocCreate : Model -> Result Data.Error ( Folder, Doc ) -> ( Model, Cmd Msg )
handleDocCreate model result =
    case result of
            Result.Ok ( newFolder, doc ) -> 
                ( { model 
                        | folder = newFolder
                        , mode = Zoom <| Data.getDocId doc 
                        }, Cmd.none )
            Result.Err _ -> ( model, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model
    = case msg of
        AddNewClicked -> ( model, perform AddNewDoc now )
        AddNewItemButtonClicked docId -> ( model, perform ( AddNewDocItem docId ) now )
        AddNewDoc time -> 
            let timestamp = posixToMillis time
            in handleDocCreate model <| Data.createDoc model.folder timestamp
        AddNewDocItem docId time ->
            let timestamp = posixToMillis time 
            in handleDocUpdate model <| Data.createItem model.folder docId timestamp 
        InvoiceClicked id -> ( { model | mode = Zoom id }, Cmd.none )
        NumberFieldChanged id value -> handleDocUpdate model <| Data.setDocNumber model.folder id value
        IssueDateFieldChanged id value -> handleDocUpdate model <| Data.setDocIssueDate model.folder id value 
        SupplyDateFieldChanged id value -> handleDocUpdate model <| Data.setDocSupplyDate model.folder id value 
        RemarksFieldChanged id value -> handleDocUpdate model <| Data.setDocRemarks model.folder id value 
        BuyerNameChanged id value -> handleDocUpdate model <| Data.setBuyerName model.folder id value
        BuyerAddress1Changed id value -> handleDocUpdate model <| Data.setBuyerAddress1 model.folder id value
        BuyerAddress2Changed id value -> handleDocUpdate model <| Data.setBuyerAddress2 model.folder id value
        BuyerVatinChanged id value -> handleDocUpdate model <| Data.setBuyerVatin model.folder id value
        SellerNameChanged id value -> handleDocUpdate model <| Data.setSellerName model.folder id value
        SellerAddress1Changed id value -> handleDocUpdate model <| Data.setSellerAddress1 model.folder id value
        SellerAddress2Changed id value -> handleDocUpdate model <| Data.setSellerAddress2 model.folder id value
        SellerVatinChanged id value -> handleDocUpdate model <| Data.setSellerVatin model.folder id value
        ItemQtyChanged id value -> 
            let qty = Maybe.withDefault 0 <| String.toFloat value
            in handleDocUpdate model <| Data.setItemQuantity model.folder id qty 
        ItemNameChanged id val -> handleDocUpdate model <| Data.setDocItemName model.folder id val 
        ItemVatRateChanged id value -> 
            let qty = Maybe.withDefault 0 <| String.toFloat value
            in handleDocUpdate model <| Data.setItemVatRate model.folder id qty 
        ItemNetPriceChanged id value -> 
            let qty = Maybe.withDefault 0 <| String.toFloat value
            in handleDocUpdate model <| Data.setItemNetPrice model.folder id qty 
        ItemUnitChanged id val -> handleDocUpdate model <| Data.setItemUnit model.folder id val 
        ItemAskedForDelete -> ( model, Cmd.none )

catalog : Folder -> Element Msg
catalog docs = 
    column []
        [ addNewButton 
        , listOfDocs docs
        ]

listOfDocs : Folder -> Element Msg
listOfDocs folder =
    let docs = Data.getDocs folder
    in column [ padding 16, spacing 8 ] <| map docToHtml docs

docToHtml : ( DocId, Doc ) -> Element Msg
docToHtml ( key, doc ) =
    el []
        <| button 
            []
            { onPress = Just <| InvoiceClicked key 
            , label = text <| Data.getDocNumber doc
            } 

addNewButton : Element Msg
addNewButton = 
    el 
        [ Bg.color theme.primary 
        , Font.color font.light
        , padding space.small
        ]
    <| button [] 
        { onPress = Just AddNewClicked
        , label = text "Nowa faktura" 
        }
    

noField : Doc -> Element Msg
noField inv =
    let docId = Data.getDocId inv
    in Input.text [] 
        { onChange = NumberFieldChanged docId 
        , text = Data.getDocNumber inv 
        , placeholder = Nothing 
        , label = labelHidden "Nr faktury" }

label : String -> Element msg 
label caption =
    el 
        [ width <| px 80
        , Element.centerY
        ] 
        <| Element.text caption

nameField : ( String -> Msg ) -> String  -> Element Msg
nameField msg name =
    let l = label "Nazwa" 
    in input l name msg

vatinField : ( String -> Msg ) -> String -> Element Msg 
vatinField msg vatin =
    let l = label "VAT"
    in input l vatin msg

address1Field : ( String -> Msg ) -> String -> Element Msg 
address1Field msg address1 = 
    let l = label "Adres 1"
    in input l address1 msg

address2Field : ( String -> Msg ) -> String -> Element Msg 
address2Field msg address2 = 
    let l = label "Adres 2"
    in input l address2 msg

embedDocId : ( DocId -> a -> Msg ) -> Doc -> ( a -> Msg )
embedDocId func doc = 
    func <| Data.getDocId doc

buyerView : Doc -> Element Msg 
buyerView doc =
    let nameMsg = embedDocId BuyerNameChanged doc 
        vatinMsg = embedDocId BuyerVatinChanged doc 
        address1Msg = embedDocId BuyerAddress1Changed doc 
        address2Msg = embedDocId BuyerAddress2Changed doc 
    in column 
        [ spacing space.small
        , width fill 
        ]
        [ nameField nameMsg <| Data.getBuyerName doc 
        , vatinField vatinMsg <| Data.getBuyerVatin doc 
        , address1Field address1Msg <| Data.getBuyerAddress1 doc 
        , address2Field address2Msg <| Data.getBuyerAddress2 doc 
        ]

sellerView : Doc -> Element Msg 
sellerView doc =
    let nameMsg = embedDocId SellerNameChanged doc 
        vatinMsg = embedDocId SellerVatinChanged doc 
        address1Msg = embedDocId SellerAddress1Changed doc 
        address2Msg = embedDocId SellerAddress2Changed doc 
    in column 
        [ spacing space.small
        , width fill 
        ]
        [ nameField nameMsg <| Data.getSellerName doc 
        , vatinField vatinMsg <| Data.getSellerVatin doc 
        , address1Field address1Msg <| Data.getSellerAddress1 doc 
        , address2Field address2Msg <| Data.getSellerAddress2 doc 
        ]

addNewItemButton : Doc -> Element Msg
addNewItemButton doc = 
    el 
        [ Bg.color theme.primary 
        , Font.color font.light
        , padding space.small
        ]
    <| button [] 
        { onPress = Just <| AddNewItemButtonClicked <| Data.getDocId doc 
        , label = text "Nowa pozycja"
        }

header : Maybe String -> Element Msg
header name = 
    el 
        [ Bg.color <| theme.primary
        , Font.color <| font.light
        , padding space.xSmall
        , Element.height fill
        ]
        <| el 
            [ Element.alignLeft 
            ]  <| case name of 
                    Just value -> text value
                    Nothing -> Element.none

nameCell : DocItem -> Element Msg
nameCell item = 
    let docItemId = Data.getDocItemId item 
        msg = ItemNameChanged docItemId
        input = 
            Input.text  [ width fill ]
                { onChange = msg 
                , text = Data.getDocItemName item 
                , placeholder = Nothing 
                , label = Input.labelHidden "Nazwa"
                }
    in el 
        [ padding space.xSmall
        ] input

qtyCell : DocItem -> Element Msg
qtyCell item = 
    let val = String.fromFloat <| Data.getItemQuantity item 
        docItemId = Data.getDocItemId item 
        msg = ItemQtyChanged docItemId
        input = 
            Input.text  [ width fill ]
                { onChange = msg 
                , text = val 
                , placeholder = Nothing 
                , label = Input.labelHidden "Ilosc"
                }
    in el 
        [ padding space.xSmall
        ] input

vatCell : DocItem -> Element Msg
vatCell item = 
    let val = String.fromFloat <| Data.getItemVatRate item 
        docItemId = Data.getDocItemId item 
        msg = ItemVatRateChanged docItemId
        input = 
            Input.text  [ width fill ]
                { onChange = msg 
                , text = val 
                , placeholder = Nothing 
                , label = Input.labelHidden "VAT"
                }
    in el 
        [ padding space.xSmall
        ] input

unitCell : DocItem -> Element Msg
unitCell item = 
    let docItemId = Data.getDocItemId item 
        msg = ItemUnitChanged docItemId
        input = 
            Input.text  [ width fill ]
                { onChange = msg 
                , text = Data.getItemUnit item 
                , placeholder = Nothing 
                , label = Input.labelHidden "J.m."
                }
    in el 
        [ padding space.xSmall
        ] input

priceCell : DocItem -> Element Msg
priceCell item = 
    let val = String.fromFloat <| Data.getItemNetPrice item 
        docItemId = Data.getDocItemId item 
        msg = ItemNetPriceChanged docItemId
        input = 
            Input.text  [ width fill ]
                { onChange = msg 
                , text = val 
                , placeholder = Nothing 
                , label = Input.labelHidden "Cena"
                }
    in el 
        [ padding space.xSmall
        ] input

delButtonCell : DocItem -> Element Msg 
delButtonCell _ =
    el 
        [ Element.centerY 
        , padding space.xSmall
        ]
        <| button 
            [ Element.alignRight
            , Font.color theme.primary_dark
            , Border.color theme.primary_dark
            , Border.solid
            , Border.width 2
            , Border.rounded 4
            , padding space.small
            ]
            { onPress = Nothing 
            , label = text "Del" 
            } 

itemsView : Doc -> Element Msg 
itemsView doc = 
    table
        []
        { data = Data.getDocItems doc  
        , columns =
            [ 
                { header = header Nothing
                , width = px 100
                , view = delButtonCell
                }
                , { header = header <| Just "Nazwa"
                , width = fill
                , view = nameCell
                }
                , { header = header <| Just "Ilosc"
                , width = px 80
                , view = qtyCell
                }
                , { header = header <| Just "J.m."
                , width = px 100
                , view = unitCell
                }
                , { header = header <| Just "Cena"
                , width = px 80
                , view = priceCell
                }
                , { header = header <| Just "VAT"
                , width = px 80
                , view = vatCell
                }
            ]
        } 

form : Doc -> Element Msg
form doc = 
    column 
        [ spacing space.small 
        , padding space.large
        , width <| px 960
        , centerX
        ] 
        [ noField doc 
        , row 
            [ width fill
            ]
            [ entityView "Kupujacy" <| buyerView doc
            , entityView "Sprzedajacy" <| sellerView doc
            ] 
        , itemsView doc
        , addNewItemButton doc
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

view : Model -> Element Msg
view model =
    let inner = case model.mode of
            Catalog -> catalog model.folder 
            Zoom docId ->
                case Data.getDocById model.folder docId of
                    Just doc ->
                        form doc
                    Nothing ->
                        text "Brak dokumentu :("
    in el 
        [ width <| px 960
        , centerX
        , padding space.normal
        ] inner
            
    