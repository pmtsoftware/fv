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
import Task exposing 
    ( perform 
    )
import List exposing 
    ( map 
    )
import Element exposing 
    ( Element
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
    , datePicker
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
import Date exposing 
    ( Date
    )
import DatePicker as DatePicker exposing
    ( ChangeEvent (..)
    )

type ViewMode 
    = Catalog 
    | Zoom FormState

type alias FormState = 
    { docId : Data.DocId
    , issueDatePickerModel : DatePicker.Model 
    , supplyDatePickerModel : DatePicker.Model
    }

type alias Model =
    { folder : Data.Folder 
    , mode : ViewMode 
    , itemAskedForDelete : Maybe DocItemId
    }

default : Model
default = 
    { folder = Data.createFolder
    , mode = Catalog 
    , itemAskedForDelete = Nothing
    }

type Msg
    = AddNewClicked 
    | AddNewItemButtonClicked DocId
    | AddNewDoc Posix
    | AddNewDocItem DocId Posix
    | InvoiceClicked DocId
    | NumberFieldChanged DocId String
    | IssueDatePickerChangeEvent DocId ChangeEvent
    | SupplyDatePickerChangeEvent DocId ChangeEvent
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
    | DeleteItemButtonClicked DocItemId
    | ConfirmItemDeleteButtonClicked 
    | RejectItemDeleteButtonClicked 


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
                        , mode = Zoom <| 
                                        { docId = Data.getDocId doc
                                        , issueDatePickerModel = DatePicker.init
                                        , supplyDatePickerModel = DatePicker.init
                                        } 
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
        InvoiceClicked id -> 
            let newMode = Zoom 
                            { docId = id 
                            , issueDatePickerModel = DatePicker.init
                            , supplyDatePickerModel = DatePicker.init
                            } 
            in ( { model | mode = newMode }, Cmd.none )
        NumberFieldChanged id value -> handleDocUpdate model <| Data.setDocNumber model.folder id value
        IssueDatePickerChangeEvent id event -> 
            case event of
                DateChanged value -> handleDocUpdate model <| Data.setDocIssueDate model.folder id (Just value) 
                TextChanged text -> ( model, Cmd.none )
                PickerChanged subMsg -> 
                    let updateMode old = 
                                        case old of 
                                            (Zoom data) -> Zoom 
                                                                { docId = data.docId 
                                                                , issueDatePickerModel = data.issueDatePickerModel |> DatePicker.update subMsg
                                                                , supplyDatePickerModel = data.supplyDatePickerModel                                                                }
                                            Catalog -> Catalog
                    in ( { model | mode = updateMode model.mode }, Cmd.none )
        SupplyDatePickerChangeEvent id event -> 
            case event of 
                DateChanged value -> handleDocUpdate model <| Data.setDocSupplyDate model.folder id (Just value) 
                TextChanged text -> ( model, Cmd.none )
                PickerChanged subMsg -> 
                    let updateMode old = 
                                        case old of 
                                            (Zoom data) -> Zoom 
                                                                { docId = data.docId 
                                                                , issueDatePickerModel = data.issueDatePickerModel
                                                                , supplyDatePickerModel = data.supplyDatePickerModel |> DatePicker.update subMsg
                                                                }
                                            Catalog -> Catalog
                    in ( { model | mode = updateMode model.mode }, Cmd.none )
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
        DeleteItemButtonClicked docItemId -> ( { model | itemAskedForDelete = Just docItemId }, Cmd.none )
        ConfirmItemDeleteButtonClicked -> 
            case model.itemAskedForDelete of
                Just docItemId -> ( { model 
                                        | itemAskedForDelete = Nothing
                                        , folder = Data.deleteItem model.folder docItemId }, Cmd.none )
                Nothing -> ( { model | itemAskedForDelete = Nothing }, Cmd.none )
        RejectItemDeleteButtonClicked -> ( { model | itemAskedForDelete = Nothing }, Cmd.none )

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
    in  Input.text 
            [ Font.size 24 
            , Font.heavy 
            , width fill
            ] 
            { onChange = NumberFieldChanged docId 
            , text = Data.getDocNumber inv 
            , placeholder = Nothing 
            , label = labelHidden "Nr faktury" 
            }

label : String -> Element msg 
label = 
    let attr = width <| px 80
    in labelWithWidth attr 

labelWithWidth : Element.Attribute msg -> String -> Element msg 
labelWithWidth attr caption =
    el 
        [ attr
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

itemButtons : Maybe DocItemId -> DocItem -> Element Msg
itemButtons itemToDelete docItem =
    let docItemId = Data.getDocItemId docItem 
        toBtn ( msg,  caption ) = button 
            [ Element.alignRight
            , Font.color theme.primary_dark
            , Border.color theme.primary_dark
            , Border.solid
            , Border.width 2
            , Border.rounded 4
            , padding space.small
            ]
            { onPress = Just msg 
            , label = text caption 
            }
        commands = case itemToDelete of
            Just idToDel ->
                if idToDel == docItemId then 
                    [ ( ConfirmItemDeleteButtonClicked, "Tak") 
                    , ( RejectItemDeleteButtonClicked, "Nie" ) 
                    ] 
                else 
                    [ ( DeleteItemButtonClicked docItemId, "Del" ) ]
            Nothing -> [ ( DeleteItemButtonClicked docItemId, "Del" ) ]
        btns = List.map toBtn commands
    in el 
        [ Element.centerY 
        , padding space.xSmall
        ] <| row [ Element.alignRight ] btns

itemsView : Doc -> Maybe DocItemId -> Element Msg 
itemsView doc itemToDelete = 
    table
        []
        { data = Data.getDocItems doc  
        , columns =
            [ 
                { header = header Nothing
                , width = px 100
                , view = itemButtons itemToDelete
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

topRight state doc = 
    let lblWidth = width <| px 120
        lbl = labelWithWidth lblWidth
        docId = Data.getDocId doc
        issueDate = Data.getDocIssueDate doc
        supplyDate = Data.getDocSupplyDate doc 
    in 
    { issueDate = 
        let cap = lbl "Data wydania" 
            msg = IssueDatePickerChangeEvent docId
        in datePicker cap issueDate msg state.issueDatePickerModel
    , supplyDate = 
        let cap = lbl "Data sprzedazy" 
            msg = SupplyDatePickerChangeEvent docId
        in datePicker cap supplyDate msg state.supplyDatePickerModel
    }

form : FormState -> Doc -> Maybe DocItemId -> Element Msg
form state doc itemToDel = 
    let topRightFields = topRight state doc
    in column 
        [ spacing space.small 
        , padding space.large
        , width <| px 960
        , centerX
        ] 
        [ row 
            [ width fill 
            , spacing space.xLarge
            ] 
            [ el [ Element.alignTop, width fill ] <| noField doc
            , column
                [ width <| px 300 
                , spacing space.xSmall
                ] 
                [ topRightFields.issueDate
                , topRightFields.supplyDate
                ]
            ] 
        , row 
            [ width fill
            ]
            [ entityView "Kupujacy" <| buyerView doc
            , entityView "Sprzedajacy" <| sellerView doc
            ] 
        , itemsView doc itemToDel
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
            Zoom state ->
                case Data.getDocById model.folder state.docId of
                    Just doc ->
                        form state doc model.itemAskedForDelete
                    Nothing ->
                        text "Brak dokumentu :("
    in el 
        [ width <| px 960
        , centerX
        , padding space.normal
        ] inner
            
    