module Data exposing 
    ( Folder
    , Error (..)
    , Doc
    , DocItem
    , DocId 
    , DocItemId 
    , Payment (..)
    , createFolder
    , createDoc
    , createItem
    , deleteDoc
    , deleteItem
    , getDocById 
    , getDocs 
    , getDocId
    , getDocNumber
    , setDocNumber
    , getDocIssueDate
    , setDocIssueDate
    , getDocSupplyDate
    , setDocSupplyDate
    , getDocGrossValue
    , setDocGrossValue
    , getDocNetValue 
    , setDocNetValue 
    , getDocRemarks 
    , setDocRemarks 
    , getDocItemId
    , getDocItems
    , getDocItemName 
    , setDocItemName 
    , getItemQuantity
    , setItemQuantity
    , getItemUnit
    , setItemUnit
    , getItemVatRate
    , setItemVatRate
    , getItemGrossPrice
    , setItemGrossPrice
    , getItemNetPrice
    , setItemNetPrice
    , getItemGrossValue
    , setItemGrossValue
    , getItemNetValue
    , setItemNetValue
    , getBuyerName
    , setBuyerName
    , getBuyerVatin
    , setBuyerVatin
    , getBuyerAddress1
    , setBuyerAddress1
    , getBuyerAddress2
    , setBuyerAddress2
    , getSellerName
    , setSellerName
    , getSellerVatin
    , setSellerVatin
    , getSellerAddress1
    , setSellerAddress1
    , getSellerAddress2
    , setSellerAddress2
    , getPayment
    , setPayment
    )

import Dict as Dict exposing
    ( Dict 
    )
import Result as Result exposing 
    ( Result 
    )
import Date exposing 
    ( Date
    )

type alias Key = Int

type alias ItemKey = Int

type alias DocsStore = Dict Key Document

type alias ItemsStore = Dict ItemKey DocumentItem

type Folder = Folder DocsStore 

type Doc = Doc Document
type DocItem = DocItem DocumentItem
type DocId = DocId Key
type DocItemId = DocItemId Key ItemKey

type Error = DocNotFound | DocItemNotFound

type Payment = Cash | Transfer

type alias DocSetter a = Folder -> DocId -> a -> Result Error ( Folder, Doc )

type alias DocItemSetter a = Folder -> DocItemId -> a -> Result Error ( Folder, DocItem )

type alias Document = 
    { number : String
    , id : Key
    , issue_date : Maybe Date
    , supply_date : Maybe Date
    , gross_value : Float
    , net_value : Float
    , remarks : String
    , sellerName : String 
    , sellerVatin : String 
    , sellerAddress1 : String 
    , sellerAddress2 : String 
    , buyerName : String 
    , buyerVatin : String 
    , buyerAddress1 : String 
    , buyerAddress2 : String 
    , payment : Payment
    , items : ItemsStore
    }

type alias DocumentItem =
    { id : ItemKey 
    , docId : Key
    , name : String
    , quantity : Float
    , unit : String 
    , vatRate : Float
    , grossPrice : Float
    , netPrice : Float
    , grossValue : Float
    , netValue : Float 
    }

getDocs : Folder -> List ( DocId, Doc ) 
getDocs ( Folder docs ) =
    let mapper = \ ( key, value ) -> ( DocId key, Doc value )
    in List.map mapper <| Dict.toList docs

createFolder : Folder
createFolder = Folder Dict.empty

deleteDoc : Folder -> DocId -> Folder
deleteDoc ( Folder store ) id =
    let key = getKey id 
    in Folder <| Dict.remove key store

createDoc : Folder -> Int -> Result Error ( Folder, Doc )
createDoc ( Folder dict ) id =
    let items = Dict.empty
        doc = 
            { number = "Faktura VAT"
            , id = id 
            , issue_date = Nothing 
            , supply_date = Nothing
            , gross_value = 0 
            , net_value = 0
            , remarks = ""
            , buyerName = ""
            , buyerVatin = ""
            , buyerAddress1 = ""
            , buyerAddress2 = ""
            , sellerName = ""
            , sellerVatin = ""
            , sellerAddress1 = ""
            , sellerAddress2 = ""
            , payment = Cash
            , items = items 
            }
        newDict = Dict.insert id doc dict
    in Result.Ok ( Folder newDict, Doc doc )

update : Int -> v -> Dict Int v -> Dict Int v 
update key val dict =
    let swap = \_ -> Just val 
    in Dict.update key swap dict

swapDoc : Folder -> Key -> Document -> Folder
swapDoc ( Folder docs ) id doc =
    let swap = \_ -> Just doc 
    in Folder <| Dict.update id swap docs

swapDocItem : Document -> DocItemId -> DocumentItem -> Document 
swapDocItem doc ( DocItemId _ id ) item =
    let newItems = update id item doc.items
    in { doc | items = newItems }

deleteItem : Folder -> DocItemId -> Folder 
deleteItem f ( DocItemId docKey key ) =
    let maybeDoc = getDoc f docKey
    in case maybeDoc of
        Just origDoc -> 
            let newItems = Dict.remove key origDoc.items
                newDoc = { origDoc | items = newItems }
            in swapDoc f docKey newDoc
        Nothing -> f

createItem : Folder -> DocId -> Int -> Result Error ( Folder, DocItem )
createItem f docId id =
    let key = getKey docId
    in case getDoc f key of
            Just invoice -> 
                let newItem = 
                        { name = ""
                        , id = id
                        , docId = getKey docId
                        , quantity = 0
                        , unit = ""
                        , vatRate = 0
                        , grossPrice = 0
                        , netPrice = 0
                        , grossValue = 0
                        , netValue = 0 
                        }
                    newItems = Dict.insert id newItem invoice.items
                    newDoc = { invoice | items = newItems }
                    newFolder = swapDoc f key newDoc
                in Result.Ok ( newFolder, DocItem newItem )
            Nothing -> Result.Err DocNotFound

getKey : DocId -> Key 
getKey ( DocId key ) = key

getDocId : Doc -> DocId
getDocId ( Doc invoice ) = DocId invoice.id

getDoc : Folder -> Key -> Maybe Document 
getDoc ( Folder docs ) docId = Dict.get docId docs 

getDocById : Folder -> DocId -> Maybe Doc 
getDocById f id = 
    let key = getKey id in Maybe.map Doc <| getDoc f key

getDocByItemId : Folder -> DocItemId -> Maybe Doc 
getDocByItemId ( Folder docs ) ( DocItemId docKey _ ) =
    Maybe.map Doc <| Dict.get docKey docs

getDocNumber : Doc -> String 
getDocNumber ( Doc invoice ) = invoice.number

setDocField : Folder -> DocId -> ( Document -> Document ) -> Result Error ( Folder, Doc )
setDocField folder docId setField =
    let key = getKey docId
    in case getDoc folder key of
        Just doc -> 
            let newDoc = setField doc 
                newFolder = swapDoc folder key newDoc
            in Result.Ok ( newFolder, Doc newDoc )
        Nothing -> Result.Err DocNotFound

setDocNumber : DocSetter String
setDocNumber f id value =
    setDocField f id <| \inv -> { inv | number = value } 

getDocIssueDate : Doc -> Maybe Date 
getDocIssueDate ( Doc doc ) = doc.issue_date

setDocIssueDate : DocSetter (Maybe Date)
setDocIssueDate f id value =
    setDocField f id <| \inv -> { inv | issue_date = value }

getDocSupplyDate : Doc -> Maybe Date
getDocSupplyDate  ( Doc doc ) = doc.supply_date

setDocSupplyDate : DocSetter (Maybe Date)
setDocSupplyDate  f id value =
    setDocField f id <| \inv -> { inv | supply_date = value }

getDocGrossValue : Doc -> Float
getDocGrossValue  ( Doc doc ) = doc.gross_value 

setDocGrossValue : DocSetter Float
setDocGrossValue  f id value =
    setDocField f id <| \inv -> { inv | gross_value = value }

getDocNetValue  : Doc -> Float 
getDocNetValue ( Doc doc ) = doc.net_value 

setDocNetValue  : DocSetter Float
setDocNetValue f id value =
    setDocField f id <| \inv -> { inv | net_value = value }

getDocRemarks  : Doc -> String
getDocRemarks ( Doc doc ) = doc.remarks

setDocRemarks : DocSetter String
setDocRemarks f id value =
    setDocField f id <| \inv -> { inv | remarks = value }

getBuyerName : Doc -> String
getBuyerName ( Doc doc ) = doc.buyerName

setBuyerName : DocSetter String
setBuyerName f id value =
    setDocField f id <| \inv -> { inv | buyerName = value }

getBuyerVatin : Doc -> String
getBuyerVatin ( Doc doc ) = doc.buyerVatin

setBuyerVatin : DocSetter String
setBuyerVatin f docId value =    
    setDocField f docId <| \inv -> { inv | buyerVatin = value }

getBuyerAddress1 : Doc -> String
getBuyerAddress1 ( Doc doc ) = doc.buyerAddress1

setBuyerAddress1 : DocSetter String
setBuyerAddress1 f id value =    
    setDocField f id <| \inv -> { inv | buyerAddress1 = value }

getBuyerAddress2 : Doc -> String
getBuyerAddress2 ( Doc doc ) = doc.buyerAddress2

setBuyerAddress2 : DocSetter String
setBuyerAddress2 f docId value =    
    setDocField f docId <| \inv -> { inv | buyerAddress2 = value }

getSellerName : Doc -> String
getSellerName ( Doc doc ) = doc.sellerName

setSellerName : DocSetter String
setSellerName f docId value =    
    setDocField f docId <| \inv -> { inv | sellerName = value }

getSellerVatin : Doc -> String
getSellerVatin ( Doc doc ) = doc.sellerVatin

setSellerVatin : DocSetter String
setSellerVatin f docId value =    
    setDocField f docId <| \inv -> { inv | sellerVatin = value }

getSellerAddress1 : Doc -> String
getSellerAddress1 ( Doc doc ) = doc.sellerAddress1

setSellerAddress1 : DocSetter String
setSellerAddress1 f docId value =    
    setDocField f docId <| \inv -> { inv | sellerAddress1 = value }

getSellerAddress2 : Doc -> String
getSellerAddress2 ( Doc doc ) = doc.sellerAddress1

setSellerAddress2 : DocSetter String
setSellerAddress2 f docId value =    
    setDocField f docId <| \inv -> { inv | sellerAddress2 = value }

getPayment : Doc -> Payment
getPayment ( Doc doc ) = doc.payment

setPayment : DocSetter Payment
setPayment f docId value =
    setDocField f docId <| \inv -> { inv | payment = value }

getDocItemId : DocItem -> DocItemId
getDocItemId ( DocItem item ) = DocItemId item.docId item.id

getDocItems : Doc -> List DocItem
getDocItems ( Doc doc ) = 
    let mapper = \( _, value ) -> DocItem value
    in List.map mapper <| Dict.toList doc.items

getDocItem : Doc -> DocItemId -> Maybe DocItem 
getDocItem (Doc doc ) ( DocItemId _ id ) = Maybe.map DocItem <| Dict.get id doc.items

getDocItemName  : DocItem -> String 
getDocItemName ( DocItem i ) = i.name 

setItemField : Folder -> DocItemId -> ( DocumentItem -> DocumentItem ) -> Result Error ( Folder, DocItem )
setItemField f docItemId setter =
    case getDocByItemId f docItemId of
        Just doc ->
            case getDocItem doc docItemId of
                Just ( DocItem item ) ->
                    let newItem = setter item 
                        ( Doc invoice ) = doc
                        newDoc = swapDocItem invoice docItemId newItem
                    in Result.Ok ( swapDoc f invoice.id newDoc, DocItem newItem )
                Nothing -> Result.Err DocItemNotFound
        Nothing -> Result.Err DocNotFound 


setDocItemName : DocItemSetter String
setDocItemName f id value =
    setItemField f id <| \item -> { item | name = value }

getItemQuantity : DocItem -> Float 
getItemQuantity ( DocItem item ) = item.quantity

setItemQuantity : DocItemSetter Float 
setItemQuantity f id value =
    setItemField f id <| \item -> { item | quantity = value }

getItemUnit : DocItem -> String 
getItemUnit ( DocItem item ) = item.unit

setItemUnit : DocItemSetter String
setItemUnit f id value =
    setItemField f id <| \item -> { item | unit = value }

getItemVatRate : DocItem -> Float 
getItemVatRate  ( DocItem item ) = item.vatRate

setItemVatRate : DocItemSetter Float
setItemVatRate f id value =
    setItemField f id <| \item -> { item | vatRate = value }

getItemGrossPrice : DocItem -> Float 
getItemGrossPrice ( DocItem item ) = item.grossPrice

setItemGrossPrice : DocItemSetter Float
setItemGrossPrice f id value =
    setItemField f id <| \item -> { item | grossPrice = value }

getItemNetPrice : DocItem -> Float 
getItemNetPrice ( DocItem item ) = item.netPrice

setItemNetPrice : DocItemSetter Float
setItemNetPrice f id value =
    setItemField f id <| \item -> { item | netPrice = value }

getItemGrossValue : DocItem -> Float
getItemGrossValue ( DocItem item ) = item.grossValue

setItemGrossValue : DocItemSetter Float
setItemGrossValue f id value =
    setItemField f id <| \item -> { item | grossValue = value }

getItemNetValue : DocItem -> Float
getItemNetValue ( DocItem item ) = item.netValue 

setItemNetValue : DocItemSetter Float
setItemNetValue f id value =
    setItemField f id <| \item -> { item | netValue = value }