module Data exposing 
    ( Folder
    , Error (..)
    , Doc
    , DocItem
    , DocId 
    , DocItemId 
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
    )

import Dict as Dict exposing
    ( Dict 
    )
import Result as Result exposing 
    ( Result 
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

type alias Document = 
    { number : String
    , id : Key
    , issue_date : String
    , supply_date : String
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
deleteDoc ( Folder store ) docId =
    let key = getKey docId 
    in Folder <| Dict.remove key store

createDoc : Folder -> Int -> Result Error ( Folder, Doc )
createDoc ( Folder dict ) id =
    let items = Dict.empty
        doc = 
            { number = "Faktura VAT"
            , id = id 
            , issue_date = ""
            , supply_date = ""
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
            , items = items 
            }
        newDict = Dict.insert id doc dict
    in Result.Ok ( Folder newDict, Doc doc )

update : Int -> v -> Dict Int v -> Dict Int v 
update key val dict =
    let swap = \_ -> Just val 
    in Dict.update key swap dict

swapDoc : Folder -> Key -> Document -> Folder
swapDoc ( Folder docs ) docId doc =
    let swap = \_ -> Just doc 
    in Folder <| Dict.update docId swap docs

swapDocItem : Document -> DocItemId -> DocumentItem -> Document 
swapDocItem doc ( DocItemId _ id ) item =
    let newItems = update id item doc.items
    in { doc | items = newItems }

deleteItem : Folder -> DocItemId -> Folder 
deleteItem folder ( DocItemId docKey key ) =
    let maybeDoc = getDoc folder docKey
    in case maybeDoc of
        Just origDoc -> 
            let newItems = Dict.remove key origDoc.items
                newDoc = { origDoc | items = newItems }
            in swapDoc folder docKey newDoc
        Nothing -> folder

createItem : Folder -> DocId -> Int -> Result Error ( Folder, DocItem )
createItem folder docId id =
    let key = getKey docId
    in case getDoc folder key of
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
                    newFolder = swapDoc folder key newDoc
                in Result.Ok ( newFolder, DocItem newItem )
            Nothing -> Result.Err DocNotFound

getKey : DocId -> Key 
getKey ( DocId key ) = key

getDocId : Doc -> DocId
getDocId ( Doc invoice ) = DocId invoice.id

getDoc : Folder -> Key -> Maybe Document 
getDoc ( Folder docs ) docId = Dict.get docId docs 

getDocById : Folder -> DocId -> Maybe Doc 
getDocById folder docId = 
    let key = getKey docId in Maybe.map Doc <| getDoc folder key

getDocByItemId : Folder -> DocItemId -> Maybe Doc 
getDocByItemId ( Folder docs ) ( DocItemId docKey _ ) =
    Maybe.map Doc <| Dict.get docKey docs

getDocNumber : Doc -> String 
getDocNumber ( Doc invoice ) = invoice.number

setDocField : Folder -> Key -> ( Document -> Document ) -> Result Error ( Folder, Doc )
setDocField folder docId setField =
    case getDoc folder docId of
        Just doc -> 
            let newDoc = setField doc 
                newFolder = swapDoc folder docId newDoc
            in Result.Ok ( newFolder, Doc newDoc )
        Nothing -> Result.Err DocNotFound

setDocNumber : Folder -> DocId -> String -> Result Error ( Folder, Doc )
setDocNumber folder docId value =
    let setter = \inv -> { inv | number = value } 
        key = getKey docId
    in setDocField folder key setter

getDocIssueDate : Doc -> String 
getDocIssueDate ( Doc doc ) = doc.issue_date

setDocIssueDate : Folder -> DocId -> String -> Result Error ( Folder, Doc )
setDocIssueDate folder docId value =
    let setter = \inv -> { inv | issue_date = value }
        key = getKey docId
    in setDocField folder key setter

getDocSupplyDate : Doc -> String
getDocSupplyDate  ( Doc doc ) = doc.supply_date

setDocSupplyDate : Folder -> DocId -> String -> Result Error ( Folder, Doc )
setDocSupplyDate  folder docId value =
    let setter = \inv -> { inv | supply_date = value }
        key = getKey docId
    in setDocField folder key setter

getDocGrossValue : Doc -> Float
getDocGrossValue  ( Doc doc ) = doc.gross_value 

setDocGrossValue : Folder -> DocId -> Float -> Result Error ( Folder, Doc )
setDocGrossValue  folder docId value =
    let setter = \inv -> { inv | gross_value = value }
        key = getKey docId
    in setDocField folder key setter

getDocNetValue  : Doc -> Float 
getDocNetValue ( Doc doc ) = doc.net_value 

setDocNetValue  : Folder -> DocId -> Float -> Result Error ( Folder, Doc )
setDocNetValue folder docId value =
    let setter = \inv -> { inv | net_value = value }
        key = getKey docId
    in setDocField folder key setter

getDocRemarks  : Doc -> String
getDocRemarks ( Doc doc ) = doc.remarks

setDocRemarks : Folder -> DocId -> String -> Result Error ( Folder, Doc )
setDocRemarks folder docId value =
    let setter = \inv -> { inv | remarks = value }
        key = getKey docId
    in setDocField folder key setter

getBuyerName : Doc -> String
getBuyerName ( Doc doc ) = doc.buyerName

setBuyerName : Folder -> DocId -> String -> Result Error ( Folder, Doc )
setBuyerName folder docId value =
    let setter = \inv -> { inv | buyerName = value }
        key = getKey docId
    in setDocField folder key setter

getBuyerVatin : Doc -> String
getBuyerVatin ( Doc doc ) = doc.buyerVatin

setBuyerVatin : Folder -> DocId -> String -> Result Error ( Folder, Doc )
setBuyerVatin folder docId value =    
    let setter = \inv -> { inv | buyerVatin = value }
        key = getKey docId
    in setDocField folder key setter

getBuyerAddress1 : Doc -> String
getBuyerAddress1 ( Doc doc ) = doc.buyerAddress1

setBuyerAddress1 : Folder -> DocId -> String -> Result Error ( Folder, Doc )
setBuyerAddress1 folder docId value =    
    let setter = \inv -> { inv | buyerAddress1 = value }
        key = getKey docId
    in setDocField folder key setter

getBuyerAddress2 : Doc -> String
getBuyerAddress2 ( Doc doc ) = doc.buyerAddress2

setBuyerAddress2 : Folder -> DocId -> String -> Result Error ( Folder, Doc )
setBuyerAddress2 folder docId value =    
    let setter = \inv -> { inv | buyerAddress2 = value }
        key = getKey docId
    in setDocField folder key setter

getSellerName : Doc -> String
getSellerName ( Doc doc ) = doc.sellerName

setSellerName : Folder -> DocId -> String -> Result Error ( Folder, Doc )
setSellerName folder docId value =    
    let setter = \inv -> { inv | sellerName = value }
        key = getKey docId
    in setDocField folder key setter

getSellerVatin : Doc -> String
getSellerVatin ( Doc doc ) = doc.sellerVatin

setSellerVatin : Folder -> DocId -> String -> Result Error ( Folder, Doc )
setSellerVatin folder docId value =    
    let setter = \inv -> { inv | sellerVatin = value }
        key = getKey docId
    in setDocField folder key setter

getSellerAddress1 : Doc -> String
getSellerAddress1 ( Doc doc ) = doc.sellerAddress1

setSellerAddress1 : Folder -> DocId -> String -> Result Error ( Folder, Doc )
setSellerAddress1 folder docId value =    
    let setter = \inv -> { inv | sellerAddress1 = value }
        key = getKey docId
    in setDocField folder key setter

getSellerAddress2 : Doc -> String
getSellerAddress2 ( Doc doc ) = doc.sellerAddress1

setSellerAddress2 : Folder -> DocId -> String -> Result Error ( Folder, Doc )
setSellerAddress2 folder docId value =    
    let setter = \inv -> { inv | sellerAddress2 = value }
        key = getKey docId
    in setDocField folder key setter

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
setItemField folder docItemId setter =
    case getDocByItemId folder docItemId of
        Just doc ->
            case getDocItem doc docItemId of
                Just ( DocItem item ) ->
                    let newItem = setter item 
                        ( Doc invoice ) = doc
                        newDoc = swapDocItem invoice docItemId newItem
                    in Result.Ok ( swapDoc folder invoice.id newDoc, DocItem newItem )
                Nothing -> Result.Err DocItemNotFound
        Nothing -> Result.Err DocNotFound 


setDocItemName : Folder -> DocItemId -> String -> Result Error ( Folder, DocItem )
setDocItemName folder id value =
    let setter = \item -> { item | name = value }
    in setItemField folder id setter 

getItemQuantity : DocItem -> Float 
getItemQuantity ( DocItem item ) = item.quantity

setItemQuantity : Folder -> DocItemId -> Float -> Result Error ( Folder, DocItem )
setItemQuantity folder id value =
    let setter = \item -> { item | quantity = value }
    in setItemField folder id setter 

getItemUnit : DocItem -> String 
getItemUnit ( DocItem item ) = item.unit

setItemUnit : Folder -> DocItemId -> String -> Result Error ( Folder, DocItem )
setItemUnit folder id value =
    let setter = \item -> { item | unit = value }
    in setItemField folder id setter 

getItemVatRate : DocItem -> Float 
getItemVatRate  ( DocItem item ) = item.vatRate

setItemVatRate : Folder -> DocItemId -> Float -> Result Error ( Folder, DocItem )
setItemVatRate folder id value =
    let setter = \item -> { item | vatRate = value }
    in setItemField folder id setter 

getItemGrossPrice : DocItem -> Float 
getItemGrossPrice ( DocItem item ) = item.grossPrice

setItemGrossPrice : Folder -> DocItemId -> Float -> Result Error ( Folder, DocItem )
setItemGrossPrice folder id value =
    let setter = \item -> { item | grossPrice = value }
    in setItemField folder id setter 

getItemNetPrice : DocItem -> Float 
getItemNetPrice ( DocItem item ) = item.netPrice

setItemNetPrice : Folder -> DocItemId -> Float -> Result Error ( Folder, DocItem )
setItemNetPrice folder id value =
    let setter = \item -> { item | netPrice = value }
    in setItemField folder id setter 

getItemGrossValue : DocItem -> Float
getItemGrossValue ( DocItem item ) = item.grossValue

setItemGrossValue : Folder -> DocItemId -> Float -> Result Error ( Folder, DocItem )
setItemGrossValue folder id value =
    let setter = \item -> { item | grossValue = value }
    in setItemField folder id setter 

getItemNetValue : DocItem -> Float
getItemNetValue ( DocItem item ) = item.netValue 

setItemNetValue : Folder -> DocItemId -> Float -> Result Error ( Folder, DocItem )
setItemNetValue folder id value =
    let setter = \item -> { item | netValue = value }
    in setItemField folder id setter 