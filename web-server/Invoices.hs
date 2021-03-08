module Invoices where

import Polysemy
import Data.Aeson
import GHC.Generics
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)

import CouchdbClient

data Invoice = Invoice
    { _id :: String
    , _rev :: String
    , number :: String
    , issue_date :: String
    , supply_date :: String
    , gross_value :: Double
    , net_value :: Double
    , remarks :: String
    , sellerName :: String
    , sellerVatin :: String
    , sellerAddress1 :: String
    , sellerAddress2 :: String
    , buyerName :: String
    , buyerVatin :: String
    , buyerAddress1 :: String
    , buyerAddress2 :: String
    , payment :: String
    , items :: [Item]
    } deriving (Show, Generic)

defaultInvoice = Invoice
    { _id = ""
    , _rev = ""
    , number = ""
    , issue_date = ""
    , supply_date = ""
    , gross_value = 0
    , net_value = 0
    , remarks = ""
    , sellerName = ""
    , sellerVatin = ""
    , sellerAddress1 = ""
    , sellerAddress2 = ""
    , buyerName = ""
    , buyerVatin = ""
    , buyerAddress1 = ""
    , buyerAddress2 = ""
    , payment = ""
    , items = []
    }

instance ToJSON Invoice
instance FromJSON Invoice

data Item = Item
    { itemNo :: Int
    , itemName :: String
    , itemQuantity :: Double
    , itemUnit :: String
    , itemVatRate :: Double
    , itemGrossPrice :: Double
    , itemNetPrice :: Double
    , itemGrossValue :: Double
    , itemNetValue :: Double
    } deriving (Show, Generic)

instance ToJSON  Item
instance FromJSON Item

dbName :: ByteString
dbName = "invoices"

create :: Members '[CouchDb] r => ByteString -> Invoice -> Sem r (Maybe String)
create id doc = do
    rev <- storeDoc dbName id doc
    return rev

update :: Members '[CouchDb] r => ByteString -> Invoice -> Sem r (Maybe String)
update id doc = do
    rev <- storeDoc dbName id doc
    return rev

get :: Members '[CouchDb] r => ByteString -> Sem r (Maybe Invoice)
get id = do 
    maybeDoc <- getDoc dbName id 
    return maybeDoc

getAll :: Members '[CouchDb] r => Sem r [Invoice]
getAll = do
    view <- getDocs dbName
    let docs = fmap (fmap doc . rows) view
    return $ fromMaybe [] docs
