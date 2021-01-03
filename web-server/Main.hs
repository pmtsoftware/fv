{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Servant
import GHC.Generics
import Data.Aeson
import Network.Wai.Handler.Warp (run)

-- GET      /invoices 
-- GET      /invoices/:id 
-- PUT      /invoices/:id
-- DELETE   /invoices/:id
type InvoiceAPI = 
    "invoices" :> Get '[JSON] [Invoice]
    :<|> "invoice" :> Capture "id" String :> Get '[JSON] Invoice
    :<|> "invoice" :> Capture "id" String :> ReqBody '[JSON] Invoice :> PutNoContent 
    :<|> "invoice" :> Capture "id" String :> DeleteNoContent

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

allInvoices = []

server :: Server InvoiceAPI 
server = getAll :<|> getById :<|> put :<|> delete 
    where 
        getAll = return allInvoices 
        getById id = undefined 
        put id doc = undefined 
        delete id = undefined

invoicesAPI :: Proxy InvoiceAPI
invoicesAPI = Proxy

main :: IO ()
main = run 8081 app 
    where 
        app = serve invoicesAPI server