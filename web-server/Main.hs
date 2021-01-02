{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified MyLib (someFunc)
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

data Invoice = Invoice 
    { id :: String 
    , no :: String 
    } deriving (Show, Generic)

instance ToJSON Invoice

allInvoices = [ Invoice "1" "FV 1", Invoice "2" "FV 2"]

server :: Server InvoiceAPI 
server = return allInvoices

invoicesAPI :: Proxy InvoiceAPI
invoicesAPI = Proxy

main :: IO ()
main = run 8081 app 
    where 
        app = serve invoicesAPI server