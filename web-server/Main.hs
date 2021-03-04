{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Servant
import GHC.Generics
import Data.Aeson
import Network.Wai.Handler.Warp (run)

import Invoices

-- GET      /invoices 
-- GET      /invoices/:id 
-- PUT      /invoices/:id
-- DELETE   /invoices/:id
type InvoiceAPI = 
    "invoices" :> Get '[JSON] [Invoice]
    :<|> "invoice" :> Capture "id" String :> Get '[JSON] Invoice
    :<|> "invoice" :> Capture "id" String :> ReqBody '[JSON] Invoice :> PutNoContent 
    :<|> "invoice" :> Capture "id" String :> DeleteNoContent

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