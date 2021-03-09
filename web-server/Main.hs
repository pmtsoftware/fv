{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Servant
import GHC.Generics
import Data.Aeson
import qualified Network.Wai.Handler.Warp as Warp
import Data.Yaml.Config
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.Trace
import Control.Monad.Except

import Invoices
import Configuration
import CouchdbClient
import Network.HTTP.Simple (Request)
import Data.Function ((&))

-- GET      /invoices 
-- GET      /invoices/:id 
-- PUT      /invoices/:id
-- DELETE   /invoices/:id
type InvoiceAPI =
    "invoices"
            :> Get '[JSON] [Invoice]
    :<|> "invoice"
            :> Capture "id" String
            :> Get '[JSON] Invoice
    :<|> "invoice"
            :> Capture "id" String
            :> QueryParam "rev" String
            :> ReqBody '[JSON] Invoice
            :> Put '[JSON] (Headers '[Header "ETag" String] NoContent)
    :<|> "invoice"
            :> Capture "id" String
            :> DeleteNoContent

allInvoices = []

server :: Server InvoiceAPI
server = getAll :<|> getById :<|> put :<|> delete
    where
        getAll = return allInvoices
        getById id = undefined
        put id maybeRev doc = return $ addHeader "" NoContent
        delete id = undefined

semServer :: Members '[CouchDb, Error InvoiceNotFound] r => ServerT InvoiceAPI (Sem r)
semServer = getAll :<|> get :<|> putDoc :<|> delete'
    where
        putDoc docId maybeRev doc = do
            rev' <- case maybeRev of
                        Just rev -> update docId rev doc
                        Nothing -> create docId doc
            return $ addHeader rev' NoContent
        delete' = fmap (const NoContent) . delete

createApp :: Config -> IO Application
createApp cfg = do
    return $ serve invoicesAPI (hoistServer invoicesAPI (interpretServer cfg) semServer)
    where
        interpretServer config sem = sem
            & runCouchDb
            & traceToIO
            & runReader @Request (createBaseReq config)
            & runError @InvoiceNotFound
            & runM
            & liftToHandler
        liftToHandler = Handler . ExceptT . fmap handleErrors
        handleErrors (Left _) = Left err412 { errBody = "error" }
        handleErrors (Right value) = Right value

invoicesAPI :: Proxy InvoiceAPI
invoicesAPI = Proxy

main :: IO ()
main = do
    config <- loadYamlSettings @Config ["./app.yaml"] [] ignoreEnv
    app <- createApp config
    Warp.run 8081 app
