{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module CouchdbClient where

import Servant.Client
import Servant.API
import GHC.Generics
import Data.Aeson
import Data.Proxy (Proxy (..))
import Web.Cookie (parseCookies)
import Data.List (find)
import qualified Data.ByteString.Char8 as BS

type CouchdbAPI =
    "_session" :> ReqBody '[JSON] Credentials :> Post '[JSON] (Headers '[Header "Set-Cookie" String] Ok)

couchdbAPI :: Proxy CouchdbAPI
couchdbAPI = Proxy

postSession = client couchdbAPI

data Ok = Ok { ok :: Bool } deriving (Show, Generic)

instance ToJSON Ok
instance FromJSON Ok

data Credentials = Credentials
    { name :: String
    , password :: String } deriving (Show, Generic)

instance ToJSON Credentials
instance FromJSON Credentials

authenticate = do
    response <- postSession $ Credentials "invoices" "invoices"
    let headers = getHeaders . getHeadersHList $ response
    return $ parseHeaders headers 
    where
        parseHeaders [] = Nothing 
        parseHeaders [(_, cookie)] = 
            let extract = BS.unpack . snd 
                findSetCookie = find $ (==) "Set-Cookie" . fst
            in fmap extract $ findSetCookie $ parseCookies cookie
        parseHeaders _ = Nothing