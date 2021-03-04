{-# LANGUAGE RecordWildCards #-}

module CouchdbClient where

import Network.HTTP.Simple
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import GHC.Generics
import Data.Aeson
import Polysemy
import Polysemy.Reader

-- home modules
import Configuration

createBaseReq :: Config -> Request
createBaseReq Config {..} =
    setRequestBasicAuth dbUser dbPassword
    . setRequestPort dbPort
    . setRequestSecure dbSecure
    . setRequestHost dbHost $ defaultRequest

data DbResponse = DbResponse
    { id :: String
    , ok :: Bool
    , rev :: String
    } deriving (Show, Generic)

instance ToJSON DbResponse
instance FromJSON DbResponse

data DbViewResponse a = DbViewResponse
    { offset :: Int
    , rows :: [DbKeyValue a]
    , total_rows :: Int
    }
    deriving (Show, Generic)

data DbKeyValue a = DbKeyValue
    { id :: String
    , key :: String
    , value :: String
    , doc :: a
    } deriving (Show, Generic)

instance ToJSON a => ToJSON (DbKeyValue a)
instance FromJSON a => FromJSON (DbKeyValue a)

instance ToJSON a => ToJSON (DbViewResponse a)
instance FromJSON a => FromJSON (DbViewResponse a)

type Db = ByteString
type DocId = ByteString

type Revision = String

data CouchDb m a where
    StoreDoc :: (ToJSON a ) => Db -> DocId -> a -> CouchDb m (Maybe Revision)
    GetDoc :: (FromJSON a) => Db -> DocId -> CouchDb m (Maybe a)
    GetDocs :: (FromJSON a) => Db -> CouchDb m (Maybe (DbViewResponse a))

makeSem ''CouchDb

runCouchDb :: Members '[Embed IO, Reader Config] r => Sem (CouchDb ': r) a -> Sem r a
runCouchDb = interpret $ \case
    StoreDoc db docId doc -> do
        cfg <- ask @Config
        let path = BS.concat ["/", db, "/", docId]
            baseReq = createBaseReq cfg
            req =
                setRequestBodyJSON doc
                . setRequestPath path
                . setRequestMethod "PUT" $ baseReq
        response <- embed $ httpJSON @IO @DbResponse req
        let body = getResponseBody response
        pure $ Just (rev body)
    GetDoc db docId -> do
        cfg <- ask @Config
        let path = BS.concat ["/", db, "/", docId]
            request = setRequestPath path . setRequestMethod "GET" $ createBaseReq cfg
        response <- embed $ httpBS @IO request
        let body = getResponseBody response
            status = getResponseStatusCode response
        if status == 200 then
            pure $ decodeStrict body
        else
            pure Nothing
    GetDocs db -> do
        cfg <- ask @Config
        let path = BS.concat ["/", db, "/_all_docs/"]
            request =
                setRequestPath path
                . setRequestQueryString [("include_docs", Just "true")] $ createBaseReq cfg
        response <- embed $ httpBS @IO request
        let status = getResponseStatusCode response
            body = decodeStrict . getResponseBody $ response
        if status == 200 then
            pure body
        else
            pure Nothing