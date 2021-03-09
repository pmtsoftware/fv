{-# LANGUAGE RecordWildCards #-}

module CouchdbClient where

import Network.HTTP.Simple
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as BS
import GHC.Generics
import Data.Aeson
import Polysemy
import Polysemy.Reader
import Polysemy.Trace

-- home modules
import Configuration

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
    StoreDoc :: (ToJSON a ) => Db -> DocId -> Maybe Revision -> a -> CouchDb m Revision
    GetDoc :: (FromJSON a) => Db -> DocId -> CouchDb m (Maybe a)
    GetDocs :: (FromJSON a) => Db -> CouchDb m (Maybe (DbViewResponse a))
    DeleteDoc :: Db -> DocId -> Revision -> CouchDb m ()

makeSem ''CouchDb

createBaseReq :: Config -> Request
createBaseReq cfg = 
    setRequestHost host 
    . setRequestSecure isSecure 
    . setRequestPort port 
    . setRequestBasicAuth user pwd $ defaultRequest
    where host = pack $ dbHost cfg
          port = dbPort cfg 
          isSecure = dbSecure cfg 
          user = pack $ dbUser cfg 
          pwd = pack $ dbPassword cfg

printMe :: Members '[Embed IO] r => String -> Sem r ()
printMe txt = do 
    embed $ putStrLn txt
    return ()

applyRevision :: Maybe Revision -> Request -> Request 
applyRevision = \case
                    Just rev -> addToRequestQueryString [("rev", Just (pack rev))]
                    Nothing -> Prelude.id

runCouchDb :: Members '[Embed IO, Reader Request, Trace] r => Sem (CouchDb ': r) a -> Sem r a
runCouchDb = interpret $ \case
    StoreDoc db docId maybeRev doc -> do
        baseReq <- ask @Request
        let path = BS.concat ["/", db, "/", docId]
            req =
                setRequestBodyJSON doc
                . applyRevision maybeRev
                . setRequestPath path
                . setRequestMethod "PUT" $ baseReq
        response <- embed $ httpJSON @IO @DbResponse req
        let body = getResponseBody response
        pure $ rev body
    GetDoc db docId -> do
        baseReq <- ask @Request
        let path = BS.concat ["/", db, "/", docId]
            request = setRequestPath path . setRequestMethod "GET" $ baseReq
        response <- embed $ httpBS @IO request
        let body = getResponseBody response
            status = getResponseStatusCode response
        if status == 200 then
            pure $ decodeStrict body
        else
            pure Nothing
    GetDocs db -> do
        baseReq <- ask @Request
        let path = BS.concat ["/", db, "/_all_docs/"]
            request =
                setRequestPath path
                . setRequestQueryString [("include_docs", Just "true")] $ baseReq
        trace $ show request
        response <- embed $ httpBS @IO request
        trace $ show response
        let status = getResponseStatusCode response
            body = decodeStrict . getResponseBody $ response
        if status == 200 then
            pure body
        else
            pure Nothing
    DeleteDoc db docId rev -> do
        baseReq <- ask @Request 
        let path = BS.concat ["/", db, "/", docId]
            request = 
                setRequestPath path 
                . setRequestMethod "DELETE" 
                . addRequestHeader "If-Match" (pack rev) $ baseReq
        response <- embed $ httpNoBody @IO request
        return ()
