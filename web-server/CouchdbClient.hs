module CouchdbClient where

import Network.HTTP.Simple
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS 
import Data.Maybe (maybeToList)
import GHC.Generics
import Data.Aeson 
import Control.Monad.IO.Class (MonadIO)
import Polysemy
import Polysemy.Reader

baseReq = 
    setRequestBasicAuth "fv" "fv" 
    . setRequestPort 5984 
    . setRequestSecure False 
    . setRequestHost "localhost" $ defaultRequest

data DbResponse = DbResponse 
    { id :: String
    , ok :: Bool
    , rev :: String
    } deriving (Show, Generic)

instance ToJSON DbResponse 
instance FromJSON DbResponse

data DbViewResponse = DbViewResponse 
    { offset :: Int 
    , rows :: [DbKeyValue] 
    , total_rows :: Int
    }
    deriving (Show, Generic)

data DbKeyValue = DbKeyValue 
    { id :: String
    , key :: String
    , value :: String
    } deriving (Show, Generic)

instance ToJSON DbKeyValue
instance FromJSON DbKeyValue

instance ToJSON DbViewResponse
instance FromJSON DbViewResponse

type Db = ByteString
type DocId = ByteString
data CouchDb m a where
    StoreDoc :: (ToJSON a ) => Db -> DocId -> a -> CouchDb m ()

makeSem ''CouchDb

runCouchDb :: Members '[Embed IO, Reader String] r => Sem (CouchDb ': r) a -> Sem r a 
runCouchDb = interpret $ \case 
    StoreDoc db docId doc -> do 
        response <- embed $ putDoc db docId doc
        return ()    

readInt = read @Int

putDoc :: ToJSON a => ByteString -> ByteString -> a -> IO DbResponse
putDoc db docId entity = do 
    resp <- httpJSON req
    let responseBody = getResponseBody resp
    return responseBody
    where
        req = 
            setRequestBodyJSON entity 
            . setRequestPath path 
            . setRequestMethod "PUT" $ baseReq
        path = BS.concat ["/", db, "/", docId]

postDoc :: ToJSON a => ByteString -> a -> IO DbResponse
postDoc db entity = do 
    resp <- httpJSON req
    let responseBody = getResponseBody resp
    return responseBody
    where
        req = 
            setRequestBodyJSON entity 
            . setRequestPath path 
            . setRequestMethod "POST" $ baseReq
        path = BS.concat ["/", db]

getDoc :: FromJSON a => ByteString -> ByteString ->  IO a
getDoc db docId = do 
    resp <- httpJSON req 
    return $ getResponseBody resp 
    where 
        req = 
            setRequestPath path 
            . setRequestMethod "GET" $ baseReq
        path = BS.concat ["/", db, "/", docId]

getView :: ByteString -> ByteString -> ByteString -> IO DbViewResponse
getView db doc view = do 
    resp <- httpJSON req 
    return $ getResponseBody resp 
    where 
        req = 
            setRequestPath path 
            . setRequestMethod "GET" $ baseReq 
        path = BS.concat ["/", db, "/_design/", doc, "/_view/", view]