module Configuration where 

import Data.ByteString (ByteString)
import GHC.Generics
import Data.Aeson

data Config = Config
    { dbHost :: String 
    , dbPort :: Int 
    , dbSecure :: Bool 
    , dbUser :: String 
    , dbPassword :: String
    } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config