module Configuration where 

import Data.ByteString (ByteString)

data Config = Config
    { dbHost :: ByteString 
    , dbPort :: Int 
    , dbSecure :: Bool 
    , dbUser :: ByteString 
    , dbPassword :: ByteString
    }

defaultConfig = Config 
    { dbHost = "localhost"
    , dbPort = 5984
    , dbSecure = False
    , dbUser = "fv"
    , dbPassword = "fv" 
    }