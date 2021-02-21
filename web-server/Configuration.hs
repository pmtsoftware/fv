module Configuration where 

import Data.ByteString (ByteString)

data Config = Config
    { dbUrl :: ByteString 
    , dbPort :: Int 
    , dbSecure :: Bool 
    , dbUser :: ByteString 
    , dbPassword :: ByteString
    }

defaultConfig = Config 
    { dbUrl = "localhost"
    , dbPort = 5984
    , dbSecure = False
    , dbUser = "fv"
    , dbPassword = "fv" 
    }