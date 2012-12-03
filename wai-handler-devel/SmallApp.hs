{-# LANGUAGE OverloadedStrings #-}
module SmallApp where

import Network.Wai
import Network.HTTP.Types
import Data.ByteString.Lazy.Char8 ()

testapp' :: Application
testapp' _ = return $ responseLBS status200 [("Content-Type", "text/html")] "test"

smallApp :: (Application -> IO ()) -> IO ()
smallApp f = f testapp'
