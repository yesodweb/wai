{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp.Internal
import Data.ByteString.Builder (byteString)
import Debug.Trace
import Control.Concurrent
import qualified Control.Exception as E

main :: IO ()
main = do
  let settings =
        defaultSettings { 
          settingsOnClose = \_ -> msg "closed!",
          settingsOnException = \_ e -> msg ("Exception: " ++ show e) >> E.throw e
        }
  runSettings settings app

msg :: String -> IO ()
msg s = traceEventIO s


app :: Application
app req respond = E.handle onErr $ do
    connectionIsInactive req
    msg "starting handler"
    threadDelay $ 10*1000*1000
    msg "handler responding..." 
    x <- respond $ responseBuilder status200 [("Content-Type", "text/plain")] (byteString "Hello, world!")
    msg "handler done" 
    return x
  where
    onErr e =
      msg ("Handler exception: " ++ show @E.SomeException e) >> E.throw e

