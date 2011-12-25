{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.RequestLogger
    ( logStdout
    , logHandle
    , logStdoutDev
    , logHandleDevLT
    ) where

import Network.Wai.Middleware.Debug (debug, debugHandle)

import System.IO (stdout)
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Request(..), Middleware)
import System.Log.FastLogger
import Network.HTTP.Types as H
import qualified Data.Text.Lazy as LT

-- | Inefficient, but convenient Development load logger middleware
logStdoutDev :: Middleware
logStdoutDev = debug

-- | Inefficient, but convenient Development load logger middleware
logHandleDevLT :: (LT.Text -> IO ()) -> Middleware
logHandleDevLT = debugHandle

-- | lik @logHandle@, but prints to 'stdout'
logStdout :: Middleware
logStdout = logHandle $ \bs -> hPutLogStr stdout [LB bs]

-- | Prints a message using the given callback function for each request.
-- Designed for fast production use at the expense of convenience.
-- In particular, no POST parameter information is currently given
-- For something with more useful output, use Network.Wai.Middleware.Debug
logHandle :: (BS.ByteString -> IO ()) -> Middleware
logHandle cb app req = do
    liftIO $ cb $ BS.concat
        [ requestMethod req
        , " "
        , rawPathInfo req
        , rawQueryString req
        , " "
        , "Accept: "
        , maybe "" toBS $ lookup "Accept" $ requestHeaders req
        ]
    app req

toBS :: H.Ascii -> BS.ByteString
toBS = id
