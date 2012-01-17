{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.RequestLogger
    ( logStdout
    , logHandle
    , logStdoutDev
    , logHandleDev
    , logStdoutDevLT
    , logHandleDevLT
    ) where

import System.IO (stdout, hFlush)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Request(..), Middleware)
import System.Log.FastLogger
import Network.HTTP.Types as H
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as TE

import Network.Wai.Parse (parseRequestBody, lbsSink, fileName, Param, File)
import qualified Data.ByteString.Lazy as LBS
import System.IO (hPutStrLn, stderr)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

-- | like @logHandle@, but prints to 'stdout'
logStdout :: Middleware
logStdout = logHandle $ \bs -> hPutLogStr stdout [LB bs]

-- | like @logHandleDev@, but prints to 'stdout'
--
-- Note that this flushes 'stdout' on each call to make it useful for
-- development. This is very inefficient for production use.
logStdoutDev :: Middleware
logStdoutDev = logHandleDev $ \bs -> hPutLogStr stdout [LB bs] >> hFlush stdout

-- FIXME This is not appropriately named at all. It's not working on a Handle,
-- it's working on a function. I find the functions in this module to be very
-- confusing.
-- - Michael

-- | Prints a message using the given callback function for each request.
-- Designed for fast production use at the expense of convenience.
-- In particular, no POST parameter information is currently given
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
        , "\n"
        ]
    app req

toBS :: H.Ascii -> BS.ByteString
toBS = id

-- FIXME: What's the purpose of this function? Why would it ever be preferable
-- to logStdoutDev? And why is it implemented via unpack instead of using a
-- more efficient Data.Text.IO function?
-- - Michael

-- | Inefficient, but convenient Development load logger middleware
-- Prints a message to 'stderr' for each request using logHandleDevLT
logStdoutDevLT :: Middleware
logStdoutDevLT = logHandleDevLT $ hPutStrLn stderr . LT.unpack

-- | logHandleDev, but expects Lazy Text instead of a ByteString
logHandleDevLT :: (LT.Text -> IO ()) -> Middleware
logHandleDevLT cb app req =
    logHandleDev (\msg -> cb $ LT.fromStrict $ TE.decodeUtf8 msg) app req

-- | Prints a message using the given callback function for each request.
-- This is not for serious production use- it is inefficient.
-- It immediately consumes a POST body and fills it back in and is otherwise inefficient
--
-- Note that this function will not flush output.
logHandleDev :: (BS.ByteString -> IO ()) -> Middleware
logHandleDev cb app req = do
    body <- requestBody req C.$$ CL.consume
    postParams <- if any (requestMethod req ==) ["GET", "HEAD"]
      then return []
      else do postParams <- liftIO $ allPostParams req body
              return $ collectPostParams postParams
    let getParams = map emptyGetParam $ queryString req

    liftIO $ cb $ BS.concat
        [ requestMethod req
        , " "
        , rawPathInfo req
        , "\n"
        , "Accept: "
        , maybe "" id $ lookup "Accept" $ requestHeaders req
        , paramsToBS  "GET " getParams
        , paramsToBS "POST " postParams
        , "\n"
        ]
    -- we just consumed the body- fill the enumerator back up so it is available again
    app req { requestBody = CL.sourceList body }
  where
    paramsToBS prefix params =
      if null params then ""
        else BS.concat ["\n", prefix, pack (show params)]

    allPostParams req' body = C.runResourceT $ CL.sourceList body C.$$ parseRequestBody lbsSink req'

    emptyGetParam :: (BS.ByteString, Maybe BS.ByteString) -> (BS.ByteString, BS.ByteString)
    emptyGetParam (k, Just v) = (k,v)
    emptyGetParam (k, Nothing) = (k,"")

    collectPostParams :: ([Param], [File LBS.ByteString]) -> [Param]
    collectPostParams (postParams, files) = postParams ++
      (map (\(k,v) -> (k, BS.append "FILE: " (fileName v))) files)
