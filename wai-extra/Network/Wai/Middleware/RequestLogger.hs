{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.RequestLogger
    ( logStdout
    , logCallback
    , logStdoutDev
    , logCallbackDev
    -- * Deprecated
    , logHandle
    , logHandleDev
    ) where

import System.IO (stdout, hFlush)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Request(..), Middleware, responseStatus)
import System.Log.FastLogger
import Network.HTTP.Types as H

import Network.Wai.Parse (sinkRequestBody, lbsBackEnd, fileName, Param, File, getRequestBodyType)
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import System.Console.ANSI
import Data.IORef
import System.IO.Unsafe

logHandle :: (BS.ByteString -> IO ()) -> Middleware
logHandle = logCallback
{-# DEPRECATED logHandle "Please use logCallback instead." #-}
logHandleDev :: (BS.ByteString -> IO ()) -> Middleware
logHandleDev = logCallbackDev
{-# DEPRECATED logHandleDev "Please use logCallbackDev instead." #-}

-- | Production request logger middleware.
-- Implemented on top of "logCallback", but prints to 'stdout'
logStdout :: Middleware
logStdout = logHandle $ \bs -> hPutLogStr stdout [LB bs]

-- | Development request logger middleware.
-- Implemented on top of "logCallbackDev", but prints to 'stdout'
--
-- Flushes 'stdout' on each request, which would be inefficient in production use.
-- Use "logStdout" in production.
logStdoutDev :: Middleware
logStdoutDev = logHandleDev $ \bs -> hPutLogStr stdout [LB bs] >> hFlush stdout

-- | Prints a message using the given callback function for each request.
-- Designed for fast production use at the expense of convenience.
-- In particular, no POST parameter information is currently given
--
-- This is lower-level - use "logStdout" unless you need this greater control
logCallback :: (BS.ByteString -> IO ()) -- ^ A function that logs the ByteString log message.
            -> Middleware
logCallback cb app req = do
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

-- no black or white which are expected to be used already
colors :: IORef [Color]
colors = unsafePerformIO $ newIORef $ [
    Red 
  , Green 
  , Yellow 
  , Blue 
  , Magenta 
  , Cyan
  ]

-- | Prints a message using the given callback function for each request.
-- This is not for serious production use- it is inefficient.
-- It immediately consumes a POST body and fills it back in and is otherwise inefficient
--
-- Note that it logs the request immediately when it is received.
-- This means if the app crashes you have still logged the request.
-- However, if you are simulating 10 simultaneous users you may find this confusing.
--
-- This is lower-level - use "logStdoutDev" unless you need this greater control
logCallbackDev :: (BS.ByteString -> IO ()) -- ^ A function that logs the ByteString log message.
               -> Middleware
logCallbackDev cb app req = do
    let mlen = lookup "content-length" (requestHeaders req) >>= readInt
    (req', body) <-
        case mlen of
            Just len | len <= 2048 -> do
                body <- requestBody req C.$$ CL.consume
                let req' = req { requestBody = CL.sourceList body }
                return (req', body)
            _ -> return (req, [])
    postParams <- if any (requestMethod req ==) ["GET", "HEAD"]
      then return []
      else do postParams <- liftIO $ allPostParams body
              return $ collectPostParams postParams
    let getParams = map emptyGetParam $ queryString req

    color <- liftIO $ atomicModifyIORef colors (\(c:cs) -> (cs ++ [c], c))

    liftIO $ cb $ BS.concat $ ansiColor color (requestMethod req) ++
        [ " "
        , rawPathInfo req
        , "\n"
        , "Accept: "
        , maybe "" id $ lookup "Accept" $ requestHeaders req
        , paramsToBS  "GET " getParams
        , paramsToBS "POST " postParams
        , "\n"
        ]

    -- The body was consumed. Fill it back up so it is available again
    rsp <- app req'

    liftIO $ cb $ BS.concat $ ansiColor color "Status: " ++ [
          sCode rsp
        , " "
        , msg rsp
        , ". "
        , rawPathInfo req -- if you need help matching the 2 logging statements
        , "\n"
      ]
    return rsp
  where
    ansiColor color bs = [
        pack $ setSGRCode [SetColor Foreground Vivid color]
      , bs
      , pack $ setSGRCode [Reset]
      ]
    sCode = pack . show . statusCode . responseStatus
    msg = statusMessage . responseStatus
    paramsToBS prefix params =
      if null params then ""
        else BS.concat ["\n", prefix, pack (show params)]

    allPostParams body =
        case getRequestBodyType req of
            Nothing -> return ([], [])
            Just rbt -> C.runResourceT $ CL.sourceList body C.$$ sinkRequestBody lbsBackEnd rbt

    emptyGetParam :: (BS.ByteString, Maybe BS.ByteString) -> (BS.ByteString, BS.ByteString)
    emptyGetParam (k, Just v) = (k,v)
    emptyGetParam (k, Nothing) = (k,"")

    collectPostParams :: ([Param], [File LBS.ByteString]) -> [Param]
    collectPostParams (postParams, files) = postParams ++
      (map (\(k,v) -> (k, BS.append "FILE: " (fileName v))) files)

    readInt bs =
        case reads $ unpack bs of
            (i, _):_ -> Just (i :: Int)
            [] -> Nothing
