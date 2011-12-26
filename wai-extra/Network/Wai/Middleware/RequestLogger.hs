{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.RequestLogger
    ( logStdout
    , logHandle
    , logStdoutDevLT
    , logHandleDevLT
    ) where

import System.IO (stdout)
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Request(..), Middleware)
import System.Log.FastLogger
import Network.HTTP.Types as H
import qualified Data.Text.Lazy as LT

import Network.Wai.Parse (parseRequestBody, lbsSink, fileName, Param, File)
import qualified Data.ByteString.Lazy as LBS
import Data.Enumerator (run_, ($$), enumList)
import Data.Enumerator.List (consume)
import System.IO (hPutStrLn, stderr)
import Data.ByteString.Char8 (unpack)

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

-- | Inefficient, but convenient Development load logger middleware
-- Prints a message to 'stderr' for each request using logHandleDevLT
logStdoutDevLT :: Middleware
logStdoutDevLT = logHandleDevLT $ hPutStrLn stderr . LT.unpack

-- | Prints a message using the given callback function for each request.
-- This is not for serious production use- it is inefficient.
-- It immediately consumes a POST body and fills it back in and is otherwise inefficient
-- For production use use module Network.Wai.Middleware.RequestLogger
logHandleDevLT :: (LT.Text -> IO ()) -> Middleware
logHandleDevLT cb app req = do
    body <- consume
    postParams <- if any (requestMethod req ==) ["GET", "HEAD"]
      then return []
      else do postParams <- liftIO $ allPostParams req body
              return $ collectPostParams postParams
    let getParams = map emptyGetParam $ queryString req

    liftIO $ cb $ LT.pack $ concat
        [ unpack $ requestMethod req
        , " "
        , unpack $ rawPathInfo req
        , "\n"
        , (++) "Accept: " $ maybe "" unpack $ lookup "Accept" $ requestHeaders req
        , paramsToStr  "GET " getParams
        , paramsToStr "POST " postParams
        ]
    -- we just consumed the body- fill the enumerator back up so it is available again
    liftIO $ run_ $ enumList 1 body $$ app req
  where
    paramsToStr prefix params = if null params then "" else "\n" ++ prefix ++ (show params)

    allPostParams req' body = run_ $ enumList 1 body $$ parseRequestBody lbsSink req'

    emptyGetParam :: (BS.ByteString, Maybe BS.ByteString) -> (BS.ByteString, BS.ByteString)
    emptyGetParam (k, Just v) = (k,v)
    emptyGetParam (k, Nothing) = (k,"")

    collectPostParams :: ([Param], [File LBS.ByteString]) -> [Param]
    collectPostParams (postParams, files) = postParams ++
      (map (\(k,v) -> (k, BS.append "FILE: " (fileName v))) files)
