{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- NOTE: Due to https://github.com/yesodweb/wai/issues/192, this module should
-- not use CPP.
module Network.Wai.Middleware.RequestLogger
    ( -- * Basic stdout logging
      logStdout
    , logStdoutDev
      -- * Create more versions
    , mkRequestLogger
    , RequestLoggerSettings
    , outputFormat
    , autoFlush
    , destination
    , OutputFormat (..)
    , OutputFormatter
    , Destination (..)
    , Callback
    , IPAddrSource (..)
    ) where

import System.IO (Handle, stdout)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Request(..), Middleware, responseStatus, Response, responseHeaders)
import System.Log.FastLogger
import Network.HTTP.Types as H
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Resource (withInternalState)
import Data.Monoid (mconcat)

import Network.Wai.Parse (sinkRequestBody, lbsBackEnd, fileName, Param, File, getRequestBodyType)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as S8

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import System.Console.ANSI
import Data.IORef.Lifted
import System.IO.Unsafe

import Data.Default (Default (def))
import Network.Wai.Logger
import Network.Wai.Middleware.RequestLogger.Internal

data OutputFormat = Apache IPAddrSource
                  | Detailed Bool -- ^ use colors?
                  | CustomOutputFormat OutputFormatter

type OutputFormatter = ZonedDate -> Request -> Status -> Maybe Integer -> LogStr

data Destination = Handle Handle
                 | Logger LoggerSet
                 | Callback Callback

type Callback = LogStr -> IO ()

data RequestLoggerSettings = RequestLoggerSettings
    {
      -- | Default value: @Detailed@ @True@.
      outputFormat :: OutputFormat
      -- | Only applies when using the @Handle@ constructor for @destination@.
      --
      -- Default value: @True@.
    , autoFlush :: Bool
      -- | Default: @Handle@ @stdout@.
    , destination :: Destination
    }

instance Default RequestLoggerSettings where
    def = RequestLoggerSettings
        { outputFormat = Detailed True
        , autoFlush = True
        , destination = Handle stdout
        }

mkRequestLogger :: RequestLoggerSettings -> IO Middleware
mkRequestLogger RequestLoggerSettings{..} = do
    let (callback, flusher) =
            case destination of
                Handle h -> (BS.hPutStr h . logToByteString, return ())
                Logger l -> (pushLogStr l, flushLogStr l)
                Callback c -> (c, return ())
    case outputFormat of
        Apache ipsrc -> do
            getdate <- getDateGetter flusher
            apache <- initLogger ipsrc (LogCallback callback (return ())) getdate
            return $ apacheMiddleware apache
        Detailed useColors -> detailedMiddleware callback useColors
        CustomOutputFormat formatter -> do
            getdate <- getDateGetter flusher
            return $ customMiddleware callback getdate formatter

apacheMiddleware :: ApacheLoggerActions -> Middleware
apacheMiddleware ala app req = do
    res <- app req
    let msize = lookup "content-length" (responseHeaders res) >>= readInt'
        readInt' bs =
            case S8.readInteger bs of
                Just (i, "") -> Just i
                _ -> Nothing
    apacheLogger ala req (responseStatus res) msize
    return res

customMiddleware :: Callback -> IO ZonedDate -> OutputFormatter -> Middleware
customMiddleware cb getdate formatter app req = do
    res <- app req
    date <- liftIO getdate
    -- We use Nothing for the response size since we generally don't know it
    liftIO $ cb $ formatter date req (responseStatus res) Nothing
    return res

-- | Production request logger middleware.
-- Implemented on top of "logCallback", but prints to 'stdout'
logStdout :: Middleware
logStdout = unsafePerformIO $ mkRequestLogger def { outputFormat = Apache FromSocket }

-- | Development request logger middleware.
-- Implemented on top of "logCallbackDev", but prints to 'stdout'
--
-- Flushes 'stdout' on each request, which would be inefficient in production use.
-- Use "logStdout" in production.
logStdoutDev :: Middleware
logStdoutDev = unsafePerformIO $ mkRequestLogger def

-- no black or white which are expected to be existing terminal colors.
colors0 :: [Color]
colors0 = [
    Red 
  , Green 
  , Yellow 
  , Blue 
  , Magenta 
  , Cyan
  ]

rotateColors :: [Color] -> ([Color], Color)
rotateColors [] = error "Impossible! There must be colors!"
rotateColors (c:cs) = (cs ++ [c], c)

-- | Prints a message using the given callback function for each request.
-- This is not for serious production use- it is inefficient.
-- It immediately consumes a POST body and fills it back in and is otherwise inefficient
--
-- Note that it logs the request immediately when it is received.
-- This meanst that you can accurately see the interleaving of requests.
-- And if the app crashes you have still logged the request.
-- However, if you are simulating 10 simultaneous users you may find this confusing.
-- The request and response are connected by color on Unix and also by the request path.
--
-- This is lower-level - use 'logStdoutDev' unless you need greater control.
--
-- Example ouput:
--
-- > GET search
-- > Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
-- >
-- > Status: 200 OK. search
-- >
-- > GET static/css/normalize.css
-- > Accept: text/css,*/*;q=0.1
-- > GET [("LXwioiBG","")]
-- >
-- > Status: 304 Not Modified. static/css/normalize.css

detailedMiddleware :: Callback -> Bool -> IO Middleware
detailedMiddleware cb useColors = do
    getAddColor <-
        if useColors
            then do
                icolors <- newIORef colors0
                return $ do
                    color <- liftIO $ atomicModifyIORef icolors rotateColors
                    return $ ansiColor color
            else return (return return)
    return $ detailedMiddleware' cb getAddColor

ansiColor :: Color -> BS.ByteString -> [BS.ByteString]
ansiColor color bs = [
    pack $ setSGRCode [SetColor Foreground Vivid color]
  , bs
  , pack $ setSGRCode [Reset]
  ]

detailedMiddleware' :: Callback
                    -> IO (BS.ByteString -> [BS.ByteString])
                    -> Middleware
detailedMiddleware' cb getAddColor app req = do
    let mlen = lookup "content-length" (requestHeaders req) >>= readInt
    (req', body) <-
        case mlen of
            -- log the request body if it is small
            Just len | len <= 2048 -> do
                 body <- requestBody req C.$$ CL.consume
                 -- logging the body here consumes it, so fill it back up
                 -- obviously not efficient, but this is the development logger
                 --
                 -- Note: previously, we simply used CL.sourceList. However,
                 -- that meant that you could read the request body in twice.
                 -- While that in itself is not a problem, the issue is that,
                 -- in production, you wouldn't be able to do this, and
                 -- therefore some bugs wouldn't show up during testing. This
                 -- implementation ensures that each chunk is only returned
                 -- once.
                 ichunks <- newIORef body
                 let rbody = do
                        chunks <- readIORef ichunks
                        case chunks of
                            [] -> return ()
                            x:xs -> do
                                writeIORef ichunks xs
                                C.yield x
                                rbody
                 let req' = req { requestBody = rbody }
                 return (req', body)
            _ -> return (req, [])

    postParams <- if requestMethod req `elem` ["GET", "HEAD"]
      then return []
      else do postParams <- liftIO $ allPostParams body
              return $ collectPostParams postParams

    let getParams = map emptyGetParam $ queryString req

    addColor <- getAddColor
    let accept = fromMaybe "" $ lookup "Accept" $ requestHeaders req

    -- log the request immediately.
    liftIO $ cb $ mconcat $ map toLogStr $ addColor (requestMethod req) ++
        [ " "
        , rawPathInfo req
        , " | "
        , accept
        , paramsToBS  "GET " getParams
        , paramsToBS "POST " postParams
        , "\n"
        ]

    rsp <- app req'

    -- log the status of the response
    -- this is color coordinated with the request logging
    -- also includes the request path to connect it to the request
    liftIO $ cb $ mconcat $ map toLogStr $
        addColor "Status: " ++ statusBS rsp ++
        [ " "
        , msgBS rsp
        , ". "
        , rawPathInfo req -- if you need help matching the 2 logging statements
        , "\n"
        ]
    return rsp
  where
    paramsToBS prefix params =
      if null params then ""
        else BS.concat ["\n", prefix, pack (show params)]

    allPostParams body =
        case getRequestBodyType req of
            Nothing -> return ([], [])
            Just rbt -> CL.sourceList body C.$$ sinkRequestBody lbsBackEnd rbt

    emptyGetParam :: (BS.ByteString, Maybe BS.ByteString) -> (BS.ByteString, BS.ByteString)
    emptyGetParam (k, Just v) = (k,v)
    emptyGetParam (k, Nothing) = (k,"")

    collectPostParams :: ([Param], [File LBS.ByteString]) -> [Param]
    collectPostParams (postParams, files) = postParams ++
      map (\(k,v) -> (k, BS.append "FILE: " (fileName v))) files

    readInt bs =
        case reads $ unpack bs of
            (i, _):_ -> Just (i :: Int)
            [] -> Nothing

statusBS :: Response -> [BS.ByteString]
statusBS rsp =
    if status > 400 then ansiColor Red bs else [bs]
  where
    bs = pack $ show status
    status = statusCode $ responseStatus rsp

msgBS :: Response -> BS.ByteString
msgBS = statusMessage . responseStatus
