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
import Data.Monoid (mconcat, (<>))
import Data.Time (getCurrentTime, diffUTCTime)

import Network.Wai.Parse (sinkRequestBody, lbsBackEnd, fileName, Param, File, getRequestBodyType)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as S8

import System.Console.ANSI
import Data.IORef.Lifted
import System.IO.Unsafe
import Control.Monad (unless)
import Network.Wai.Internal (Response (ResponseRaw))

import Data.Default.Class (Default (def))
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
        Detailed useColors -> detailedMiddleware
                                  (\str -> callback str >> flusher)
                                  useColors
        CustomOutputFormat formatter -> do
            getdate <- getDateGetter flusher
            return $ customMiddleware callback getdate formatter

apacheMiddleware :: ApacheLoggerActions -> Middleware
apacheMiddleware ala app req sendResponse = app req $ \res -> do
    let msize = lookup "content-length" (responseHeaders res) >>= readInt'
        readInt' bs =
            case S8.readInteger bs of
                Just (i, "") -> Just i
                _ -> Nothing
    apacheLogger ala req (responseStatus res) msize
    sendResponse res

customMiddleware :: Callback -> IO ZonedDate -> OutputFormatter -> Middleware
customMiddleware cb getdate formatter app req sendResponse = app req $ \res -> do
    date <- liftIO getdate
    -- We use Nothing for the response size since we generally don't know it
    liftIO $ cb $ formatter date req (responseStatus res) Nothing
    sendResponse res

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

-- | Prints a message using the given callback function for each request.
-- This is not for serious production use- it is inefficient.
-- It immediately consumes a POST body and fills it back in and is otherwise inefficient
--
-- Note that it logs the request immediately when it is received.
-- This meanst that you can accurately see the interleaving of requests.
-- And if the app crashes you have still logged the request.
-- However, if you are simulating 10 simultaneous users you may find this confusing.
--
-- This is lower-level - use 'logStdoutDev' unless you need greater control.
--
-- Example ouput:
--
-- > GET search
-- >   Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
-- >   Status: 200 OK 0.010555s
-- >
-- > GET static/css/normalize.css
-- >   Params: [("LXwioiBG","")]
-- >   Accept: text/css,*/*;q=0.1
-- >   Status: 304 Not Modified 0.010555s

detailedMiddleware :: Callback -> Bool -> IO Middleware
detailedMiddleware cb useColors =
    let (ansiColor, ansiMethod, ansiStatusCode) =
          if useColors
            then (ansiColor', ansiMethod', ansiStatusCode')
            else (\_ t -> [t], (:[]), \_ t -> [t])

    in return $ detailedMiddleware' cb ansiColor ansiMethod ansiStatusCode

ansiColor' :: Color -> BS.ByteString -> [BS.ByteString]
ansiColor' color bs =
    [ pack $ setSGRCode [SetColor Foreground Dull color]
    , bs
    , pack $ setSGRCode [Reset]
    ]

-- | Tags http method with a unique color.
ansiMethod' :: BS.ByteString -> [BS.ByteString]
ansiMethod' m = case m of
    "GET"    -> ansiColor' Cyan m
    "HEAD"   -> ansiColor' Cyan m
    "PUT"    -> ansiColor' Green m
    "POST"   -> ansiColor' Yellow m
    "DELETE" -> ansiColor' Red m
    _        -> ansiColor' Magenta m

ansiStatusCode' :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
ansiStatusCode' c t = case S8.take 1 c of
    "2"     -> ansiColor' Green t
    "3"     -> ansiColor' Yellow t
    "4"     -> ansiColor' Red t
    "5"     -> ansiColor' Magenta t
    _       -> ansiColor' Blue t

detailedMiddleware' :: Callback
                    -> (Color -> BS.ByteString -> [BS.ByteString])
                    -> (BS.ByteString -> [BS.ByteString])
                    -> (BS.ByteString -> BS.ByteString -> [BS.ByteString])
                    -> Middleware
detailedMiddleware' cb ansiColor ansiMethod ansiStatusCode app req sendResponse = do
    let mlen = lookup "content-length" (requestHeaders req) >>= readInt
    (req', body) <-
        case mlen of
            -- log the request body if it is small
            Just len | len <= 2048 -> do
                 let loop front = do
                        bs <- requestBody req
                        if S8.null bs
                            then return $ front []
                            else loop $ front . (bs:)
                 body <- loop id
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
                 let rbody = atomicModifyIORef ichunks $ \chunks ->
                        case chunks of
                            [] -> ([], S8.empty)
                            x:y -> (y, x)
                 let req' = req { requestBody = rbody }
                 return (req', body)
            _ -> return (req, [])

    postParams <- if requestMethod req `elem` ["GET", "HEAD"]
        then return []
        else do postParams <- liftIO $ allPostParams body
                return $ collectPostParams postParams

    let getParams = map emptyGetParam $ queryString req
        accept = fromMaybe "" $ lookup "Accept" $ requestHeaders req
        params = let par | not $ null postParams = [pack (show postParams)]
                         | not $ null getParams  = [pack (show getParams)]
                         | otherwise             = []
                 in if null par then [""] else ansiColor White "  Params: " <> par <> ["\n"]

    -- log the request immediately.
    liftIO $ cb $ mconcat $ map toLogStr $
        ansiMethod (requestMethod req) ++ [" ", rawPathInfo req, "\n"] ++
        params ++
        ansiColor White "  Accept: " ++ [accept, "\n"]

    t0 <- getCurrentTime
    app req' $ \rsp -> do
        let isRaw =
                case rsp of
                    ResponseRaw{} -> True
                    _ -> False
            stCode = statusBS rsp
            stMsg = msgBS rsp
        t1 <- getCurrentTime

        -- log the status of the response
        unless isRaw $ cb $ mconcat $ map toLogStr $
            ansiColor White "  Status: " ++
            ansiStatusCode stCode (stCode <> " " <> stMsg) ++
            [" ", pack $ show $ diffUTCTime t1 t0, "\n"]

        sendResponse rsp
  where
    allPostParams body =
        case getRequestBodyType req of
            Nothing -> return ([], [])
            Just rbt -> do
                ichunks <- newIORef body
                let rbody = atomicModifyIORef ichunks $ \chunks ->
                        case chunks of
                            [] -> ([], S8.empty)
                            x:y -> (y, x)
                sinkRequestBody lbsBackEnd rbt rbody

    emptyGetParam :: (BS.ByteString, Maybe BS.ByteString) -> (BS.ByteString, BS.ByteString)
    emptyGetParam (k, Just v) = (k,v)
    emptyGetParam (k, Nothing) = (k,"")

    collectPostParams :: ([Param], [File LBS.ByteString]) -> [Param]
    collectPostParams (postParams, files) = postParams ++
      map (\(k,v) -> (k, "FILE: " <> fileName v)) files

    readInt bs =
        case reads $ unpack bs of
            (i, _):_ -> Just (i :: Int)
            [] -> Nothing

statusBS :: Response -> BS.ByteString
statusBS = pack . show . statusCode . responseStatus

msgBS :: Response -> BS.ByteString
msgBS = statusMessage . responseStatus
