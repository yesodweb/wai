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
    , DetailedSettings(..)
    , OutputFormatter
    , OutputFormatterWithDetails
    , OutputFormatterWithDetailsAndHeaders
    , Destination (..)
    , Callback
    , IPAddrSource (..)
    ) where

import System.IO (Handle, hFlush, stdout)
import qualified Data.ByteString.Builder as B (Builder, byteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Network.Wai
  ( Request(..), requestBodyLength, RequestBodyLength(..)
  , Middleware
  , Response, responseStatus, responseHeaders
  )
import System.Log.FastLogger
import Network.HTTP.Types as H
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Monoid (mconcat, (<>))
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime, UTCTime)
import Network.Wai.Parse (sinkRequestBody, lbsBackEnd, fileName, Param, File
                         , getRequestBodyType)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as S8
import System.Console.ANSI
import Data.IORef
import System.IO.Unsafe
import Network.Wai.Internal (Response (..))
import Data.Default.Class (Default (def))
import Network.Wai.Logger
import Network.Wai.Middleware.RequestLogger.Internal
import Network.Wai.Header (contentLength)
import Data.Text.Encoding (decodeUtf8')

-- | The logging format.
data OutputFormat
  = Apache IPAddrSource
  | Detailed Bool -- ^ use colors?
  | DetailedWithSettings DetailedSettings -- ^ @since 3.1.3
  | CustomOutputFormat OutputFormatter
  | CustomOutputFormatWithDetails OutputFormatterWithDetails
  | CustomOutputFormatWithDetailsAndHeaders OutputFormatterWithDetailsAndHeaders

-- | Settings for the `Detailed` `OutputFormat`.
--
-- `mModifyParams` allows you to pass a function to hide confidential
-- information (such as passwords) from the logs. If result is `Nothing`, then
-- the parameter is hidden. For example:
-- > myformat = Detailed True (Just hidePasswords)
-- >   where hidePasswords p@(k,v) = if k = "password" then (k, "***REDACTED***") else p
--
-- `mFilterRequests` allows you to filter which requests are logged, based on
-- the request and response.
--
-- @since 3.1.3
data DetailedSettings = DetailedSettings
    { useColors :: Bool
    , mModifyParams :: Maybe (Param -> Maybe Param)
    , mFilterRequests :: Maybe (Request -> Response -> Bool)
    , mPrelogRequests :: Bool -- ^ @since 3.1.7
    }

instance Default DetailedSettings where
    def = DetailedSettings
        { useColors = True
        , mModifyParams = Nothing
        , mFilterRequests = Nothing
        , mPrelogRequests = False
        }

type OutputFormatter = ZonedDate -> Request -> Status -> Maybe Integer -> LogStr

type OutputFormatterWithDetails
   = ZonedDate
  -> Request
  -> Status
  -> Maybe Integer
  -> NominalDiffTime
  -> [S8.ByteString]
  -> B.Builder
  -> LogStr

-- | Same as @OutputFormatterWithDetails@ but with response headers included
--
-- This is useful if you wish to include arbitrary application data in your
-- logs, e.g., an authenticated user ID, which you would set in a response
-- header in your application and retrieve in the log formatter.
--
-- @since 3.0.27
type OutputFormatterWithDetailsAndHeaders
   = ZonedDate -- ^ When the log message was generated
  -> Request -- ^ The WAI request
  -> Status -- ^ HTTP status code
  -> Maybe Integer -- ^ Response size
  -> NominalDiffTime -- ^ Duration of the request
  -> [S8.ByteString] -- ^ The request body
  -> B.Builder -- ^ Raw response
  -> [Header] -- ^ The response headers
  -> LogStr

data Destination = Handle Handle
                 | Logger LoggerSet
                 | Callback Callback

type Callback = LogStr -> IO ()

-- | @RequestLoggerSettings@ is an instance of Default. See <https://hackage.haskell.org/package/data-default Data.Default> for more information.
--
-- @outputFormat@, @autoFlush@, and @destination@ are record fields
-- for the record type @RequestLoggerSettings@, so they can be used to
-- modify settings values using record syntax.
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
                Handle h -> (BS.hPutStr h . logToByteString, when autoFlush (hFlush h))
                Logger l -> (pushLogStr l, when autoFlush (flushLogStr l))
                Callback c -> (c, return ())
        callbackAndFlush str = callback str >> flusher
    case outputFormat of
        Apache ipsrc -> do
            getdate <- getDateGetter flusher
            apache <- initLogger ipsrc (LogCallback callback flusher) getdate
            return $ apacheMiddleware apache
        Detailed useColors ->
            let settings = def { useColors = useColors}
            in detailedMiddleware callbackAndFlush settings
        DetailedWithSettings settings ->
            detailedMiddleware callbackAndFlush settings
        CustomOutputFormat formatter -> do
            getDate <- getDateGetter flusher
            return $ customMiddleware callbackAndFlush getDate formatter
        CustomOutputFormatWithDetails formatter -> do
            getdate <- getDateGetter flusher
            return $ customMiddlewareWithDetails callbackAndFlush getdate formatter
        CustomOutputFormatWithDetailsAndHeaders formatter -> do
            getdate <- getDateGetter flusher
            return $ customMiddlewareWithDetailsAndHeaders callbackAndFlush getdate formatter

apacheMiddleware :: ApacheLoggerActions -> Middleware
apacheMiddleware ala app req sendResponse = app req $ \res -> do
    let msize = contentLength (responseHeaders res)
    apacheLogger ala req (responseStatus res) msize
    sendResponse res

customMiddleware :: Callback -> IO ZonedDate -> OutputFormatter -> Middleware
customMiddleware cb getdate formatter app req sendResponse = app req $ \res -> do
    date <- liftIO getdate
    let msize = contentLength (responseHeaders res)
    liftIO $ cb $ formatter date req (responseStatus res) msize
    sendResponse res

customMiddlewareWithDetails :: Callback -> IO ZonedDate -> OutputFormatterWithDetails -> Middleware
customMiddlewareWithDetails cb getdate formatter app req sendResponse = do
  (req', reqBody) <- getRequestBody req
  t0 <- getCurrentTime
  app req' $ \res -> do
    t1 <- getCurrentTime
    date <- liftIO getdate
    let msize = contentLength (responseHeaders res)
    builderIO <- newIORef $ B.byteString ""
    res' <- recordChunks builderIO res
    rspRcv <- sendResponse res'
    _ <- liftIO . cb .
      formatter date req' (responseStatus res') msize (t1 `diffUTCTime` t0) reqBody =<<
      readIORef builderIO
    return rspRcv

customMiddlewareWithDetailsAndHeaders :: Callback -> IO ZonedDate -> OutputFormatterWithDetailsAndHeaders -> Middleware
customMiddlewareWithDetailsAndHeaders cb getdate formatter app req sendResponse = do
  (req', reqBody) <- getRequestBody req
  t0 <- getCurrentTime
  app req' $ \res -> do
    t1 <- getCurrentTime
    date <- liftIO getdate
    let msize = contentLength (responseHeaders res)
    builderIO <- newIORef $ B.byteString ""
    res' <- recordChunks builderIO res
    rspRcv <- sendResponse res'
    _ <- do
      rawResponse <- readIORef builderIO
      let status = responseStatus res'
          duration = t1 `diffUTCTime` t0
          resHeaders = responseHeaders res'
      liftIO . cb $ formatter date req' status msize duration reqBody rawResponse resHeaders
    return rspRcv
-- | Production request logger middleware.
--
-- This uses the 'Apache' logging format, and takes IP addresses for clients from
-- the socket (see 'IPAddrSource' for more information). It logs to 'stdout'.
{-# NOINLINE logStdout #-}
logStdout :: Middleware
logStdout = unsafePerformIO $ mkRequestLogger def { outputFormat = Apache FromSocket }

-- | Development request logger middleware.
--
-- This uses the 'Detailed' 'True' logging format and logs to 'stdout'.
{-# NOINLINE logStdoutDev #-}
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

detailedMiddleware :: Callback -> DetailedSettings -> IO Middleware
detailedMiddleware cb settings =
    let (ansiColor, ansiMethod, ansiStatusCode) =
          if useColors settings
            then (ansiColor', ansiMethod', ansiStatusCode')
            else (\_ t -> [t], (:[]), \_ t -> [t])

    in return $ detailedMiddleware' cb settings ansiColor ansiMethod ansiStatusCode

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

recordChunks :: IORef B.Builder -> Response -> IO Response
recordChunks i (ResponseStream s h sb) =
  return . ResponseStream s h $ (\send flush -> sb (\b -> modifyIORef i (<> b) >> send b) flush)
recordChunks i (ResponseBuilder s h b) =
  modifyIORef i (<> b) >> (return $ ResponseBuilder s h b)
recordChunks _ r =
  return r

getRequestBody :: Request -> IO (Request, [S8.ByteString])
getRequestBody req = do
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

detailedMiddleware' :: Callback
                    -> DetailedSettings
                    -> (Color -> BS.ByteString -> [BS.ByteString])
                    -> (BS.ByteString -> [BS.ByteString])
                    -> (BS.ByteString -> BS.ByteString -> [BS.ByteString])
                    -> Middleware
detailedMiddleware' cb DetailedSettings{..} ansiColor ansiMethod ansiStatusCode app req sendResponse = do
  (req', body) <-
      -- second tuple item should not be necessary, but a test runner might mess it up
      case (requestBodyLength req, contentLength (requestHeaders req)) of
          -- log the request body if it is small
          (KnownLength len, _) | len <= 2048 -> getRequestBody req
          (_, Just len)        | len <= 2048 -> getRequestBody req
          _ -> return (req, [])

  let reqbodylog _ = if null body || isJust mModifyParams
                      then [""]
                      else ansiColor White "  Request Body: " <> body <> ["\n"]
      reqbody = concatMap (either (const [""]) reqbodylog . decodeUtf8') body
  postParams <- if requestMethod req `elem` ["GET", "HEAD"]
      then return []
      else do (unmodifiedPostParams, files) <- liftIO $ allPostParams body
              let postParams =
                    case mModifyParams of
                      Just modifyParams -> mapMaybe modifyParams unmodifiedPostParams
                      Nothing -> unmodifiedPostParams
              return $ collectPostParams (postParams, files)

  let getParams = map emptyGetParam $ queryString req
      accept = fromMaybe "" $ lookup H.hAccept $ requestHeaders req
      params = let par | not $ null postParams = [pack (show postParams)]
                      | not $ null getParams  = [pack (show getParams)]
                      | otherwise             = []
              in if null par then [""] else ansiColor White "  Params: " <> par <> ["\n"]

  t0 <- getCurrentTime

  -- Optionally prelog the request
  when mPrelogRequests $
    cb $ "PRELOGGING REQUEST: " <> mkRequestLog params reqbody accept

  app req' $ \rsp -> do
      case mFilterRequests of
        Just f | not $ f req' rsp -> pure ()
        _ -> do
          let isRaw =
                  case rsp of
                      ResponseRaw{} -> True
                      _ -> False
              stCode = statusBS rsp
              stMsg = msgBS rsp
          t1 <- getCurrentTime

          -- log the status of the response
          cb $
            mkRequestLog params reqbody accept
            <> mkResponseLog isRaw stCode stMsg t1 t0

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
    
    mkRequestLog :: (Foldable t, ToLogStr m) => t m -> t m -> m -> LogStr
    mkRequestLog params reqbody accept =
        foldMap toLogStr (ansiMethod (requestMethod req))
        <> " "
        <> toLogStr (rawPathInfo req)
        <> "\n"
        <> foldMap toLogStr params
        <> foldMap toLogStr reqbody
        <> foldMap toLogStr (ansiColor White "  Accept: ")
        <> toLogStr accept
        <> "\n"

    mkResponseLog :: Bool -> S8.ByteString -> S8.ByteString -> UTCTime -> UTCTime -> LogStr
    mkResponseLog isRaw stCode stMsg t1 t0 =
      if isRaw then "" else
        foldMap toLogStr (ansiColor White "  Status: ")
        <> foldMap toLogStr (ansiStatusCode stCode (stCode <> " " <> stMsg))
        <> " "
        <> toLogStr (pack $ show $ diffUTCTime t1 t0)
        <> "\n"

statusBS :: Response -> BS.ByteString
statusBS = pack . show . statusCode . responseStatus

msgBS :: Response -> BS.ByteString
msgBS = statusMessage . responseStatus
