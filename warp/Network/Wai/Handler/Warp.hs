{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
---------------------------------------------------------
--
-- Module        : Network.Wai.Handler.Warp
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- A fast, light-weight HTTP server handler for WAI.
--
---------------------------------------------------------

-- | A fast, light-weight HTTP server handler for WAI.
module Network.Wai.Handler.Warp
    ( -- * Run a Warp server
      run
    , runSettings
    , runSettingsSocket
      -- * Settings
    , Settings
    , defaultSettings
    , settingsPort
    , settingsHost
    , settingsOnException
    , settingsOnOpen
    , settingsOnClose
    , settingsTimeout
    , settingsIntercept
    , settingsManager
      -- ** Data types
    , HostPreference (..)
      -- * Connection
    , Connection (..)
    , runSettingsConnection
      -- * Datatypes
    , Port
    , InvalidRequest (..)
      -- * Internal
    , Manager
    , withManager
    , parseRequest
    , sendResponse
    , registerKillThread
    , pause
    , resume
    , T.cancel
    , T.register
    , T.initialize
#if TEST
    , takeHeaders
    , parseFirst
    , readInt
#endif
    ) where

import Prelude hiding (lines)
import Network.Wai

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network (sClose, Socket)
import Network.Socket (accept, SockAddr)
import qualified Network.Socket.ByteString as Sock
import Control.Exception
    ( mask, handle, onException, bracket
    , Exception, SomeException
    , fromException, AsyncException (ThreadKilled)
    , try
#if __GLASGOW_HASKELL__ >= 702
    , allowInterrupt
#else
    , unblock
#endif
#if WINDOWS
    , finally
#endif
    )
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe, isJust)
import Data.Char (toLower, isHexDigit)
import Data.Word (Word)

import Data.Typeable (Typeable)

import Data.Conduit
import Data.Conduit.Internal (ResumableSource (..))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Blaze (builderToByteString)
import Control.Exception.Lifted (throwIO)
import Blaze.ByteString.Builder.HTTP
    (chunkedTransferEncoding, chunkedTransferTerminator)
import Blaze.ByteString.Builder
    (copyByteString, Builder, toLazyByteString, toByteStringIO, flush)
import Blaze.ByteString.Builder.Char8 (fromChar, fromShow)
import Data.Monoid (mappend, mempty)
import Network.Sendfile

import qualified System.PosixCompat.Files as P

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Timeout as T
import Timeout (Manager, registerKillThread, pause, resume)
import Data.Word (Word8)
import Data.List (foldl')
import Control.Monad (forever, when, void)
import qualified Network.HTTP.Types as H
import qualified Data.CaseInsensitive as CI
import System.IO (hPrint, stderr)
import ReadInt (readInt64)
import qualified Data.IORef as I
import Data.Conduit.Network (bindPort, HostPreference (HostIPv4))

#if WINDOWS
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.MVar as MV
import Network.Socket (withSocketsDo)
#endif

import Data.Version (showVersion)
import qualified Paths_warp

warpVersion :: String
warpVersion = showVersion Paths_warp.version

#if __GLASGOW_HASKELL__ < 702
allowInterrupt :: IO ()
allowInterrupt = unblock $ return ()
#endif

-- |
--
-- In order to provide slowloris protection, Warp provides timeout handlers. We
-- follow these rules:
--
-- * A timeout is created when a connection is opened.
--
-- * When all request headers are read, the timeout is tickled.
--
-- * Every time at least 2048 bytes of the request body are read, the timeout
--   is tickled.
--
-- * The timeout is paused while executing user code. This will apply to both
--   the application itself, and a ResponseSource response. The timeout is
--   resumed as soon as we return from user code.
--
-- * Every time data is successfully sent to the client, the timeout is tickled.
data Connection = Connection
    { connSendMany :: [B.ByteString] -> IO ()
    , connSendAll  :: B.ByteString -> IO ()
    , connSendFile :: FilePath -> Integer -> Integer -> IO () -> IO () -- ^ offset, length
    , connClose    :: IO ()
    , connRecv     :: IO B.ByteString
    }

socketConnection :: Socket -> Connection
socketConnection s = Connection
    { connSendMany = Sock.sendMany s
    , connSendAll = Sock.sendAll s
    , connSendFile = \fp off len act -> sendfile s fp (PartOfFile off len) act
    , connClose = sClose s
    , connRecv = Sock.recv s bytesPerRead
    }

-- | Run an 'Application' on the given port. This calls 'runSettings' with
-- 'defaultSettings'.
run :: Port -> Application -> IO ()
run p = runSettings defaultSettings { settingsPort = p }

-- | Run a Warp server with the given settings.
runSettings :: Settings -> Application -> IO ()
#if WINDOWS
runSettings set app = withSocketsDo $ do
    var <- MV.newMVar Nothing
    let clean = MV.modifyMVar_ var $ \s -> maybe (return ()) sClose s >> return Nothing
    _ <- forkIO $ bracket
        (bindPort (settingsPort set) (settingsHost set))
        (const clean)
        (\s -> do
            MV.modifyMVar_ var (\_ -> return $ Just s)
            runSettingsSocket set s app)
    forever (threadDelay maxBound) `finally` clean
#else
runSettings set =
    bracket
        (bindPort (settingsPort set) (settingsHost set))
        sClose .
        flip (runSettingsSocket set)
#endif

type Port = Int

-- | Same as 'runSettings', but uses a user-supplied socket instead of opening
-- one. This allows the user to provide, for example, Unix named socket, which
-- can be used when reverse HTTP proxying into your application.
--
-- Note that the 'settingsPort' will still be passed to 'Application's via the
-- 'serverPort' record.
runSettingsSocket :: Settings -> Socket -> Application -> IO ()
runSettingsSocket set socket app =
    runSettingsConnection set getter app
  where
    getter = do
        (conn, sa) <- accept socket
        return (socketConnection conn, sa)

runSettingsConnection :: Settings -> IO (Connection, SockAddr) -> Application -> IO ()
runSettingsConnection set getConn app = do
    let onE = settingsOnException set
        port = settingsPort set
        onOpen = settingsOnOpen set
        onClose = settingsOnClose set
    tm <- maybe (T.initialize $ settingsTimeout set * 1000000) return
        $ settingsManager set
    mask $ \restore -> forever $ do
        allowInterrupt
        (conn, addr) <- getConn
        void $ forkIO $ do
            th <- T.registerKillThread tm
            handle onE $ (do onOpen
                             restore $ serveConnection set th port app conn addr
                             connClose conn >> T.cancel th >> onClose
                         ) `onException` (T.cancel th >> connClose conn >> onClose)

-- | Contains a @Source@ and a byte count that is still to be read in.
newtype IsolatedBSSource = IsolatedBSSource (I.IORef (Int, ResumableSource (ResourceT IO) ByteString))

-- | Given an @IsolatedBSSource@ provide a @Source@ that only allows up to the
-- specified number of bytes to be passed downstream. All leftovers should be
-- retained within the @Source@. If there are not enough bytes available,
-- throws a @ConnectionClosedByPeer@ exception.
ibsIsolate :: IsolatedBSSource -> Source (ResourceT IO) ByteString
ibsIsolate ibs@(IsolatedBSSource ref) = do
    (count, src) <- liftIO $ I.readIORef ref
    if count == 0
        -- No more bytes wanted downstream, so we're done.
        then return ()
        else do
            -- Get the next chunk (if available) and the updated source
            (src', mbs) <- lift $ src $$++ CL.head

            -- If no chunk available, then there aren't enough bytes in the
            -- stream. Throw a ConnectionClosedByPeer
            bs <- maybe (liftIO $ throwIO ConnectionClosedByPeer) return mbs

            let -- How many of the bytes in this chunk to send downstream
                toSend = min count (S.length bs)
                -- How many bytes will still remain to be sent downstream
                count' = count - toSend
            case () of
                ()
                    -- The expected count is greater than the size of the
                    -- chunk we just read. Send the entire chunk
                    -- downstream, and then loop on this function for the
                    -- next chunk.
                    | count' > 0 -> do
                        liftIO $ I.writeIORef ref (count', src')
                        yield bs
                        ibsIsolate ibs

                    -- The expected count is the total size of the chunk we
                    -- just read. Send this chunk downstream, and then
                    -- terminate the stream.
                    | count == S.length bs -> do
                        liftIO $ I.writeIORef ref (count', src')
                        yield bs

                    -- Some of the bytes in this chunk should not be sent
                    -- downstream. Split up the chunk into the sent and
                    -- not-sent parts, add the not-sent parts onto the new
                    -- source, and send the rest of the chunk downstream.
                    | otherwise -> do
                        let (x, y) = S.splitAt toSend bs
                        liftIO $ I.writeIORef ref (count', fmapResume (yield y >>) src')
                        yield x

-- | Extract the underlying @Source@ from an @IsolatedBSSource@, which will not
-- perform any more isolation.
ibsDone :: IsolatedBSSource -> IO (ResumableSource (ResourceT IO) ByteString)
ibsDone (IsolatedBSSource ref) = fmap snd $ I.readIORef ref

serveConnection :: Settings
                -> T.Handle
                -> Port -> Application -> Connection -> SockAddr-> IO ()
serveConnection settings th port app conn remoteHost' =
    runResourceT serveConnection'
  where
    serveConnection' :: ResourceT IO ()
    serveConnection' = do
        let fromClient = connSource conn th
        serveConnection'' fromClient

    serveConnection'' fromClient = do
        (env, getSource) <- parseRequest conn port remoteHost' fromClient
        case settingsIntercept settings env of
            Nothing -> do
                -- Let the application run for as long as it wants
                liftIO $ T.pause th
                res <- app env

                -- flush the rest of the request body
                requestBody env $$ CL.sinkNull
                ResumableSource fromClient' _ <- liftIO getSource

                liftIO $ T.resume th
                keepAlive <- sendResponse th env conn res
                when keepAlive $ serveConnection'' fromClient'
            Just intercept -> do
                liftIO $ T.pause th
                ResumableSource fromClient' _ <- liftIO getSource
                intercept fromClient' conn

parseRequest :: Connection -> Port -> SockAddr
             -> Source (ResourceT IO) S.ByteString
             -> ResourceT IO (Request, IO (ResumableSource (ResourceT IO) ByteString))
parseRequest conn port remoteHost' src1 = do
    (src2, headers') <- src1 $$+ takeHeaders
    parseRequest' conn port headers' remoteHost' src2

-- FIXME come up with good values here
bytesPerRead, maxTotalHeaderLength :: Int
bytesPerRead = 4096
maxTotalHeaderLength = 50 * 1024

data InvalidRequest =
    NotEnoughLines [String]
    | BadFirstLine String
    | NonHttp
    | IncompleteHeaders
    | ConnectionClosedByPeer
    | OverLargeHeader
    deriving (Show, Typeable, Eq)
instance Exception InvalidRequest

handleExpect :: Connection
             -> H.HttpVersion
             -> ([H.Header] -> [H.Header])
             -> [H.Header]
             -> IO [H.Header]
handleExpect _ _ front [] = return $ front []
handleExpect conn hv front (("expect", "100-continue"):rest) = do
    connSendAll conn $
        if hv == H.http11
            then "HTTP/1.1 100 Continue\r\n\r\n"
            else "HTTP/1.0 100 Continue\r\n\r\n"
    return $ front rest
handleExpect conn hv front (x:xs) = handleExpect conn hv (front . (x:)) xs

-- | Parse a set of header lines and body into a 'Request'.
parseRequest' :: Connection
              -> Port
              -> [ByteString]
              -> SockAddr
              -> ResumableSource (ResourceT IO) S.ByteString -- FIXME was buffered
              -> ResourceT IO (Request, IO (ResumableSource (ResourceT IO) ByteString))
parseRequest' _ _ [] _ _ = throwIO $ NotEnoughLines []
parseRequest' conn port (firstLine:otherLines) remoteHost' src = do
    (method, rpath', gets, httpversion) <- parseFirst firstLine
    let (host',rpath)
            | S.null rpath' = ("", "/")
            | "http://" `S.isPrefixOf` rpath' = S.breakByte 47 $ S.drop 7 rpath'
            | otherwise = ("", rpath')
    heads <- liftIO
           $ handleExpect conn httpversion id
             (map parseHeaderNoAttr otherLines)
    let host = fromMaybe host' $ lookup "host" heads
    let len0 =
            case lookup "content-length" heads of
                Nothing -> 0
                Just bs -> readInt bs
    let serverName' = takeUntil 58 host -- ':'
    let chunked = maybe False ((== "chunked") . B.map toLower)
                  $ lookup "transfer-encoding" heads
    (rbody, getSource) <- liftIO $
        if chunked
          then do
            ref <- I.newIORef (src, NeedLen)
            return (chunkedSource ref, fmap fst $ I.readIORef ref)
          else do
            ibs <- fmap IsolatedBSSource $ I.newIORef (len0, src)
            return (ibsIsolate ibs, ibsDone ibs)

    return (Request
            { requestMethod = method
            , httpVersion = httpversion
            , pathInfo = H.decodePathSegments rpath
            , rawPathInfo = rpath
            , rawQueryString = gets
            , queryString = H.parseQuery gets
            , serverName = serverName'
            , serverPort = port
            , requestHeaders = heads
            , isSecure = False
            , remoteHost = remoteHost'
            , requestBody = rbody
            , vault = mempty
            }, getSource)

data ChunkState = NeedLen
                | NeedLenNewline
                | HaveLen Word

chunkedSource :: MonadIO m
              => I.IORef (ResumableSource m ByteString, ChunkState)
              -> Source m ByteString
chunkedSource ipair = do
    (src, mlen) <- liftIO $ I.readIORef ipair
    go src mlen
  where
    go' src front = do
        (src', (len, bs)) <- lift $ src $$++ front getLen
        let src''
                | S.null bs = src'
                | otherwise = fmapResume (yield bs >>) src'
        go src'' $ HaveLen len

    go src NeedLen = go' src id
    go src NeedLenNewline = go' src (CB.take 2 >>)
    go src (HaveLen 0) = liftIO $ I.writeIORef ipair (src, HaveLen 0)
    go src (HaveLen len) = do
        (src', mbs) <- lift $ src $$++ CL.head
        case mbs of
            Nothing -> liftIO $ I.writeIORef ipair (src', HaveLen 0)
            Just bs ->
                case S.length bs `compare` fromIntegral len of
                    EQ -> yield' src' NeedLenNewline bs
                    LT -> do
                        let mlen = HaveLen $ len - fromIntegral (S.length bs)
                        yield' src' mlen bs
                    GT -> do
                        let (x, y) = S.splitAt (fromIntegral len) bs
                        let src'' = fmapResume (yield y >>) src'
                        yield' src'' NeedLenNewline x

    yield' src mlen bs = do
        liftIO $ I.writeIORef ipair (src, mlen)
        yield bs
        go src mlen

    getLen :: Monad m => Sink ByteString m (Word, ByteString)
    getLen = do
        mbs <- CL.head
        case mbs of
            Nothing -> return (0, S.empty)
            Just bs -> do
                (x, y) <-
                    case S.breakByte 10 bs of
                        (x, y)
                            | S.null y -> do
                                mbs2 <- CL.head
                                case mbs2 of
                                    Nothing -> return (x, y)
                                    Just bs2 -> return $ S.breakByte 10 $ bs `S.append` bs2
                            | otherwise -> return (x, y)
                let w =
                        S.foldl' (\i c -> i * 16 + fromIntegral (hexToWord c)) 0
                        $ B.takeWhile isHexDigit x
                return (w, S.drop 1 y)

    hexToWord w
        | w < 58 = w - 48
        | w < 71 = w - 55
        | otherwise = w - 87

takeUntil :: Word8 -> ByteString -> ByteString
takeUntil c bs =
    case S.elemIndex c bs of
       Just !idx -> SU.unsafeTake idx bs
       Nothing -> bs
{-# INLINE takeUntil #-}

parseFirst :: ByteString
           -> ResourceT IO (ByteString, ByteString, ByteString, H.HttpVersion)
parseFirst s =
    case filter (not . S.null) $ S.split 32 s of  -- ' '
        [method, query, http'] -> do
            let (hfirst, hsecond) = B.splitAt 5 http'
            if hfirst == "HTTP/"
               then let (rpath, qstring) = S.breakByte 63 query  -- '?'
                        hv =
                            case hsecond of
                                "1.1" -> H.http11
                                _ -> H.http10
                    in return (method, rpath, qstring, hv)
               else throwIO NonHttp
        _ -> throwIO $ BadFirstLine $ B.unpack s
{-# INLINE parseFirst #-} -- FIXME is this inline necessary? the function is only called from one place and not exported

httpBuilder, spaceBuilder, newlineBuilder, transferEncodingBuilder
           , colonSpaceBuilder :: Builder
httpBuilder = copyByteString "HTTP/"
spaceBuilder = fromChar ' '
newlineBuilder = copyByteString "\r\n"
transferEncodingBuilder = copyByteString "Transfer-Encoding: chunked\r\n\r\n"
colonSpaceBuilder = copyByteString ": "

headers :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> Builder
headers !httpversion !status !responseHeaders !isChunked' = {-# SCC "headers" #-}
    let !start = httpBuilder
                `mappend` copyByteString
                            (case httpversion of
                                H.HttpVersion 1 1 -> "1.1"
                                _ -> "1.0")
                `mappend` spaceBuilder
                `mappend` fromShow (H.statusCode status)
                `mappend` spaceBuilder
                `mappend` copyByteString (H.statusMessage status)
                `mappend` newlineBuilder
        !start' = foldl' responseHeaderToBuilder start (serverHeader responseHeaders)
        !end = if isChunked'
                 then transferEncodingBuilder
                 else newlineBuilder
    in start' `mappend` end

responseHeaderToBuilder :: Builder -> H.Header -> Builder
responseHeaderToBuilder b (x, y) = b
  `mappend` copyByteString (CI.original x)
  `mappend` colonSpaceBuilder
  `mappend` copyByteString y
  `mappend` newlineBuilder

checkPersist :: Request -> Bool
checkPersist req
    | ver == H.http11 = checkPersist11 conn
    | otherwise       = checkPersist10 conn
  where
    ver = httpVersion req
    conn = lookup "connection" $ requestHeaders req
    checkPersist11 (Just x)
        | CI.foldCase x == "close"      = False
    checkPersist11 _                    = True
    checkPersist10 (Just x)
        | CI.foldCase x == "keep-alive" = True
    checkPersist10 _                    = False

isChunked :: H.HttpVersion -> Bool
isChunked = (==) H.http11

hasBody :: H.Status -> Request -> Bool
hasBody s req = s /= H.Status 204 "" && s /= H.status304 &&
                H.statusCode s >= 200 && requestMethod req /= "HEAD"

sendResponse :: T.Handle
             -> Request -> Connection -> Response -> ResourceT IO Bool
sendResponse th req conn r = sendResponse' r
  where
    version = httpVersion req
    isPersist = checkPersist req
    isChunked' = isChunked version
    needsChunked hs = isChunked' && not (hasLength hs)
    isKeepAlive hs = isPersist && (isChunked' || hasLength hs)
    hasLength hs = isJust $ lookup "content-length" hs
    sendHeader = connSendMany conn . L.toChunks . toLazyByteString

    sendResponse' :: Response -> ResourceT IO Bool
    sendResponse' (ResponseFile s hs fp mpart) = do
        eres <-
            case (readInt `fmap` lookup "content-length" hs, mpart) of
                (Just cl, _) -> return $ Right (hs, cl)
                (Nothing, Nothing) -> liftIO $ try $ do
                    cl <- P.fileSize `fmap` P.getFileStatus fp
                    return $ addClToHeaders cl
                (Nothing, Just part) -> do
                    let cl = filePartByteCount part
                    return $ Right $ addClToHeaders cl
        case eres of
            Left (_ :: SomeException) -> sendResponse' $ responseLBS
                H.status404
                [("Content-Type", "text/plain")]
                "File not found"
            Right (lengthyHeaders, cl) -> liftIO $ do
                let headers' = headers version s lengthyHeaders
                sendHeader $ headers' False
                T.tickle th
                if hasBody s req then do
                    case mpart of
                        Nothing   -> connSendFile conn fp 0 cl (T.tickle th)
                        Just part -> connSendFile conn fp (filePartOffset part) (filePartByteCount part) (T.tickle th)
                    T.tickle th
                    return isPersist
                  else
                    return isPersist
      where
        addClToHeaders cl = (("Content-Length", B.pack $ show cl):hs, fromIntegral cl)

    sendResponse' (ResponseBuilder s hs b)
        | hasBody s req = liftIO $ do
              toByteStringIO (\bs -> do
                connSendAll conn bs
                T.tickle th) body
              return (isKeepAlive hs)
        | otherwise = liftIO $ do
            sendHeader $ headers' False
            T.tickle th
            return isPersist
      where
        headers' = headers version s hs
        needsChunked' = needsChunked hs
        body = if needsChunked'
                  then headers' needsChunked'
                       `mappend` chunkedTransferEncoding b
                       `mappend` chunkedTransferTerminator
                  else headers' False `mappend` b

    sendResponse' (ResponseSource s hs bodyFlush)
        | hasBody s req = do
            let src = CL.sourceList [headers' needsChunked'] `mappend`
                      (if needsChunked' then body $= chunk else body)
            src $$ builderToByteString =$ connSink conn th
            return $ isKeepAlive hs
        | otherwise = liftIO $ do
            sendHeader $ headers' False
            T.tickle th
            return isPersist
      where
        body = mapOutput (\x -> case x of
                        Flush -> flush
                        Chunk builder -> builder) bodyFlush
        headers' = headers version s hs
        -- FIXME perhaps alloca a buffer per thread and reuse that in all
        -- functions below. Should lessen greatly the GC burden (I hope)
        needsChunked' = needsChunked hs
        chunk :: Conduit Builder (ResourceT IO) Builder
        chunk = await >>= maybe (yield chunkedTransferTerminator) (\x -> yield (chunkedTransferEncoding x) >> chunk)

parseHeaderNoAttr :: ByteString -> H.Header
parseHeaderNoAttr s =
    let (k, rest) = S.breakByte 58 s -- ':'
        rest' = S.dropWhile (\c -> c == 32 || c == 9) $ S.drop 1 rest
     in (CI.mk k, rest')

connSource :: Connection -> T.Handle -> Source (ResourceT IO) ByteString
connSource Connection { connRecv = recv } th =
    src
  where
    src = do
        bs <- liftIO recv
        if S.null bs
            then return ()
            else do
                when (S.length bs >= 2048) $ liftIO $ T.tickle th
                yield bs
                src

-- | Use 'connSendAll' to send this data while respecting timeout rules.
connSink :: Connection -> T.Handle -> Sink B.ByteString (ResourceT IO) ()
connSink Connection { connSendAll = send } th =
    sink
  where
    sink = await >>= maybe close push
    close = liftIO (T.resume th)
    push x = do
        liftIO $ T.resume th
        liftIO $ send x
        liftIO $ T.pause th
        sink
    -- We pause timeouts before passing control back to user code. This ensures
    -- that a timeout will only ever be executed when Warp is in control. We
    -- also make sure to resume the timeout after the completion of user code
    -- so that we can kill idle connections.

------ The functions below are not warp-specific and could be split out into a
--separate package.

-- | Various Warp server settings. This is purposely kept as an abstract data
-- type so that new settings can be added without breaking backwards
-- compatibility. In order to create a 'Settings' value, use 'defaultSettings'
-- and record syntax to modify individual records. For example:
--
-- > defaultSettings { settingsTimeout = 20 }
data Settings = Settings
    { settingsPort :: Int -- ^ Port to listen on. Default value: 3000
    , settingsHost :: HostPreference -- ^ Default value: HostIPv4
    , settingsOnException :: SomeException -> IO () -- ^ What to do with exceptions thrown by either the application or server. Default: ignore server-generated exceptions (see 'InvalidRequest') and print application-generated applications to stderr.
    , settingsOnOpen :: IO () -- ^ What to do when a connection is open. Default: do nothing.
    , settingsOnClose :: IO ()  -- ^ What to do when a connection is close. Default: do nothing.
    , settingsTimeout :: Int -- ^ Timeout value in seconds. Default value: 30
    , settingsIntercept :: Request -> Maybe (Source (ResourceT IO) S.ByteString -> Connection -> ResourceT IO ())
    , settingsManager :: Maybe Manager -- ^ Use an existing timeout manager instead of spawning a new one. If used, 'settingsTimeout' is ignored. Default is 'Nothing'
    }

-- | The default settings for the Warp server. See the individual settings for
-- the default value.
defaultSettings :: Settings
defaultSettings = Settings
    { settingsPort = 3000
    , settingsHost = HostIPv4
    , settingsOnException = \e ->
        case fromException e of
            Just x -> go x
            Nothing ->
                when (go' $ fromException e) $
                    hPrint stderr e
    , settingsOnOpen = return ()
    , settingsOnClose = return ()
    , settingsTimeout = 30
    , settingsIntercept = const Nothing
    , settingsManager = Nothing
    }
  where
    go :: InvalidRequest -> IO ()
    go _ = return ()
    go' (Just ThreadKilled) = False
    go' _ = True

type BSEndo = ByteString -> ByteString
type BSEndoList = [ByteString] -> [ByteString]

data THStatus = THStatus
    {-# UNPACK #-} !Int -- running total byte count
    BSEndoList -- previously parsed lines
    BSEndo -- bytestrings to be prepended

takeHeaders :: Sink ByteString (ResourceT IO) [ByteString]
takeHeaders =
    await >>= maybe close (push (THStatus 0 id id))
  where
    close :: Sink ByteString (ResourceT IO) a
    close = throwIO IncompleteHeaders

    push (THStatus len lines prepend) bs
        -- Too many bytes
        | len > maxTotalHeaderLength = throwIO OverLargeHeader
        | otherwise =
            case mnl of
                -- No newline find in this chunk.  Add it to the prepend,
                -- update the length, and continue processing.
                Nothing ->
                    let len' = len + bsLen
                        prepend' = prepend . S.append bs
                        status = THStatus len' lines prepend'
                     in await >>= maybe close (push status)
                -- Found a newline, but next line continues as a multiline header
                Just (end, True) ->
                    let rest = S.drop (end + 1) bs
                        prepend' = prepend . S.append (SU.unsafeTake (checkCR bs end) bs)
                        len' = len + end
                        status = THStatus len' lines prepend'
                     in push status rest
                -- Found a newline at position end.
                Just (end, False) ->
                    let start = end + 1 -- start of next chunk
                        line
                            -- There were some bytes before the newline, get them
                            | end > 0 = prepend $ SU.unsafeTake (checkCR bs end) bs
                            -- No bytes before the newline
                            | otherwise = prepend S.empty
                     in if S.null line
                            -- no more headers
                            then
                                let lines' = lines []
                                    -- leftover
                                    rest = if start < bsLen
                                               then Just (SU.unsafeDrop start bs)
                                               else Nothing
                                 in maybe (return ()) leftover rest >> return lines'
                            -- more headers
                            else
                                let len' = len + start
                                    lines' = lines . (line:)
                                    status = THStatus len' lines' id
                                 in if start < bsLen
                                        -- more bytes in this chunk, push again
                                        then let bs' = SU.unsafeDrop start bs
                                              in push status bs'
                                        -- no more bytes in this chunk, ask for more
                                        else await >>= maybe close (push status)
      where
        bsLen = S.length bs
        mnl = do
            nl <- S.elemIndex 10 bs
            -- check if there are two more bytes in the bs
            -- if so, see if the second of those is a horizontal space
            if bsLen > nl + 1
                then
                    let c = S.index bs (nl + 1)
                     in Just (nl, c == 32 || c == 9)
                else Just (nl, False)
{-# INLINE takeHeaders #-}

checkCR :: ByteString -> Int -> Int
checkCR bs pos =
  let !p = pos - 1
  in if '\r' == B.index bs p
        then p
        else pos
{-# INLINE checkCR #-}

readInt :: Integral a => ByteString -> a
readInt bs = fromIntegral $ readInt64 bs
{-# INLINE readInt #-}


-- | Call the inner function with a timeout manager.
withManager :: Int -- ^ timeout in microseconds
            -> (Manager -> IO a)
            -> IO a
withManager timeout f = do
    -- FIXME when stopManager is available, use it
    man <- T.initialize timeout
    f man

serverHeader :: H.RequestHeaders -> H.RequestHeaders
serverHeader hdrs = case lookup key hdrs of
    Nothing  -> server : hdrs
    Just _ -> hdrs
 where
    key = "Server"
    ver = B.pack $ "Warp/" ++ warpVersion
    server = (key, ver)

fmapResume :: (Source m o1 -> Source m o2) -> ResumableSource m o1 -> ResumableSource m o2
fmapResume f (ResumableSource src m) = ResumableSource (f src) m
