{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash  #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

-- | A fast, light-weight HTTP server handler for WAI. Some random notes (a FAQ, if you will):
--
-- * When a 'ResponseFile' indicates a file which does not exist, an exception
--   is thrown. This will close the connection to the client as well. You should
--   handle file existance checks at the application level.
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
    , bindPort
    , pause
    , resume
    , T.cancel
    , T.register
    , T.initialize
#if TEST
    , takeHeaders
    , readInt
#endif
    ) where

import Prelude hiding (catch, lines)
import Network.Wai

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network (sClose, Socket)
import Network.Socket
    ( accept, Family (..)
    , SocketType (Stream), listen, bindSocket, setSocketOption, maxListenQueue
    , SockAddr, SocketOption (ReuseAddr)
    , AddrInfo(..), AddrInfoFlag(..), defaultHints, getAddrInfo
    )
import qualified Network.Socket
import qualified Network.Socket.ByteString as Sock
import Control.Exception
    ( bracket, finally, Exception, SomeException, catch
    , fromException, AsyncException (ThreadKilled)
    , bracketOnError
    , IOException
    )
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe, isJust)

import Data.Typeable (Typeable)

import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
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

import Control.Monad.IO.Class (liftIO)
import qualified Timeout as T
import Timeout (Manager, registerKillThread, pause, resume)
import Data.Word (Word8)
import Data.List (foldl')
import Control.Monad (forever, when)
import qualified Network.HTTP.Types as H
import qualified Data.CaseInsensitive as CI
import System.IO (hPutStrLn, stderr)
import ReadInt (readInt64)
import qualified Data.IORef as I
import Data.String (IsString (..))

#if WINDOWS
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.MVar as MV
import Network.Socket (withSocketsDo)
#endif

import Data.Version (showVersion)
import qualified Paths_warp

warpVersion :: String
warpVersion = showVersion Paths_warp.version

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


bindPort :: Int -> HostPreference -> IO Socket
bindPort p s = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE
                                         , AI_NUMERICSERV
                                         , AI_NUMERICHOST]
                             , addrSocketType = Stream }
        host =
            case s of
                Host s' -> Just s'
                _ -> Nothing
        port = Just . show $ p
    addrs <- getAddrInfo (Just hints) host port
    -- Choose an IPv6 socket if exists.  This ensures the socket can
    -- handle both IPv4 and IPv6 if v6only is false.
    let addrs4 = filter (\x -> addrFamily x /= AF_INET6) addrs
        addrs6 = filter (\x -> addrFamily x == AF_INET6) addrs
        addrs' =
            case s of
                HostIPv4 -> addrs4 ++ addrs6
                HostIPv6 -> addrs6 ++ addrs4
                _ -> addrs

        tryAddrs (addr1:rest@(_:_)) = 
                                      catch
                                      (theBody addr1) 
                                      (\(_ :: IOException) -> tryAddrs rest)
        tryAddrs (addr1:[])         = theBody addr1
        tryAddrs _                  = error "bindPort: addrs is empty"
        theBody addr = 
          bracketOnError
          (Network.Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
          (sClose)
          (\sock -> do
              setSocketOption sock ReuseAddr 1
              bindSocket sock (addrAddress addr)
              listen sock maxListenQueue
              return sock
          )
    tryAddrs addrs'

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
    tm <- maybe (T.initialize $ settingsTimeout set * 1000000) return
        $ settingsManager set
    forever $ do
        (conn, addr) <- getConn
        _ <- forkIO $ do
            th <- T.registerKillThread tm
            serveConnection set th onE port app conn addr
            T.cancel th
        return ()

serveConnection :: Settings
                -> T.Handle
                -> (SomeException -> IO ())
                -> Port -> Application -> Connection -> SockAddr -> IO ()
serveConnection settings th onException port app conn remoteHost' =
    catch
        (finally
          (runResourceT serveConnection')
          (connClose conn))
        onException
  where
    serveConnection' :: ResourceT IO ()
    serveConnection' = do
        fromClient <- C.bufferSource $ connSource conn th
        serveConnection'' fromClient

    serveConnection'' fromClient = do
        env <- parseRequest conn port remoteHost' fromClient
        case settingsIntercept settings env of
            Nothing -> do
                -- Let the application run for as long as it wants
                liftIO $ T.pause th
                res <- app env

                -- flush the rest of the request body
                requestBody env C.$$ CL.sinkNull

                liftIO $ T.resume th
                keepAlive <- sendResponse th env conn res
                when keepAlive $ serveConnection'' fromClient
            Just intercept -> do
                liftIO $ T.pause th
                intercept fromClient conn

parseRequest :: Connection -> Port -> SockAddr
             -> C.BufferedSource IO S.ByteString
             -> ResourceT IO Request
parseRequest conn port remoteHost' src = do
    headers' <- src C.$$ takeHeaders
    parseRequest' conn port headers' remoteHost' src

-- FIXME come up with good values here
bytesPerRead, maxTotalHeaderLength :: Int
bytesPerRead = 4096
maxTotalHeaderLength = 50 * 1024

data InvalidRequest =
    NotEnoughLines [String]
    | BadFirstLine String
    | NonHttp
    | IncompleteHeaders
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
              -> C.BufferedSource IO S.ByteString
              -> ResourceT IO Request
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
    rbody <-
        if len0 == 0
            then return mempty
            else do
                -- We can't use the standard isolate, as its counter is not
                -- kept in a mutable variable.
                lenRef <- liftIO $ I.newIORef len0
                let isolate = C.Conduit push close
                    push bs = do
                        len <- liftIO $ I.readIORef lenRef
                        let (a, b) = S.splitAt len bs
                            len' = len - S.length a
                        liftIO $ I.writeIORef lenRef len'
                        return $ if len' == 0
                            then C.Finished (if S.null b then Nothing else Just b) (if S.null a then [] else [a])
                            else C.Producing isolate [a]
                    close = return []

                    -- Make sure that we don't connect to the source after the
                    -- isolate conduit closes.
                    --
                    -- Here's the issue: we fuse our buffered request body with
                    -- an isolate conduit which ensures no more than X bytes
                    -- are read. Suppose we read all X bytes, and then we call
                    -- requestBody again. What happens?
                    --
                    -- Previously, we would try to read one more chunk from the
                    -- buffered source. This is inherent to conduit: we
                    -- wouldn't know that the isolate Conduit isn't accepting
                    -- more data until after we've pushed some data to it. This
                    -- results in hanging, since there's no data available on
                    -- the wire.
                    --
                    -- Instead, we add a wrapper that checks if the request
                    -- body has already been depleted before making that first
                    -- pull.
                    --
                    -- Possible optimization: do away with the Conduit
                    -- entirely. However, this may be less efficient overall,
                    -- as we'd now have to check the BufferedSource status on
                    -- each call. Worth looking into.

                    wrap src' = C.Source
                        { C.sourcePull = do
                            len <- liftIO $ I.readIORef lenRef
                            if len <= 0
                                then return C.Closed
                                else C.sourcePull src'
                        , C.sourceClose = return ()
                        }
                return $ wrap $ src C.$= isolate
    return Request
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
            }


takeUntil :: Word8 -> ByteString -> ByteString
takeUntil c bs =
    case S.elemIndex c bs of
       Just !idx -> SU.unsafeTake idx bs
       Nothing -> bs
{-# INLINE takeUntil #-}

parseFirst :: ByteString
           -> ResourceT IO (ByteString, ByteString, ByteString, H.HttpVersion)
parseFirst s =
    case S.split 32 s of  -- ' '
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

    sendResponse' :: Response -> ResourceT IO Bool
    sendResponse' (ResponseFile s hs fp mpart) = liftIO $ do
        (lengthyHeaders, cl) <-
            case (readInt `fmap` lookup "content-length" hs, mpart) of
                (Just cl, _) -> return (hs, cl)
                (Nothing, Nothing) -> do
                    cl <- P.fileSize `fmap` P.getFileStatus fp
                    return $ addClToHeaders cl
                (Nothing, Just part) -> do
                    let cl = filePartByteCount part
                    return $ addClToHeaders cl
        connSendMany conn $ L.toChunks $ toLazyByteString $
          headers version s lengthyHeaders False
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
            connSendMany conn
                $ L.toChunks
                $ toLazyByteString
                $ headers' False
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
                      (if needsChunked' then body C.$= chunk else body)
            src C.$$ builderToByteString C.=$ connSink conn th
            return $ isKeepAlive hs
        | otherwise = liftIO $ do
            connSendMany conn
                $ L.toChunks $ toLazyByteString
                $ headers' False
            T.tickle th
            return isPersist
      where
        body = fmap (\x -> case x of
                        C.Flush -> flush
                        C.Chunk builder -> builder) bodyFlush
        headers' = headers version s hs
        -- FIXME perhaps alloca a buffer per thread and reuse that in all
        -- functions below. Should lessen greatly the GC burden (I hope)
        needsChunked' = needsChunked hs
        chunk :: C.Conduit Builder IO Builder
        chunk = C.Conduit
            { C.conduitPush = push
            , C.conduitClose = close
            }
        conduit = C.Conduit push close
        push x = return $ C.Producing conduit [chunkedTransferEncoding x]
        close = return [chunkedTransferTerminator]

parseHeaderNoAttr :: ByteString -> H.Header
parseHeaderNoAttr s =
    let (k, rest) = S.breakByte 58 s -- ':'
        restLen = S.length rest
        -- FIXME check for colon without following space?
        rest' = if restLen > 1 && SU.unsafeTake 2 rest == ": "
                   then SU.unsafeDrop 2 rest
                   else rest
     in (CI.mk k, rest')

connSource :: Connection -> T.Handle -> C.Source IO ByteString
connSource Connection { connRecv = recv } th =
    src
  where
    src = C.Source
        { C.sourcePull = do
            bs <- liftIO recv
            if S.null bs
                then return C.Closed
                else do
                    when (S.length bs >= 2048) $ liftIO $ T.tickle th
                    return (C.Open src bs)
        , C.sourceClose = return ()
        }

-- | Use 'connSendAll' to send this data while respecting timeout rules.
connSink :: Connection -> T.Handle -> C.Sink B.ByteString IO ()
connSink Connection { connSendAll = send } th =
    C.SinkData push close
  where
    close = liftIO (T.resume th)
    push x = do
        liftIO $ do
            T.resume th
            send x
            T.pause th
        return (C.Processing push close)
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
    , settingsTimeout :: Int -- ^ Timeout value in seconds. Default value: 30
    , settingsIntercept :: Request -> Maybe (C.BufferedSource IO S.ByteString -> Connection -> ResourceT IO ())
    , settingsManager :: Maybe Manager -- ^ Use an existing timeout manager instead of spawning a new one. If used, 'settingsTimeout' is ignored. Default is 'Nothing'
    }

-- | Which host to bind.
--
-- Note: The @IsString@ instance recognizes the following special values:
--
-- * @*@ means @HostAny@
--
-- * @*4@ means @HostIPv4@
--
-- * @*6@ means @HostIPv6@
data HostPreference =
    HostAny
  | HostIPv4
  | HostIPv6
  | Host String
    deriving (Show, Eq, Ord)

instance IsString HostPreference where
    -- The funny code coming up is to get around some irritating warnings from
    -- GHC. I should be able to just write:
    {-
    fromString "*" = HostAny
    fromString "*4" = HostIPv4
    fromString "*6" = HostIPv6
    -}
    fromString s'@('*':s) =
        case s of
            [] -> HostAny
            ['4'] -> HostIPv4
            ['6'] -> HostIPv6
            _ -> Host s'
    fromString s = Host s

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
                    hPutStrLn stderr $ show e
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

takeHeaders :: C.Sink ByteString IO [ByteString]
takeHeaders =
    C.sinkState (THStatus 0 id id) takeHeadersPush close
  where
    close _ = throwIO IncompleteHeaders
{-# INLINE takeHeaders #-}

takeHeadersPush :: THStatus
                -> ByteString
                -> ResourceT IO (C.SinkStateResult THStatus ByteString [ByteString])
takeHeadersPush (THStatus len _ _ ) _
    | len > maxTotalHeaderLength = throwIO OverLargeHeader
takeHeadersPush (THStatus len lines prepend) bs =
    case mnl of
        -- no newline.  prepend entire bs to next line
        Nothing -> do
            let len' = len + bsLen
            return $ C.StateProcessing $ THStatus len' lines (prepend . S.append bs)
        Just nl -> do
            let end = nl
                start = nl + 1
                line = prepend (if end > 0
                                    then SU.unsafeTake (checkCR bs end) bs
                                    else S.empty)
            if S.null line
                -- no more headers
                then do
                    let lines' = lines []
                    if start < bsLen
                        then do
                            let rest = SU.unsafeDrop start bs
                            return $ C.StateDone (Just rest) lines'
                        else return $ C.StateDone Nothing lines'
                -- more headers
                else do
                    let len' = len + start
                        lines' = lines . (:) line
                    if start < bsLen
                        then do
                            let more = SU.unsafeDrop start bs
                            takeHeadersPush (THStatus len' lines' id) more
                        else return $ C.StateProcessing $ THStatus len' lines' id
  where
    bsLen = S.length bs
    mnl = S.elemIndex 10 bs
{-# INLINE takeHeadersPush #-}

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
