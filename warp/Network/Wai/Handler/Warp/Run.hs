{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Network.Wai.Handler.Warp.Run where

import "iproute" Data.IP (toHostAddress, toHostAddress6)
import Control.Arrow (first)
import qualified Control.Concurrent as Conc (yield)
import Control.Exception as E
import qualified Data.ByteString as S
import Data.Char (chr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Streaming.Network (bindPortTCP)
import Foreign.C.Error (Errno(..), eCONNABORTED)
import GHC.IO.Exception (IOException(..))
import Network.Socket (Socket, close, accept, withSocketsDo, SockAddr(SockAddrInet, SockAddrInet6), setSocketOption, SocketOption(..))
import qualified Network.Socket.ByteString as Sock
import Network.Wai
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import System.Environment (getEnvironment)
import System.Timeout (timeout)

import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Counter
import qualified Network.Wai.Handler.Warp.Date as D
import qualified Network.Wai.Handler.Warp.FdCache as F
import qualified Network.Wai.Handler.Warp.FileInfoCache as I
import Network.Wai.Handler.Warp.HTTP2 (http2, isHTTP2)
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.Imports hiding (readInt)
import Network.Wai.Handler.Warp.ReadInt
import Network.Wai.Handler.Warp.Recv
import Network.Wai.Handler.Warp.Request
import Network.Wai.Handler.Warp.Response
import Network.Wai.Handler.Warp.SendFile
import Network.Wai.Handler.Warp.Settings
import qualified Network.Wai.Handler.Warp.Timeout as T
import Network.Wai.Handler.Warp.Types


#if WINDOWS
import Network.Wai.Handler.Warp.Windows
#else
import Network.Socket (fdSocket)
#endif

-- | Creating 'Connection' for plain HTTP based on a given socket.
socketConnection :: Socket -> IO Connection
socketConnection s = do
    bufferPool <- newBufferPool
    writeBuf <- allocateBuffer bufferSize
    let sendall = Sock.sendAll s
    return Connection {
        connSendMany = Sock.sendMany s
      , connSendAll = sendall
      , connSendFile = sendFile s writeBuf bufferSize sendall
      , connClose = close s
      , connFree = freeBuffer writeBuf
      , connRecv = receive s bufferPool
      , connRecvBuf = receiveBuf s
      , connWriteBuffer = writeBuf
      , connBufferSize = bufferSize
      }

-- | Run an 'Application' on the given port.
-- This calls 'runSettings' with 'defaultSettings'.
run :: Port -> Application -> IO ()
run p = runSettings defaultSettings { settingsPort = p }

-- | Run an 'Application' on the port present in the @PORT@
-- environment variable. Uses the 'Port' given when the variable is unset.
-- This calls 'runSettings' with 'defaultSettings'.
--
-- Since 3.0.9
runEnv :: Port -> Application -> IO ()
runEnv p app = do
    mp <- lookup "PORT" <$> getEnvironment

    maybe (run p app) runReadPort mp

  where
    runReadPort :: String -> IO ()
    runReadPort sp = case reads sp of
        ((p', _):_) -> run p' app
        _ -> fail $ "Invalid value in $PORT: " ++ sp

-- | Run an 'Application' with the given 'Settings'.
-- This opens a listen socket on the port defined in 'Settings' and
-- calls 'runSettingsSocket'.
runSettings :: Settings -> Application -> IO ()
runSettings set app = withSocketsDo $
    bracket
        (bindPortTCP (settingsPort set) (settingsHost set))
        close
        (\socket -> do
            setSocketCloseOnExec socket
            runSettingsSocket set socket app)

-- | This installs a shutdown handler for the given socket and
-- calls 'runSettingsConnection' with the default connection setup action
-- which handles plain (non-cipher) HTTP.
-- When the listen socket in the second argument is closed, all live
-- connections are gracefully shut down.
--
-- The supplied socket can be a Unix named socket, which
-- can be used when reverse HTTP proxying into your application.
--
-- Note that the 'settingsPort' will still be passed to 'Application's via the
-- 'serverPort' record.
runSettingsSocket :: Settings -> Socket -> Application -> IO ()
runSettingsSocket set socket app = do
    settingsInstallShutdownHandler set closeListenSocket
    runSettingsConnection set getConn app
  where
    getConn = do
#if WINDOWS
        (s, sa) <- windowsThreadBlockHack $ accept socket
#else
        (s, sa) <- accept socket
#endif
        setSocketCloseOnExec s
        -- NoDelay causes an error for AF_UNIX.
        setSocketOption s NoDelay 1 `E.catch` \(E.SomeException _) -> return ()
        conn <- socketConnection s
        return (conn, sa)

    closeListenSocket = close socket

-- | The connection setup action would be expensive. A good example
-- is initialization of TLS.
-- So, this converts the connection setup action to the connection maker
-- which will be executed after forking a new worker thread.
-- Then this calls 'runSettingsConnectionMaker' with the connection maker.
-- This allows the expensive computations to be performed
-- in a separate worker thread instead of the main server loop.
--
-- Since 1.3.5
runSettingsConnection :: Settings -> IO (Connection, SockAddr) -> Application -> IO ()
runSettingsConnection set getConn app = runSettingsConnectionMaker set getConnMaker app
  where
    getConnMaker = do
      (conn, sa) <- getConn
      return (return conn, sa)

-- | This modifies the connection maker so that it returns 'TCP' for 'Transport'
-- (i.e. plain HTTP) then calls 'runSettingsConnectionMakerSecure'.
runSettingsConnectionMaker :: Settings -> IO (IO Connection, SockAddr) -> Application -> IO ()
runSettingsConnectionMaker x y =
    runSettingsConnectionMakerSecure x (toTCP <$> y)
  where
    toTCP = first ((, TCP) <$>)

----------------------------------------------------------------

-- | The core run function which takes 'Settings',
-- a connection maker and 'Application'.
-- The connection maker can return a connection of either plain HTTP
-- or HTTP over TLS.
--
-- Since 2.1.4
runSettingsConnectionMakerSecure :: Settings -> IO (IO (Connection, Transport), SockAddr) -> Application -> IO ()
runSettingsConnectionMakerSecure set getConnMaker app = do
    settingsBeforeMainLoop set
    counter <- newCounter
    withII0 $ acceptConnection set getConnMaker app counter
  where
    withII0 action =
        withTimeoutManager $ \tm ->
        D.withDateCache $ \dc ->
        F.withFdCache fdCacheDurationInSeconds $ \fdc ->
        I.withFileInfoCache fdFileInfoDurationInSeconds $ \fic -> do
            let ii0 = InternalInfo0 tm dc fdc fic
            action ii0

    !fdCacheDurationInSeconds = settingsFdCacheDuration set * 1000000
    !fdFileInfoDurationInSeconds = settingsFileInfoCacheDuration set * 1000000
    !timeoutInSeconds = settingsTimeout set * 1000000
    withTimeoutManager f = case settingsManager set of
        Just tm -> f tm
        Nothing -> bracket
                   (T.initialize timeoutInSeconds)
                   T.stopManager
                   f

-- Note that there is a thorough discussion of the exception safety of the
-- following code at: https://github.com/yesodweb/wai/issues/146
--
-- We need to make sure of two things:
--
-- 1. Asynchronous exceptions are not blocked entirely in the main loop.
--    Doing so would make it impossible to kill the Warp thread.
--
-- 2. Once a connection maker is received via acceptNewConnection, the
--    connection is guaranteed to be closed, even in the presence of
--    async exceptions.
--
-- Our approach is explained in the comments below.
acceptConnection :: Settings
                 -> IO (IO (Connection, Transport), SockAddr)
                 -> Application
                 -> Counter
                 -> InternalInfo0
                 -> IO ()
acceptConnection set getConnMaker app counter ii0 = do
    -- First mask all exceptions in acceptLoop. This is necessary to
    -- ensure that no async exception is throw between the call to
    -- acceptNewConnection and the registering of connClose.
    void $ mask_ acceptLoop
    gracefulShutdown set counter
  where
    acceptLoop = do
        -- Allow async exceptions before receiving the next connection maker.
        allowInterrupt

        -- acceptNewConnection will try to receive the next incoming
        -- request. It returns a /connection maker/, not a connection,
        -- since in some circumstances creating a working connection
        -- from a raw socket may be an expensive operation, and this
        -- expensive work should not be performed in the main event
        -- loop. An example of something expensive would be TLS
        -- negotiation.
        mx <- acceptNewConnection
        case mx of
            Nothing             -> return ()
            Just (mkConn, addr) -> do
                fork set mkConn addr app counter ii0
                acceptLoop

    acceptNewConnection = do
        ex <- try getConnMaker
        case ex of
            Right x -> return $ Just x
            Left e -> do
                let eConnAborted = getErrno eCONNABORTED
                    getErrno (Errno cInt) = cInt
                if ioe_errno e == Just eConnAborted
                    then acceptNewConnection
                    else do
                        settingsOnException set Nothing $ toException e
                        return Nothing

-- Fork a new worker thread for this connection maker, and ask for a
-- function to unmask (i.e., allow async exceptions to be thrown).
fork :: Settings
     -> IO (Connection, Transport)
     -> SockAddr
     -> Application
     -> Counter
     -> InternalInfo0
     -> IO ()
fork set mkConn addr app counter ii0 = settingsFork set $ \unmask ->
    -- Call the user-supplied on exception code if any
    -- exceptions are thrown.
    handle (settingsOnException set Nothing) .
    -- Allocate a new IORef indicating whether the connection has been
    -- closed, to avoid double-freeing a connection
    withClosedRef $ \ref ->
        -- Run the connection maker to get a new connection, and ensure
        -- that the connection is closed. If the mkConn call throws an
        -- exception, we will leak the connection. If the mkConn call is
        -- vulnerable to attacks (e.g., Slowloris), we do nothing to
        -- protect the server. It is therefore vital that mkConn is well
        -- vetted.
        --
        -- We grab the connection before registering timeouts since the
        -- timeouts will be useless during connection creation, due to the
        -- fact that async exceptions are still masked.
        bracket mkConn (cleanUp ref) (serve unmask ref)
  where
    withClosedRef inner = newIORef False >>= inner

    closeConn ref conn = do
        isClosed <- atomicModifyIORef' ref $ \x -> (True, x)
        unless isClosed $ connClose conn

    cleanUp ref (conn, _) = closeConn ref conn `finally` connFree conn

    -- We need to register a timeout handler for this thread, and
    -- cancel that handler as soon as we exit. We additionally close
    -- the connection immediately in case the child thread catches the
    -- async exception or performs some long-running cleanup action.
    serve unmask ref (conn, transport) = bracket register cancel $ \th -> do
        let ii1 = toInternalInfo1 ii0 th
        -- We now have fully registered a connection close handler in
        -- the case of all exceptions, so it is safe to one again
        -- allow async exceptions.
        unmask .
            -- Call the user-supplied code for connection open and
            -- close events
           bracket (onOpen addr) (onClose addr) $ \goingon ->
           -- Actually serve this connection.  bracket with closeConn
           -- above ensures the connection is closed.
           when goingon $ serveConnection conn ii1 addr transport set app
      where
        register = T.registerKillThread (timeoutManager0 ii0)
                                        (closeConn ref conn)
        cancel   = T.cancel

    onOpen adr    = increase counter >> settingsOnOpen  set adr
    onClose adr _ = decrease counter >> settingsOnClose set adr

serveConnection :: Connection
                -> InternalInfo1
                -> SockAddr
                -> Transport
                -> Settings
                -> Application
                -> IO ()
serveConnection conn ii1 origAddr transport settings app = do
    -- fixme: Upgrading to HTTP/2 should be supported.
    (h2,bs) <- if isHTTP2 transport then
                   return (True, "")
                 else do
                   bs0 <- connRecv conn
                   if S.length bs0 >= 4 && "PRI " `S.isPrefixOf` bs0 then
                       return (True, bs0)
                     else
                       return (False, bs0)
    istatus <- newIORef False
    if settingsHTTP2Enabled settings && h2 then do
        rawRecvN <- makeReceiveN bs (connRecv conn) (connRecvBuf conn)
        let recvN = wrappedRecvN th istatus (settingsSlowlorisSize settings) rawRecvN
        -- fixme: origAddr
        http2 conn ii1 origAddr transport settings recvN app
      else do
        src <- mkSource (wrappedRecv conn th istatus (settingsSlowlorisSize settings))
        writeIORef istatus True
        leftoverSource src bs
        addr <- getProxyProtocolAddr src
        http1 True addr istatus src `E.catch` \e ->
          case fromException e of
            -- See comment below referencing
            -- https://github.com/yesodweb/wai/issues/618
            Just NoKeepAliveRequest -> return ()
            Nothing -> do
              sendErrorResponse addr istatus e
              throwIO e
  where
    getProxyProtocolAddr src =
        case settingsProxyProtocol settings of
            ProxyProtocolNone ->
                return origAddr
            ProxyProtocolRequired -> do
                seg <- readSource src
                parseProxyProtocolHeader src seg
            ProxyProtocolOptional -> do
                seg <- readSource src
                if S.isPrefixOf "PROXY " seg
                    then parseProxyProtocolHeader src seg
                    else do leftoverSource src seg
                            return origAddr

    parseProxyProtocolHeader src seg = do
        let (header,seg') = S.break (== 0x0d) seg -- 0x0d == CR
            maybeAddr = case S.split 0x20 header of -- 0x20 == space
                ["PROXY","TCP4",clientAddr,_,clientPort,_] ->
                    case [x | (x, t) <- reads (decodeAscii clientAddr), null t] of
                        [a] -> Just (SockAddrInet (readInt clientPort)
                                                       (toHostAddress a))
                        _ -> Nothing
                ["PROXY","TCP6",clientAddr,_,clientPort,_] ->
                    case [x | (x, t) <- reads (decodeAscii clientAddr), null t] of
                        [a] -> Just (SockAddrInet6 (readInt clientPort)
                                                        0
                                                        (toHostAddress6 a)
                                                        0)
                        _ -> Nothing
                ("PROXY":"UNKNOWN":_) ->
                    Just origAddr
                _ ->
                    Nothing
        case maybeAddr of
            Nothing -> throwIO (BadProxyHeader (decodeAscii header))
            Just a -> do leftoverSource src (S.drop 2 seg') -- drop CRLF
                         return a

    decodeAscii = map (chr . fromEnum) . S.unpack

    th = threadHandle1 ii1

    shouldSendErrorResponse se
        | Just ConnectionClosedByPeer <- fromException se = False
        | otherwise                                       = True

    sendErrorResponse addr istatus e = do
        status <- readIORef istatus
        when (shouldSendErrorResponse e && status) $ do
           let ii = toInternalInfo ii1 0 -- dummy
               dreq = dummyreq addr
           void $ sendResponse settings conn ii dreq defaultIndexRequestHeader (return S.empty) (errorResponse e)

    dummyreq addr = defaultRequest { remoteHost = addr }

    errorResponse e = settingsOnExceptionResponse settings e

    http1 firstRequest addr istatus src = do
        (req', mremainingRef, idxhdr, nextBodyFlush, ii) <- recvRequest firstRequest settings conn ii1 addr src
        let req = req' { isSecure = isTransportSecure transport }
        keepAlive <- processRequest istatus src req mremainingRef idxhdr nextBodyFlush ii
            `E.catch` \e -> do
                -- Call the user-supplied exception handlers, passing the request.
                sendErrorResponse addr istatus e
                settingsOnException settings (Just req) e
                -- Don't throw the error again to prevent calling settingsOnException twice.
                return False

        -- When doing a keep-alive connection, the other side may just
        -- close the connection. We don't want to treat that as an
        -- exceptional situation, so we pass in False to http1 (which
        -- in turn passes in False to recvRequest), indicating that
        -- this is not the first request. If, when trying to read the
        -- request headers, no data is available, recvRequest will
        -- throw a NoKeepAliveRequest exception, which we catch here
        -- and ignore. See: https://github.com/yesodweb/wai/issues/618
        when keepAlive $ http1 False addr istatus src

    processRequest istatus src req mremainingRef idxhdr nextBodyFlush ii = do
        -- Let the application run for as long as it wants
        T.pause th

        -- In the event that some scarce resource was acquired during
        -- creating the request, we need to make sure that we don't get
        -- an async exception before calling the ResponseSource.
        keepAliveRef <- newIORef $ error "keepAliveRef not filled"
        _ <- app req $ \res -> do
            T.resume th
            -- FIXME consider forcing evaluation of the res here to
            -- send more meaningful error messages to the user.
            -- However, it may affect performance.
            writeIORef istatus False
            keepAlive <- sendResponse settings conn ii req idxhdr (readSource src) res
            writeIORef keepAliveRef keepAlive
            return ResponseReceived
        keepAlive <- readIORef keepAliveRef

        -- We just send a Response and it takes a time to
        -- receive a Request again. If we immediately call recv,
        -- it is likely to fail and the IO manager works.
        -- It is very costly. So, we yield to another Haskell
        -- thread hoping that the next Request will arrive
        -- when this Haskell thread will be re-scheduled.
        -- This improves performance at least when
        -- the number of cores is small.
        Conc.yield

        if not keepAlive then
            return False
          else
            -- If there is an unknown or large amount of data to still be read
            -- from the request body, simple drop this connection instead of
            -- reading it all in to satisfy a keep-alive request.
            case settingsMaximumBodyFlush settings of
                Nothing -> do
                    flushEntireBody nextBodyFlush
                    T.resume th
                    return True
                Just maxToRead -> do
                    let tryKeepAlive = do
                            -- flush the rest of the request body
                            isComplete <- flushBody nextBodyFlush maxToRead
                            if isComplete then do
                                T.resume th
                                return True
                              else
                                return False
                    case mremainingRef of
                        Just ref -> do
                            remaining <- readIORef ref
                            if remaining <= maxToRead then
                                tryKeepAlive
                              else
                                return False
                        Nothing -> tryKeepAlive

flushEntireBody :: IO ByteString -> IO ()
flushEntireBody src =
    loop
  where
    loop = do
        bs <- src
        unless (S.null bs) loop

flushBody :: IO ByteString -- ^ get next chunk
          -> Int -- ^ maximum to flush
          -> IO Bool -- ^ True == flushed the entire body, False == we didn't
flushBody src =
    loop
  where
    loop toRead = do
        bs <- src
        let toRead' = toRead - S.length bs
        case () of
            ()
                | S.null bs -> return True
                | toRead' >= 0 -> loop toRead'
                | otherwise -> return False

wrappedRecv :: Connection -> T.Handle -> IORef Bool -> Int -> IO ByteString
wrappedRecv Connection { connRecv = recv } th istatus slowlorisSize = do
    bs <- recv
    unless (S.null bs) $ do
        writeIORef istatus True
        when (S.length bs >= slowlorisSize) $ T.tickle th
    return bs

wrappedRecvN :: T.Handle -> IORef Bool -> Int -> (BufSize -> IO ByteString) -> (BufSize -> IO ByteString)
wrappedRecvN th istatus slowlorisSize readN bufsize = do
    bs <- readN bufsize
    unless (S.null bs) $ do
        writeIORef istatus True
    -- TODO: think about the slowloris protection in HTTP2: current code
    -- might open a slow-loris attack vector. Rather than timing we should
    -- consider limiting the per-client connections assuming that in HTTP2
    -- we should allow only few connections per host (real-world
    -- deployments with large NATs may be trickier).
        when (S.length bs >= slowlorisSize || bufsize <= slowlorisSize) $ T.tickle th
    return bs

-- | Set flag FileCloseOnExec flag on a socket (on Unix)
--
-- Copied from: https://github.com/mzero/plush/blob/master/src/Plush/Server/Warp.hs
--
-- @since 3.2.17
setSocketCloseOnExec :: Socket -> IO ()
#if WINDOWS
setSocketCloseOnExec _ = return ()
#else
setSocketCloseOnExec socket = F.setFileCloseOnExec $ fromIntegral $ fdSocket socket
#endif

gracefulShutdown :: Settings -> Counter -> IO ()
gracefulShutdown set counter =
    case settingsGracefulShutdownTimeout set of
        Nothing ->
            waitForZero counter
        (Just seconds) ->
            void (timeout (seconds * microsPerSecond) (waitForZero counter))
            where microsPerSecond = 1000000
