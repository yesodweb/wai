{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Network.Wai.Handler.Warp.Run where

import Control.Arrow (first)
import qualified Control.Exception
import Control.Exception (allowInterrupt)
import qualified Data.ByteString as S
import Data.IORef (newIORef, readIORef)
import Data.Streaming.Network (bindPortTCP)
import Foreign.C.Error (Errno(..), eCONNABORTED, eMFILE)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import Network.Socket (Socket, close, withSocketsDo, SockAddr, setSocketOption, SocketOption(..))
#if MIN_VERSION_network(3,1,1)
import Network.Socket (gracefulClose)
#endif
import Network.Socket.BufferPool
import qualified Network.Socket.ByteString as Sock
import Network.Wai
import System.Environment (lookupEnv)
import System.IO.Error (ioeGetErrorType)
import qualified System.TimeManager as T
import System.Timeout (timeout)
import qualified UnliftIO
import UnliftIO (toException)

import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Counter
import qualified Network.Wai.Handler.Warp.Date as D
import qualified Network.Wai.Handler.Warp.FdCache as F
import qualified Network.Wai.Handler.Warp.FileInfoCache as I
import Network.Wai.Handler.Warp.HTTP1 (http1)
import Network.Wai.Handler.Warp.HTTP2 (http2)
import Network.Wai.Handler.Warp.HTTP2.Types (isHTTP2)
import Network.Wai.Handler.Warp.Imports hiding (readInt)
import Network.Wai.Handler.Warp.SendFile
import Network.Wai.Handler.Warp.Settings
import Network.Wai.Handler.Warp.Types


#if WINDOWS
import Network.Wai.Handler.Warp.Windows
#else
import Network.Socket (fdSocket)
#endif

-- | Creating 'Connection' for plain HTTP based on a given socket.
socketConnection :: Settings -> Socket -> IO Connection
#if MIN_VERSION_network(3,1,1)
socketConnection set s = do
#else
socketConnection _ s = do
#endif
    bufferPool <- newBufferPool 2048 16384
    writeBuffer <- createWriteBuffer 16384
    writeBufferRef <- newIORef writeBuffer
    isH2 <- newIORef False -- HTTP/1.x
    return Connection {
        connSendMany = Sock.sendMany s
      , connSendAll = sendall
      , connSendFile = sendfile writeBufferRef
#if MIN_VERSION_network(3,1,1)
      , connClose = do
            h2 <- readIORef isH2
            let tm = if h2 then settingsGracefulCloseTimeout2 set
                           else settingsGracefulCloseTimeout1 set
            if tm == 0 then
                close s
              else
                gracefulClose s tm `UnliftIO.catchAny` \(UnliftIO.SomeException _) -> return ()
#else
      , connClose = close s
#endif
      , connRecv = receive' s bufferPool
      , connRecvBuf = receiveBuf s
      , connWriteBuffer = writeBufferRef
      , connHTTP2 = isH2
      }
  where
    receive' sock pool = UnliftIO.handleIO handler $ receive sock pool
      where
        handler :: UnliftIO.IOException -> IO ByteString
        handler e
          | ioeGetErrorType e == InvalidArgument = return ""
          | otherwise                            = UnliftIO.throwIO e

    sendfile writeBufferRef fid offset len hook headers = do
      writeBuffer <- readIORef writeBufferRef
      sendFile s (bufBuffer writeBuffer) (bufSize writeBuffer) sendall
        fid offset len hook headers

    sendall = sendAll' s

    sendAll' sock bs = UnliftIO.handleJust
      (\ e -> if ioeGetErrorType e == ResourceVanished
        then Just ConnectionClosedByPeer
        else Nothing)
      UnliftIO.throwIO
      $ Sock.sendAll sock bs

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
    mp <- lookupEnv "PORT"

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
    UnliftIO.bracket
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
runSettingsSocket set@Settings{settingsAccept = accept'} socket app = do
    settingsInstallShutdownHandler set closeListenSocket
    runSettingsConnection set getConn app
  where
    getConn = do
        (s, sa) <- accept' socket
        setSocketCloseOnExec s
        -- NoDelay causes an error for AF_UNIX.
        setSocketOption s NoDelay 1 `UnliftIO.catchAny` \(UnliftIO.SomeException _) -> return ()
        conn <- socketConnection set s
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
    withII set $ acceptConnection set getConnMaker app counter

-- | Running an action with internal info.
--
-- Since 3.3.11
withII :: Settings -> (InternalInfo -> IO a) -> IO a
withII set action =
    withTimeoutManager $ \tm ->
    D.withDateCache $ \dc ->
    F.withFdCache fdCacheDurationInSeconds $ \fdc ->
    I.withFileInfoCache fdFileInfoDurationInSeconds $ \fic -> do
        let ii = InternalInfo tm dc fdc fic
        action ii
  where
    !fdCacheDurationInSeconds = settingsFdCacheDuration set * 1000000
    !fdFileInfoDurationInSeconds = settingsFileInfoCacheDuration set * 1000000
    !timeoutInSeconds = settingsTimeout set * 1000000
    withTimeoutManager f = case settingsManager set of
        Just tm -> f tm
        Nothing -> UnliftIO.bracket
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
                 -> InternalInfo
                 -> IO ()
acceptConnection set getConnMaker app counter ii = do
    -- First mask all exceptions in acceptLoop. This is necessary to
    -- ensure that no async exception is throw between the call to
    -- acceptNewConnection and the registering of connClose.
    --
    -- acceptLoop can be broken by closing the listening socket.
    void $ UnliftIO.mask_ acceptLoop
    -- In some cases, we want to stop Warp here without graceful shutdown.
    -- So, async exceptions are allowed here.
    -- That's why `finally` is not used.
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
                fork set mkConn addr app counter ii
                acceptLoop

    acceptNewConnection = do
        ex <- UnliftIO.tryIO getConnMaker
        case ex of
            Right x -> return $ Just x
            Left e -> do
                let getErrno (Errno cInt) = cInt
                    eConnAborted = getErrno eCONNABORTED
                    eMfile = getErrno eMFILE
                    merrno = ioe_errno e
                if merrno == Just eConnAborted || merrno == Just eMfile
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
     -> InternalInfo
     -> IO ()
fork set mkConn addr app counter ii = settingsFork set $ \unmask ->
    -- Call the user-supplied on exception code if any
    -- exceptions are thrown.
    --
    -- Intentionally using Control.Exception.handle, since we want to
    -- catch all exceptions and avoid them from propagating, even
    -- async exceptions. See:
    -- https://github.com/yesodweb/wai/issues/850
    Control.Exception.handle (settingsOnException set Nothing) $
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
        UnliftIO.bracket mkConn cleanUp (serve unmask)
  where
    cleanUp (conn, _) = connClose conn `UnliftIO.finally` do
                          writeBuffer <- readIORef $ connWriteBuffer conn
                          bufFree writeBuffer

    -- We need to register a timeout handler for this thread, and
    -- cancel that handler as soon as we exit.
    serve unmask (conn, transport) = UnliftIO.bracket register cancel $ \th -> do
        -- We now have fully registered a connection close handler in
        -- the case of all exceptions, so it is safe to once again
        -- allow async exceptions.
        unmask .
            -- Call the user-supplied code for connection open and
            -- close events
           UnliftIO.bracket (onOpen addr) (onClose addr) $ \goingon ->
           -- Actually serve this connection.  bracket with closeConn
           -- above ensures the connection is closed.
           when goingon $ serveConnection conn ii th addr transport set app
      where
        register = T.registerKillThread (timeoutManager ii) (connClose conn)
        cancel   = T.cancel

    onOpen adr    = increase counter >> settingsOnOpen  set adr
    onClose adr _ = decrease counter >> settingsOnClose set adr

serveConnection :: Connection
                -> InternalInfo
                -> T.Handle
                -> SockAddr
                -> Transport
                -> Settings
                -> Application
                -> IO ()
serveConnection conn ii th origAddr transport settings app = do
    -- fixme: Upgrading to HTTP/2 should be supported.
    (h2,bs) <- if isHTTP2 transport then
                   return (True, "")
                 else do
                   bs0 <- connRecv conn
                   if S.length bs0 >= 4 && "PRI " `S.isPrefixOf` bs0 then
                       return (True, bs0)
                     else
                       return (False, bs0)
    if settingsHTTP2Enabled settings && h2 then do
        http2 settings ii conn transport app origAddr th bs
      else do
        http1 settings ii conn transport app origAddr th bs

-- | Set flag FileCloseOnExec flag on a socket (on Unix)
--
-- Copied from: https://github.com/mzero/plush/blob/master/src/Plush/Server/Warp.hs
--
-- @since 3.2.17
setSocketCloseOnExec :: Socket -> IO ()
#if WINDOWS
setSocketCloseOnExec _ = return ()
#else
setSocketCloseOnExec socket = do
#if MIN_VERSION_network(3,0,0)
    fd <- fdSocket socket
#else
    let fd = fdSocket socket
#endif
    F.setFileCloseOnExec $ fromIntegral fd
#endif

gracefulShutdown :: Settings -> Counter -> IO ()
gracefulShutdown set counter =
    case settingsGracefulShutdownTimeout set of
        Nothing ->
            waitForZero counter
        (Just seconds) ->
            void (timeout (seconds * microsPerSecond) (waitForZero counter))
            where microsPerSecond = 1000000
