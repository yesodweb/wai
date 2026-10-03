{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Network.Wai.Handler.Warp.Run where

import Control.Arrow (first)
#if WINDOWS
import Control.Concurrent (forkIO)
#endif
import Control.Concurrent.STM (
    STM,
    TVar,
    atomically,
    check,
    modifyTVar',
    newTVarIO,
    readTVar,
    retry,
#if WINDOWS
    newEmptyTMVarIO,
    putTMVar,
    takeTMVar,
    throwSTM,
#endif
 )
import qualified Control.Exception as E
import qualified Data.ByteString as S
import Data.Functor (($>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Streaming.Network (bindPortTCP)
import Foreign.C.Error (
    Errno (..),
    eBADF,
    eCONNABORTED,
    eHOSTDOWN,
    eHOSTUNREACH,
    eMFILE,
    eNETDOWN,
    eNETUNREACH,
    eNONET,
    eNOPROTOOPT,
    ePROTO,
 )
import GHC.Conc.Sync (labelThread, myThreadId)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Network.Socket (
    SockAddr,
    Socket,
    SocketOption (..),
    close,
#if !WINDOWS
    fdSocket,
#if MIN_VERSION_network(3,2,2)
    waitReadSocketSTM,
#endif
#endif
    getSocketName,
    setSocketOption,
    withSocketsDo,
 )
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

import Network.Wai.Handler.Warp.Buffer (createWriteBuffer)
import Network.Wai.Handler.Warp.Counter
import qualified Network.Wai.Handler.Warp.Date as D
import qualified Network.Wai.Handler.Warp.FdCache as F
import qualified Network.Wai.Handler.Warp.FileInfoCache as I
import Network.Wai.Handler.Warp.HTTP1 (http1)
import Network.Wai.Handler.Warp.HTTP2 (http2)
import Network.Wai.Handler.Warp.HTTP2.Types (isHTTP2)
import Network.Wai.Handler.Warp.Imports hiding (readInt)
import Network.Wai.Handler.Warp.SendFile (sendFile)
import Network.Wai.Handler.Warp.Settings
import Network.Wai.Handler.Warp.ShuttingDown (readShuttingDown, writeShuttingDown)
import Network.Wai.Handler.Warp.Types
import System.Watchdog

-- | Creating 'Connection' for plain HTTP based on a given socket.
--
-- (N.B. make sure the 'Settings' have an initialized 'ServerState' to guarantee
-- a graceful shutdown)
socketConnection :: Settings -> Socket -> IO Connection
socketConnection set s = do
    (ss, _) <- makeServerState set
    bufferPool <- newBufferPool 2048 16384
    writeBuffer <- createWriteBuffer 16384
    writeBufferRef <- newIORef writeBuffer
    isH2 <- newIORef False -- HTTP/1.x
    mysa <- getSocketName s
    appsInProgress <- newTVarIO 0
    wd <- newWatchdog $ settingsTimeout set * 1000000
    return
        Connection
            { connSendMany = Sock.sendMany s
            , connSendAll = sendall wd
            , connSendFile = sendfile wd writeBufferRef
#if MIN_VERSION_network(3,1,1)
            , connClose = do
                h2 <- readIORef isH2
                let tm =
                        if h2
                            then settingsGracefulCloseTimeout2 set
                            else settingsGracefulCloseTimeout1 set
                if tm <= 0
                    then close s
                    else gracefulClose s tm `E.catch` throughAsync (return ())
#else
            , connClose = close s
#endif
            , connRecv = receive' bufferPool ss appsInProgress wd
            , connRecvBuf = \_ _ -> return True -- obsoleted
            , connWriteBuffer = writeBufferRef
            , connHTTP2 = isH2
            , connMySockAddr = mysa
            , connAppsInProgress = appsInProgress
            , connWatchdog = wd
            }
  where
    receive' bufferPool ss appsInProgress wd =
        E.handle handler $ makeWatchedRecv s bufferPool ss appsInProgress wd
      where
        handler :: E.IOException -> IO ByteString
        handler e
            | ioeGetErrorType e == InvalidArgument = return ""
            | otherwise = E.throwIO e

    sendfile wd writeBufferRef fid offset len hook headers = do
        writeBuffer <- readIORef writeBufferRef
        sendFile
            s
            writeBuffer
            (sendall wd)
            fid
            offset
            len
            hook
            headers

    sendall wd bs =
        E.handleJust
            ( \e ->
                if ioeGetErrorType e == ResourceVanished
                    then Just ConnectionClosedByPeer
                    else Nothing
            )
            E.throwIO
            $ send' wd bs

#if WINDOWS
    -- As with the read: a send that blocks on WINDOWS blocks in a foreign
    -- call, where neither the watchdog's 'TVar' nor an asynchronous
    -- exception can reach this thread, so a peer that stops reading is a
    -- connection that cannot be given up on.  The send is done on a thread
    -- of its own and this one waits for it alongside the watchdog.
    --
    -- The send left behind is left to the socket being closed, which is what
    -- ends this connection anyway.
    send' wd bs = do
        var <- newEmptyTMVarIO
        void $ forkIO $ do
            r <- E.try $ Sock.sendAll s bs
            atomically $ putTMVar var (r :: Either E.SomeException ())
        done <- atomically $
            (timedOutSTM wd $> Nothing) <|> (Just <$> takeTMVar var)
        case done of
            Nothing -> E.throwIO T.TimeoutThread
            Just r -> either E.throwIO return r
#else
    send' _wd bs = Sock.sendAll s bs
#endif

-- | Create a 'Recv' using 'Network.Socket.BufferPool.Recv.receive', but make
-- it non-blocking with 'waitReadSocketSTM' /AND/ cut off receiving any bytes
-- when the server is shutting down and there are no more 'Application's
-- actively using this 'Socket'.
makeGracefulRecv :: Socket -> BufferPool -> ServerState -> TVar Int -> Recv
makeGracefulRecv sock pool ss appsInProgress =
    makeGracefulRecvWith sock pool ss appsInProgress retry

-- | 'makeGracefulRecv' which also gives up when the 'Watchdog' decides
-- that the connection timed out, by throwing 'T.TimeoutThread'.
--
-- The timeout is composed with the socket readiness in a single STM
-- transaction, so a receiving thread is never killed from outside
-- while it waits for the peer.
--
-- @since 3.5.0
makeWatchedRecv
    :: Socket -> BufferPool -> ServerState -> TVar Int -> Watchdog -> Recv
makeWatchedRecv sock pool ss appsInProgress wd = do
    timedOut <- isTimedOut wd
    when timedOut $ E.throwIO T.TimeoutThread
    makeGracefulRecvWith sock pool ss appsInProgress (timedOutSTM wd)

makeGracefulRecvWith
    :: Socket -> BufferPool -> ServerState -> TVar Int -> STM () -> Recv
makeGracefulRecvWith sock pool ss appsInProgress timedOut = do
    tryFastPath <- not <$> readShuttingDown (serverShuttingDown ss)
    if tryFastPath then do
        mbs <- receiveNoWait sock pool
        case mbs of
          Just bs -> return bs
          Nothing -> slowPath
      else slowPath
  where
    slowPath = makeGracefulRecvSlow sock pool ss appsInProgress timedOut

data RecvEvent = ShuttingDown | TimedOut | Readable | Received ByteString

makeGracefulRecvSlow
    :: Socket -> BufferPool -> ServerState -> TVar Int -> STM () -> Recv
makeGracefulRecvSlow sock pool ss appsInProgress timedOut = do
    waitRecv <- waitForSomethingToRead
    ev <- atomically $
        -- when shutting down
        (checkShutdown $> ShuttingDown)
        <|>
        -- when the watchdog gave up on this connection
        (timedOut $> TimedOut)
        <|>
        -- else wait for the socket, or for the read done on our behalf
        waitRecv
    case ev of
        ShuttingDown -> pure ""
        TimedOut -> E.throwIO T.TimeoutThread
        Readable -> recv
        Received bs -> pure bs
  where
    recv = receive sock pool

#if !WINDOWS && MIN_VERSION_network(3,2,2)
    -- The socket can be waited on, so the read is left where it is and this
    -- thread waits for it alongside everything else.
    waitForSomethingToRead = ($> Readable) <$> waitReadSocketSTM sock
#else
    -- 'waitReadSocketSTM' does not work on WINDOWS, and a read that cannot
    -- be waited on is a read that blocks in a foreign call -- where neither
    -- the watchdog's 'TVar' nor an asynchronous exception can reach this
    -- thread, so a connection that has to be given up on cannot be.  The
    -- read is done on a thread of its own instead and hands back what it
    -- read, which is what 'windowsThreadBlockHack' already does for accept.
    --
    -- A read this thread has stopped waiting for is a read whose connection
    -- is over, so the buffer it fills is nobody's by then.
    waitForSomethingToRead = do
        var <- newEmptyTMVarIO
        void $ forkIO $ do
            r <- E.try recv
            atomically $ putTMVar var (r :: Either E.SomeException ByteString)
        return $ takeTMVar var >>= either throwSTM (return . Received)
#endif
    checkShutdown = do
       check =<< currentShuttingDownStateSTM ss
       check . (<= 0) =<< readTVar appsInProgress

-- | Run an 'Application' on the given port.
-- This calls 'runSettings' with 'defaultSettings'.
run :: Port -> Application -> IO ()
run p = runSettings defaultSettings{settingsPort = p}

-- | Run an 'Application' on the port present in the @PORT@
-- environment variable. Uses the 'Port' given when the variable is unset.
-- This calls 'runSettings' with 'defaultSettings'.
--
-- @since 3.0.9
runEnv :: Port -> Application -> IO ()
runEnv p app = do
    mp <- lookupEnv "PORT"

    maybe (run p app) runReadPort mp
  where
    runReadPort :: String -> IO ()
    runReadPort sp = case reads sp of
        ((p', _) : _) -> run p' app
        _ -> fail $ "Invalid value in $PORT: " ++ sp

-- | Run an 'Application' with the given 'Settings'.
-- This opens a listen socket on the port defined in 'Settings' and
-- calls 'runSettingsSocket'.
runSettings :: Settings -> Application -> IO ()
runSettings set app =
    withSocketsDo $
        E.bracket
            (bindPortTCP (settingsPort set) (settingsHost set))
            close
            ( \socket -> do
                setSocketCloseOnExec socket
                runSettingsSocket set socket app
            )

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
runSettingsSocket oldSettings@Settings{settingsAccept = accept'} socket app = do
    settingsInstallShutdownHandler oldSettings closeListenSocket
    (_, newSettings) <- makeServerState oldSettings
    runSettingsConnection newSettings (getConn newSettings) app
  where
    getConn set = do
        (s, sa) <- accept' socket
        setSocketCloseOnExec s
        -- NoDelay causes an error for AF_UNIX.
        setSocketOption s NoDelay 1 `E.catch` throughAsync (return ())
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
-- @since 1.3.5
runSettingsConnection
    :: Settings -> IO (Connection, SockAddr) -> Application -> IO ()
runSettingsConnection set getConn app = runSettingsConnectionMaker set getConnMaker app
  where
    getConnMaker = do
        (conn, sa) <- getConn
        return (return conn, sa)

-- | This modifies the connection maker so that it returns 'TCP' for 'Transport'
-- (i.e. plain HTTP) then calls 'runSettingsConnectionMakerSecure'.
runSettingsConnectionMaker
    :: Settings -> IO (IO Connection, SockAddr) -> Application -> IO ()
runSettingsConnectionMaker x y =
    runSettingsConnectionMakerSecure x (toTCP <$> y)
  where
    toTCP = first ((,TCP) <$>)

----------------------------------------------------------------

-- | The core run function which takes 'Settings',
-- a connection maker and 'Application'.
-- The connection maker can return a connection of either plain HTTP
-- or HTTP over TLS.
--
-- @since 2.1.4
runSettingsConnectionMakerSecure
    :: Settings -> IO (IO (Connection, Transport), SockAddr) -> Application -> IO ()
runSettingsConnectionMakerSecure oldSettings getConnMaker app = do
    settingsBeforeMainLoop oldSettings
    (ServerState{serverConnectionCounter}, newSettings) <- makeServerState oldSettings
    withII newSettings $ \ii ->
        initFdExhaustionRef >>=
            acceptConnection newSettings getConnMaker app serverConnectionCounter ii

-- | Running an action with internal info.
--
-- @since 3.3.11
withII :: Settings -> (InternalInfo -> IO a) -> IO a
withII set action =
    withTimeoutManager $ \tm ->
        D.withDateCache $ \dc ->
            F.withFdCache fdCacheDurationInMicroseconds $ \fdc ->
                I.withFileInfoCache fdFileInfoDurationInMicroseconds $ \fic -> do
                    let ii = InternalInfo tm dc fdc fic
                    action ii
  where
    !fdCacheDurationInMicroseconds = settingsFdCacheDuration set * 1000000
    !fdFileInfoDurationInMicroseconds = settingsFileInfoCacheDuration set * 1000000
    !timeoutInMicroseconds = settingsTimeout set * 1000000
    withTimeoutManager f = case settingsManager set of
        Just tm -> f tm
        Nothing ->
            E.bracket
                (T.initialize timeoutInMicroseconds)
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
acceptConnection
    :: Settings
    -> IO (IO (Connection, Transport), SockAddr)
    -> Application
    -> Counter
    -> InternalInfo
    -> IORef FdExhaustion
        -- ^ This ref will be used to "debounce" the call to 'settingsOnException'
        -- when we hit an 'IOError' with 'eMFILE' in the case that Warp is not
        -- the reason the file descriptors are exhausted.
    -> IO ()
acceptConnection set getConnMaker app counter ii fdRef = do
    -- First mask all exceptions in acceptLoop. This is necessary to
    -- ensure that no async exception is throw between the call to
    -- acceptNewConnection and the registering of connClose.
    --
    -- acceptLoop can be broken by closing the listening socket.
    void $ E.mask_ acceptLoop
    -- In some cases, we want to stop Warp here without graceful shutdown.
    -- So, async exceptions are allowed here.
    -- That's why `finally` is not used.
    gracefulShutdown set counter
  where
    acceptLoop = do
        -- Allow async exceptions before receiving the next connection maker.
        E.allowInterrupt

        -- acceptNewConnection will try to receive the next incoming
        -- request. It returns a /connection maker/, not a connection,
        -- since in some circumstances creating a working connection
        -- from a raw socket may be an expensive operation, and this
        -- expensive work should not be performed in the main event
        -- loop. An example of something expensive would be TLS
        -- negotiation.
        mx <- acceptNewConnection
        case mx of
            Nothing -> return ()
            Just (mkConn, addr) -> do
                fork set mkConn addr app counter ii
                acceptLoop

    acceptNewConnection = do
        ex <- E.try getConnMaker
        case ex of
            Right x -> do
                -- Important to mark the exhaustion issue to be resolved
                -- when we get connections again.
                resetFdExhaustion fdRef
                return $ Just x
            Left e -> do
                let getErrno (Errno cInt) = cInt
                    isErrno err = ioe_errno e == Just (getErrno err)
                    -- Errors about one queued connection rather than about the
                    -- listening socket. eCONNABORTED is the familiar one, a
                    -- peer that went away before it could be accepted. Linux
                    -- also reports the new socket's already-pending network
                    -- errors through accept(), and accept(2) asks for those to
                    -- be treated the same way, retried like EAGAIN. Either
                    -- way the connection has left the queue, so the retry
                    -- blocks for a new one rather than spinning on the same
                    -- failure.
                    --
                    -- eOPNOTSUPP is on that list in accept(2) and is left off
                    -- this one on purpose: it also means the listening socket
                    -- is not SOCK_STREAM, which is a permanent condition that
                    -- retrying would spin on forever. Throwing tells whoever
                    -- passed that socket, which is the only thing that helps.
                    isQueuedConnectionError =
                        any
                            isErrno
                            [ eCONNABORTED
                            , eNETDOWN
                            , ePROTO
                            , eNOPROTOOPT
                            , eHOSTDOWN
                            , eNONET
                            , eHOSTUNREACH
                            , eNETUNREACH
                            ]
                    isFdExhaustion = isErrno eMFILE
                    isIntentionallyClosedSocket = isErrno eBADF
                if | isQueuedConnectionError -> do
                        -- Important to mark the exhaustion issue to be resolved
                        resetFdExhaustion fdRef
                        acceptNewConnection
                     -- Keep in mind to reset the ref when anything other
                     -- than this branch runs
                   | isFdExhaustion -> do
                        handleFdExhaustion e
                        acceptNewConnection
                     -- A graceful shutdown ends this loop by closing the
                     -- listening socket, and that always arrives here as
                     -- EBADF: 'close' replaces the descriptor with -1 before
                     -- closing it, so every later accept() is handed -1 and
                     -- fails that way. Matching EBADF alone therefore cannot
                     -- miss a deliberate shutdown.
                   | isIntentionallyClosedSocket -> do
                        resetFdExhaustion fdRef
                        settingsOnException set Nothing $ E.toException e
                        return Nothing
                     -- Everything left is something the listening socket will
                     -- keep giving: descriptors exhausted system-wide, no
                     -- memory for a socket, a socket that cannot accept.
                     -- Ending the loop for those would return the same () a
                     -- graceful shutdown returns, so a server that died and a
                     -- server that was asked to stop would be reported
                     -- identically and the caller could not tell which had
                     -- happened. Throw instead, so it can.
                   | otherwise -> do
                        -- Maybe not important to mark the exhaustion issue
                        -- as resolved here, but just for completeness' sake.
                        resetFdExhaustion fdRef
                        settingsOnException set Nothing $ E.toException e
#if WINDOWS
                        -- None of the guards above can match on Windows, where
                        -- network reports a socket error with no errno on it,
                        -- so every accept() failure arrives here including the
                        -- EBADF of a deliberate shutdown. Throwing would turn
                        -- an ordinary shutdown into an exception, so Windows
                        -- keeps ending the loop the way it always has.
                        return Nothing
#else
                        E.throwIO e
#endif

    handleFdExhaustion e = do
        fdExhaustion <- readIORef fdRef
        -- If file descriptors are exhausted while Warp has
        -- no current connections, 'settingsOnException' would
        -- get called an enormous amount of times per second.
        when (fdExhaustion /= FdExhausted) $
            settingsOnException set Nothing $ E.toException e
        hasDecreased <- waitForDecreased counter
        -- If we get 'NoConnections', that means the file
        -- descriptor exhaustion is outside of our control.
        -- We flag it so that 'settingsOnException' doesn't get
        -- called until the exhaustion issue is resolved.
        when (hasDecreased == NoConnections) $ setFdExhaustion fdRef

-- Fork a new worker thread for this connection maker, and ask for a
-- function to unmask (i.e., allow async exceptions to be thrown).
fork
    :: Settings
    -> IO (Connection, Transport)
    -> SockAddr
    -> Application
    -> Counter
    -> InternalInfo
    -> IO ()
fork set mkConn addr app counter ii = do
    -- Count the connection here rather than in the thread below.  The
    -- accept loop does not wait for that thread to be scheduled, so
    -- counting there leaves a window in which the connection is accepted
    -- and not counted, and 'gracefulShutdown' waits on this counter.
    increase counter
    settingsFork set $ \unmask -> runConnection unmask `E.finally` decrease counter
  where
    runConnection unmask = do
        tid <- myThreadId
        labelThread tid "Warp just forked"
        -- Call the user-supplied on exception code if any
        -- exceptions are thrown.
        --
        -- Intentionally using Control.Exception.handle, since we want to
        -- catch all exceptions and avoid them from propagating, even
        -- async exceptions. See:
        -- https://github.com/yesodweb/wai/issues/850
        E.handle (onConnectionException set addr) $
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
            E.bracket mkConn cleanUp (serve unmask)

    cleanUp (conn, _) =
        connClose conn `E.finally` do
            writeBuffer <- readIORef $ connWriteBuffer conn
            bufFree writeBuffer

    -- Supervise this connection with its watchdog, for both HTTP/1.1
    -- and HTTP/2, and stop the watchdog as soon as we exit. The http2
    -- library records into the same watchdog. Writes are wrapped here so
    -- that every protocol reports them in the same way. The time handle
    -- is a dummy, kept for the signatures only.
    --
    -- 'T.TimeoutThread', whether thrown by 'connRecv' or as the last
    -- resort, does not escape.
    serve unmask (conn0, transport) = E.handle ignoreTimeout $ do
      tid <- myThreadId
      withWatchdog wd (E.throwTo tid T.TimeoutThread) $ do
        let conn = watchSend conn0
            th = T.emptyHandle
        -- We now have fully registered a connection close handler in
        -- the case of all exceptions, so it is safe to once again
        -- allow async exceptions.
        unmask
            .
            -- Call the user-supplied code for connection open and
            -- close events
            E.bracket (onOpen addr) (onClose addr)
            $ \goingon ->
                -- Actually serve this connection.  bracket with closeConn
                -- above ensures the connection is closed.
                when goingon $ serveConnection conn ii th addr transport set app
      where
        wd = connWatchdog conn0
        ignoreTimeout T.TimeoutThread = return ()

    onOpen adr = settingsOnOpen set adr
    onClose adr _ = settingsOnClose set adr

serveConnection
    :: Connection
    -> InternalInfo
    -> T.Handle
    -> SockAddr
    -> Transport
    -> Settings
    -> Application
    -> IO ()
serveConnection conn ii th origAddr transport settings app = do
    -- fixme: Upgrading to HTTP/2 should be supported.
    tid <- myThreadId
    (h2, bs) <-
        if isHTTP2 transport
            then return (True, "")
            else do
                bs0 <- recv4 ""
                if "PRI " `S.isPrefixOf` bs0
                    then return (True, bs0)
                    else return (False, bs0)
    let appsInProgress = connAppsInProgress conn
        app' req rsp =
            E.bracket_
                (atomically $ modifyTVar' appsInProgress $ (+ 1))
                (atomically $ modifyTVar' appsInProgress $ \i -> (i - 1))
                $ app req rsp
    if settingsHTTP2Enabled settings && h2
        then do
            labelThread tid ("Warp HTTP/2 " ++ show origAddr)
            http2 settings ii conn transport app' origAddr th bs
        else do
            labelThread tid ("Warp HTTP/1.1 " ++ show origAddr)
            -- For HTTP/2, the http2 library tells the watchdog that
            -- applications run.
            let app'' req rsp = runningApp (connWatchdog conn) $ app' req rsp
            http1 settings ii conn transport app'' origAddr th bs
  where
    recv4 bs0 = do
        bs1 <- connRecv conn
        if S.null bs1 then
            return bs0
          else do
            -- In the case where bs0 is "", (<>) is called unnecessarily.
            -- But we adopt this logic for simplicity.
            let bs2 = bs0 <> bs1
            if S.length bs2 >= 4
                 then return bs2
                 else recv4 bs2

-- | Reporting writes to the 'Watchdog' of the connection.
watchSend :: Connection -> Connection
watchSend conn =
    conn
        { connSendAll = sending wd . connSendAll conn
        , connSendMany = sending wd . connSendMany conn
        , connSendFile = \fid off len hook hdrs ->
            sending wd $ connSendFile conn fid off len (hook >> txTick wd) hdrs
        }
  where
    wd = connWatchdog conn

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
gracefulShutdown set counter = do
    setShuttingDown
    case settingsGracefulShutdownTimeout set of
        Nothing ->
            waitForZero counter
        (Just seconds) ->
            void (timeout (seconds * microsPerSecond) (waitForZero counter))
  where
    microsPerSecond = 1000000
    setShuttingDown =
        case settingsServerState set of
            Nothing -> pure ()
            Just ServerState{serverShuttingDown} ->
                writeShuttingDown serverShuttingDown True

data FdExhaustion = NoFdIssue | FdExhausted
    deriving (Eq, Show)

initFdExhaustionRef :: IO (IORef FdExhaustion)
initFdExhaustionRef = newIORef NoFdIssue

-- [FD_EXHAUSTION]
-- No need for "atomic" variants, since this is only used in a tight loop in
-- 'acceptConnection'.
resetFdExhaustion :: IORef FdExhaustion -> IO ()
resetFdExhaustion = flip writeIORef NoFdIssue

setFdExhaustion :: IORef FdExhaustion -> IO ()
setFdExhaustion = flip writeIORef FdExhausted -- [FD_EXHAUSTION]
