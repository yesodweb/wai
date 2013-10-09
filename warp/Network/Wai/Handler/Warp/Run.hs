{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Warp.Run where

import Control.Concurrent (threadDelay, forkIOWithUnmask)
import qualified Control.Concurrent as Conc (yield)
import Control.Exception as E
import Control.Monad (forever, when, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.Conduit
import Data.Conduit.Internal (ResumableSource (..))
import qualified Data.Conduit.List as CL
import Data.Conduit.Network (bindPort)
import Network (sClose, Socket)
import Network.Sendfile
import Network.Socket (accept, SockAddr)
import qualified Network.Socket.ByteString as Sock
import Network.Wai
import Network.Wai.Handler.Warp.Request
import Network.Wai.Handler.Warp.Response
import Network.Wai.Handler.Warp.Settings
import qualified Network.Wai.Handler.Warp.Timeout as T
import Network.Wai.Handler.Warp.Types
import Network.Wai.Handler.Warp.Recv

#if WINDOWS
import qualified Control.Concurrent.MVar as MV
import Network.Socket (withSocketsDo)
import Control.Concurrent (forkIO)
#else
import System.Posix.IO (FdOption(CloseOnExec), setFdOption)
import Network.Socket (fdSocket)
#endif

#if SENDFILEFD
import Control.Applicative
import qualified Network.Wai.Handler.Warp.FdCache as F
#endif

-- FIXME come up with good values here
bytesPerRead :: Int
bytesPerRead = 4096

-- | Default action value for 'Connection'
socketConnection :: Socket -> IO Connection
socketConnection s = do
    buf <- allocateRecvBuffer bytesPerRead
    return Connection {
        connSendMany = Sock.sendMany s
      , connSendAll = Sock.sendAll s
      , connSendFile = sendFile s
      , connClose = sClose s >> freeRecvBuffer buf
      , connRecv = receive s buf bytesPerRead
      }

sendFile :: Socket -> FilePath -> Integer -> Integer -> IO () -> [ByteString] -> Cleaner -> IO ()
#if SENDFILEFD
sendFile s path off len act hdr cleaner = case fdCacher cleaner of
    Nothing  -> sendfileWithHeader s path (PartOfFile off len) act hdr
    Just fdc -> do
        (fd, fresher) <- F.getFd fdc path
        sendfileFdWithHeader s fd (PartOfFile off len) (act>>fresher) hdr
#else
sendFile s path off len act hdr _ =
    sendfileWithHeader s path (PartOfFile off len) act hdr
#endif

#if __GLASGOW_HASKELL__ < 702
allowInterrupt :: IO ()
allowInterrupt = unblock $ return ()
#endif

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
    void . forkIO $ bracket
        (bindPort (settingsPort set) (settingsHost set))
        (const clean)
        (\s -> do
            MV.modifyMVar_ var (\_ -> return $ Just s)
            runSettingsSocket set s app)
    forever (threadDelay maxBound) `finally` clean
#else
runSettings set app =
    bracket
        (bindPort (settingsPort set) (settingsHost set))
        sClose
        (\socket -> do
            setSocketCloseOnExec socket
            runSettingsSocket set socket app)
#endif

-- | Same as 'runSettings', but uses a user-supplied socket instead of opening
-- one. This allows the user to provide, for example, Unix named socket, which
-- can be used when reverse HTTP proxying into your application.
--
-- Note that the 'settingsPort' will still be passed to 'Application's via the
-- 'serverPort' record.
runSettingsSocket :: Settings -> Socket -> Application -> IO ()
runSettingsSocket set socket app =
    runSettingsConnection set getConn app
  where
    getConn = do
        (s, sa) <- accept socket
        setSocketCloseOnExec socket
        conn <- socketConnection s
        return (conn, sa)

runSettingsConnection :: Settings -> IO (Connection, SockAddr) -> Application -> IO ()
runSettingsConnection set getConn app = runSettingsConnectionMaker set getConnMaker app
  where
    getConnMaker = do
      (conn, sa) <- getConn
      return (return conn, sa)

-- | Allows you to provide a function which will return a @Connection@. In
-- cases where creating the @Connection@ can be expensive, this allows the
-- expensive computations to be performed in a separate thread instead of the
-- main server loop.
--
-- Since 1.3.5
runSettingsConnectionMaker :: Settings -> IO (IO Connection, SockAddr) -> Application -> IO ()
runSettingsConnectionMaker set getConnMaker app = do
#if SENDFILEFD
    let duration = settingsFdCacheDuration set
    fc <- case duration of
        0 -> return Nothing
        _ -> Just <$> F.initialize (duration * 1000000)
#endif
    settingsBeforeMainLoop set

    -- Note that there is a thorough discussion of the exception safety of the
    -- following code at: https://github.com/yesodweb/wai/issues/146
    --
    -- We need to make sure of two things:
    --
    -- 1. Asynchronous exceptions are not blocked entirely in the main loop.
    --    Doing so would make it impossible to kill the Warp thread.
    --
    -- 2. Once a connection maker is received via getConnLoop, the connection
    --    is guaranteed to be closed, even in the presence of async exceptions.
    --
    -- Our approach is explained in the comments below.

    -- First mask all exceptions in the main loop. This is necessary to ensure
    -- that no async exception is throw between the call to getConnLoop and the
    -- registering of connClose.
    withTimeoutManager $ \tm -> mask_ . forever $ do
        -- Allow async exceptions before receiving the next connection maker.
        allowInterrupt

        -- getConnLoop will try to receive the next incoming request. It
        -- returns a /connection maker/, not a connection, since in some
        -- circumstances creating a working connection from a raw socket may be
        -- an expensive operation, and this expensive work should not be
        -- performed in the main event loop. An example of something expensive
        -- would be TLS negotiation.
        (mkConn, addr) <- getConnLoop

        -- Fork a new worker thread for this connection maker, and ask for a
        -- function to unmask (i.e., allow async exceptions to be thrown).
        --
        -- GHC 7.8 cannot infer the type of "void . forkIOWithUnmask"
        void $ forkIOWithUnmask $ \unmask ->
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
            bracket mkConn connClose $ \conn ->

            -- We need to register a timeout handler for this thread, and
            -- cancel that handler as soon as we exit.
            bracket (T.registerKillThread tm) T.cancel $ \th ->
#if SENDFILEFD
                let cleaner = Cleaner th fc
#else
                let cleaner = Cleaner th
#endif
                    -- We now have fully registered a connection close handler
                    -- in the case of all exceptions, so it is safe to one
                    -- again allow async exceptions.
                 in unmask .
                    -- Call the user-supplied on exception code if any
                    -- exceptions are thrown.
                    handle (onE Nothing) .

                    -- Call the user-supplied code for connection open and close events
                    bracket_ onOpen onClose $

                    -- Actually serve this connection.
                    serveConnection th set cleaner app conn addr
  where
    -- FIXME: only IOEception is caught. What about other exceptions?
    getConnLoop = getConnMaker `E.catch` \(e :: IOException) -> do
        onE Nothing (toException e)
        -- "resource exhausted (Too many open files)" may happen by accept().
        -- Wait a second hoping that resource will be available.
        threadDelay 1000000
        getConnLoop
    onE = settingsOnException set
    onOpen = settingsOnOpen set
    onClose = settingsOnClose set

    withTimeoutManager f =
        case settingsManager set of
            Nothing -> bracket
                (T.initialize $ settingsTimeout set * 1000000)
                T.stopManager
                f
            Just tm -> f tm

serveConnection :: T.Handle
                -> Settings
                -> Cleaner
                -> Application -> Connection -> SockAddr-> IO ()
serveConnection timeoutHandle settings cleaner app conn remoteHost' =
    serveConnection'' $ connSource conn th
  where
    th = threadHandle cleaner

    serveConnection'' fromClient = do
        (env, getSource) <- parseRequest conn timeoutHandle remoteHost' fromClient
        case settingsIntercept settings env of
            Nothing -> do
                -- Let the application run for as long as it wants
                liftIO $ T.pause th

                -- In the event that some scarce resource was acquired during
                -- creating the request, we need to make sure that we don't get
                -- an async exception before calling the ResponseSource.
                keepAlive <- mask $ \restore -> do
                    res <- restore $ app env
                    liftIO $ T.resume th
                    sendResponse settings cleaner env conn restore res

                -- | We just send a Response and it takes a time to
                --   receive a Request again. If we immediately call recv,
                --   it is likely to fail and the IO manager works.
                --   It is very costy. So, we yield to another Haskell
                --   thread hoping that the next Request will arraive
                --   when this Haskell thread will be re-scheduled.
                --   This improves performance at least when
                --   the number of cores is small.
                Conc.yield
                -- flush the rest of the request body
                requestBody env $$ CL.sinkNull
                ResumableSource fromClient' _ <- liftIO getSource

                when keepAlive $ serveConnection'' fromClient'
            Just intercept -> do
                liftIO $ T.pause th
                ResumableSource fromClient' _ <- liftIO getSource
                intercept fromClient' conn

connSource :: Connection -> T.Handle -> Source IO ByteString
connSource Connection { connRecv = recv } th = src
  where
    src = do
        bs <- liftIO recv
        unless (S.null bs) $ do
            when (S.length bs >= 2048) $ liftIO $ T.tickle th
            yield bs
            src

-- Copied from: https://github.com/mzero/plush/blob/master/src/Plush/Server/Warp.hs
setSocketCloseOnExec :: Socket -> IO ()
#if WINDOWS
setSocketCloseOnExec _ = return ()
#else
setSocketCloseOnExec socket =
    setFdOption (fromIntegral $ fdSocket socket) CloseOnExec True
#endif
