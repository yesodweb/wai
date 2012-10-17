{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Warp.Run where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Control.Monad (forever, when, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
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
import Prelude hiding (catch)

-- Sock.recv first tries to call recvfrom() optimistically.
-- If EAGAIN returns, it polls incoming data with epoll/kqueue.
-- This code first polls incoming data with epoll/kqueue.
#if !WINDOWS
#define PESSIMISTIC_RECV 1
#endif

#ifdef PESSIMISTIC_RECV
import System.Posix.Types (Fd(..))
import Control.Concurrent (threadWaitRead)
import Network.Socket (Socket(..))
#endif

#if WINDOWS
import qualified Control.Concurrent.MVar as MV
import Network.Socket (withSocketsDo)
#endif

#if SENDFILEFD
import Control.Applicative
import qualified Network.Wai.Handler.Warp.FdCache as F
#endif

-- FIXME come up with good values here
bytesPerRead :: Int
bytesPerRead = 4096

-- | Default action value for 'Connection'
socketConnection :: Socket -> Connection
#ifdef PESSIMISTIC_RECV
socketConnection s@(MkSocket fd _ _ _ _) = Connection
#else
socketConnection s = Connection
#endif
    { connSendMany = Sock.sendMany s
    , connSendAll = Sock.sendAll s
    , connSendFile = sendFile s
    , connClose = sClose s
#ifdef PESSIMISTIC_RECV
    , connRecv = threadWaitRead (Fd fd) >> Sock.recv s bytesPerRead
#else
    , connRecv = Sock.recv s bytesPerRead
#endif
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
runSettings set =
    bracket
        (bindPort (settingsPort set) (settingsHost set))
        sClose .
        flip (runSettingsSocket set)
#endif

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
    tm <- maybe (T.initialize $ settingsTimeout set * 1000000) return
        $ settingsManager set
#if SENDFILEFD
    let duration = settingsFdCacheDuration set
    fc <- case duration of
        0 -> return Nothing
        _ -> Just <$> F.initialize (duration * 1000000)
#endif
    mask $ \restore -> forever $ do
        allowInterrupt
        (conn, addr) <- getConnLoop
        void . forkIO $ do
            th <- T.registerKillThread tm
#if SENDFILEFD
            let cleaner = Cleaner th fc
#else
            let cleaner = Cleaner th
#endif
            let serve = do
                    onOpen
                    restore $ serveConnection set cleaner port app conn addr
                    cleanup
                cleanup = connClose conn >> T.cancel th >> onClose
            handle onE (serve `onException` cleanup)
  where
    -- FIXME: only IOEception is caught. What about other exceptions?
    getConnLoop = getConn `catch` \(e :: IOException) -> do
        onE (toException e)
        -- "resource exhausted (Too many open files)" may happen by accept().
        -- Wait a second hoping that resource will be available.
        threadDelay 1000000
        getConnLoop
    onE = settingsOnException set
    port = settingsPort set
    onOpen = settingsOnOpen set
    onClose = settingsOnClose set

serveConnection :: Settings
                -> Cleaner
                -> Port -> Application -> Connection -> SockAddr-> IO ()
serveConnection settings cleaner port app conn remoteHost' =
    runResourceT serveConnection'
  where
    innerRunResourceT
        | settingsResourceTPerRequest settings = lift . runResourceT
        | otherwise = id
    th = threadHandle cleaner

    serveConnection' :: ResourceT IO ()
    serveConnection' = serveConnection'' $ connSource conn th

    serveConnection'' fromClient = do
        (env, getSource) <- parseRequest conn port remoteHost' fromClient
        case settingsIntercept settings env of
            Nothing -> do
                -- Let the application run for as long as it wants
                liftIO $ T.pause th
                keepAlive <- innerRunResourceT $ do
                    res <- app env

                    liftIO $ T.resume th
                    sendResponse cleaner env conn res

                -- flush the rest of the request body
                requestBody env $$ CL.sinkNull
                ResumableSource fromClient' _ <- liftIO getSource

                when keepAlive $ serveConnection'' fromClient'
            Just intercept -> do
                liftIO $ T.pause th
                ResumableSource fromClient' _ <- liftIO getSource
                intercept fromClient' conn

connSource :: Connection -> T.Handle -> Source (ResourceT IO) ByteString
connSource Connection { connRecv = recv } th = src
  where
    src = do
        bs <- liftIO recv
        unless (S.null bs) $ do
            when (S.length bs >= 2048) $ liftIO $ T.tickle th
            yield bs
            src
