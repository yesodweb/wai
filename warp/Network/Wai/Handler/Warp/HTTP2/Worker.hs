{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.HTTP2.Worker (
    Responder
  , response
  , worker
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException(..), AsyncException(..))
import qualified Control.Exception as E
import Control.Monad (void, when)
import Data.Typeable
import Network.HTTP2
import Network.HTTP2.Priority
import Network.Wai.HTTP2 (Http2Application, Response)
import Network.Wai hiding (Response, responseStatus)
import Network.Wai.Handler.Warp.HTTP2.EncodeFrame
import Network.Wai.Handler.Warp.HTTP2.Manager
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.IORef
import qualified Network.Wai.Handler.Warp.Settings as S
import qualified Network.Wai.Handler.Warp.Timeout as T

----------------------------------------------------------------

-- | An 'Http2Application' takes a @forall a. Response a -> IO a@; this type
-- implements that with extra arguments.
type Responder = forall a. ThreadContinue -> T.Handle -> Stream ->
                 TBQueue Sequence -> Response a -> IO a

-- | This function is passed to workers.
--   They also pass 'Response's from 'Http2Application's to this function.
--   This function enqueues commands for the HTTP/2 sender.
response :: Context -> Manager -> Responder
response Context{outputQ} mgr tconf th strm sq (s, h, strmbdy) = do
    -- TODO(awpr)r HEAD requests will still stream.
    --
    -- We must not exit this WAI application.
    -- If the application exits, streaming would be also closed.
    -- So, this work occupies this thread.
    --
    -- We need to increase the number of workers.
    myThreadId >>= replaceWithAction mgr
    -- After this work, this thread stops to decease
    -- the number of workers.
    setThreadContinue tconf False
    tvar <- newTVarIO SyncNone
    let out = OResponse strm s h (Persist sq tvar)
    -- Since we must not enqueue an empty queue to the priority
    -- queue, we spawn a thread to ensure that the designated
    -- queue is not empty.
    void $ forkIO $ waiter tvar sq strm outputQ
    atomically $ writeTVar tvar (SyncNext out)
    let push b = do
            atomically $ writeTBQueue sq (SBuilder b)
            T.tickle th
        flush  = atomically $ writeTBQueue sq SFlush
    strmbdy push flush

data Break = Break deriving (Show, Typeable)

instance Exception Break

worker :: Context
       -> S.Settings
       -> T.Manager
       -> Http2Application
       -> Responder
       -> IO ()
worker ctx@Context{inputQ,outputQ} set tm app responder = do
    tid <- myThreadId
    sinfo <- newStreamInfo
    tcont <- newThreadContinue
    let setup = T.register tm $ E.throwTo tid Break
    E.bracket setup T.cancel $ go sinfo tcont
  where
    go sinfo tcont th = do
        setThreadContinue tcont True
        -- Since 'Body' is loop, we cannot control it.
        -- So, let's serialize 'Builder' with a designated queue.
        sq <- newTBQueueIO 10 -- fixme: hard coding: 10
        ex <- E.try $ do
            T.pause th
            Input strm req <- atomically $ readTQueue inputQ
            setStreamInfo sinfo strm req
            T.resume th
            T.tickle th
            app req $ responder tcont th strm sq
        cont1 <- case ex of
            Right trailers -> do
                atomically $ writeTBQueue sq $ SFinish trailers
                return True
            Left  e@(SomeException _)
              | Just Break        <- E.fromException e -> do
                  cleanup sinfo Nothing
                  return True
              -- killed by the sender
              | Just ThreadKilled <- E.fromException e -> do
                  cleanup sinfo Nothing
                  return False
              | otherwise -> do
                  cleanup sinfo (Just e)
                  return True
        cont2 <- getThreadContinue tcont
        when (cont1 && cont2) $ go sinfo tcont th
    cleanup sinfo me = do
        m <- getStreamInfo sinfo
        case m of
            Nothing -> return ()
            Just (strm,req) -> do
                closed ctx strm Killed
                let frame = resetFrame InternalError (streamNumber strm)
                enqueue outputQ (OFrame frame) highestPriority
                case me of
                    Nothing -> return ()
                    Just e  -> S.settingsOnException set (Just req) e
                clearStreamInfo sinfo

waiter :: TVar Sync -> TBQueue Sequence -> Stream -> PriorityTree Output -> IO ()
waiter tvar sq strm outQ = do
    -- waiting for actions other than SyncNone
    mx <- atomically $ do
        mout <- readTVar tvar
        case mout of
            SyncNone            -> retry
            SyncNext out        -> do
                writeTVar tvar SyncNone
                return $ Right out
            SyncFinish trailers -> return $ Left trailers
    case mx of
        Left  trailers -> enqueueWhenWindowIsOpen outQ (OTrailers strm trailers)
        Right out      -> do
            -- ensuring that the streaming queue is not empty.
            atomically $ do
                isEmpty <- isEmptyTBQueue sq
                when isEmpty retry
            -- ensuring that stream window is greater than 0.
            enqueueWhenWindowIsOpen outQ out
            waiter tvar sq strm outQ

----------------------------------------------------------------

-- | It would nice if responders could return values to workers.
--   Unfortunately, 'ResponseReceived' is already defined in WAI 2.0.
--   It is not wise to change this type.
--   So, a reference is shared by a responder and its worker.
--   The reference refers a value of this type as a return value.
--   If 'True', the worker continue to serve requests.
--   Otherwise, the worker get finished.
newtype ThreadContinue = ThreadContinue (IORef Bool)

newThreadContinue :: IO ThreadContinue
newThreadContinue = ThreadContinue <$> newIORef True

setThreadContinue :: ThreadContinue -> Bool -> IO ()
setThreadContinue (ThreadContinue ref) x = writeIORef ref x

getThreadContinue :: ThreadContinue -> IO Bool
getThreadContinue (ThreadContinue ref) = readIORef ref

----------------------------------------------------------------

-- | The type to store enough information for 'settingsOnException'.
newtype StreamInfo = StreamInfo (IORef (Maybe (Stream,Request)))

newStreamInfo :: IO StreamInfo
newStreamInfo = StreamInfo <$> newIORef Nothing

clearStreamInfo :: StreamInfo -> IO ()
clearStreamInfo (StreamInfo ref) = writeIORef ref Nothing

setStreamInfo :: StreamInfo -> Stream -> Request -> IO ()
setStreamInfo (StreamInfo ref) strm req = writeIORef ref $ Just (strm,req)

getStreamInfo :: StreamInfo -> IO (Maybe (Stream, Request))
getStreamInfo (StreamInfo ref) = readIORef ref
