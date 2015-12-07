{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.HTTP.Types as H
import Network.HTTP2
import Network.HTTP2.Priority
import Network.Wai
import Network.Wai.Handler.Warp.HTTP2.EncodeFrame
import Network.Wai.Handler.Warp.HTTP2.Manager
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.IORef
import qualified Network.Wai.Handler.Warp.Response as R
import qualified Network.Wai.Handler.Warp.Settings as S
import qualified Network.Wai.Handler.Warp.Timeout as T
import Network.Wai.Internal (Response(..), ResponseReceived(..), ResponseReceived(..))

----------------------------------------------------------------

-- | The wai definition is 'type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived'.
--   This type implements the second argument (Response -> IO ResponseReceived)
--   with extra arguments.
type Responder = ThreadContinue -> T.Handle -> Stream -> Request ->
                 Response -> IO ResponseReceived

-- | This function is passed to workers.
--   They also pass 'Response's from 'Application's to this function.
--   This function enqueues commands for the HTTP/2 sender.
response :: Context -> Manager -> Responder
response Context{outputQ} mgr tconf th strm req rsp = do
    case rsp of
        ResponseStream _ _ strmbdy -> do
            -- We must not exit this WAI application.
            -- If the application exits, streaming would be also closed.
            -- So, this work occupies this thread.
            --
            -- We need to increase the number of workers.
            myThreadId >>= replaceWithAction mgr
            -- After this work, this thread stops to decease
            -- the number of workers.
            setThreadContinue tconf False
            -- Since 'StreamingBody' is loop, we cannot control it.
            -- So, let's serialize 'Builder' with a designated queue.
            sq <- newTBQueueIO 10 -- fixme: hard coding: 10
            tvar <- newTVarIO SyncNone
            let out = OResponse strm rsp (Persist sq tvar)
            -- Since we must not enqueue an empty queue to the priority
            -- queue, we spawn a thread to ensure that the designated
            -- queue is not empty.
            void $ forkIO $ waiter tvar sq outputQ
            atomically $ writeTVar tvar (SyncNext out)
            let push b = do
                    atomically $ writeTBQueue sq (SBuilder b)
                    T.tickle th
                flush  = atomically $ writeTBQueue sq SFlush
            strmbdy push flush
            atomically $ writeTBQueue sq SFinish
        _ -> do
            setThreadContinue tconf True
            let hasBody = requestMethod req /= H.methodHead
                       && R.hasBody (responseStatus rsp)
                out = OResponse strm rsp (Oneshot hasBody)
            enqueueOrSpawnTemporaryWaiter strm outputQ out
    return ResponseReceived

data Break = Break deriving (Show, Typeable)

instance Exception Break

worker :: Context -> S.Settings -> T.Manager -> Application -> Responder -> IO ()
worker ctx@Context{inputQ,outputQ} set tm app responder = do
    tid <- myThreadId
    sinfo <- newStreamInfo
    tcont <- newThreadContinue
    let setup = T.register tm $ E.throwTo tid Break
    E.bracket setup T.cancel $ go sinfo tcont
  where
    go sinfo tcont th = do
        setThreadContinue tcont True
        ex <- E.try $ do
            T.pause th
            Input strm req <- atomically $ readTQueue inputQ
            setStreamInfo sinfo strm req
            T.resume th
            T.tickle th
            app req $ responder tcont th strm req
        cont1 <- case ex of
            Right ResponseReceived -> return True
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
                enqueueControl outputQ 0 (OFrame frame)
                case me of
                    Nothing -> return ()
                    Just e  -> S.settingsOnException set (Just req) e
                clearStreamInfo sinfo

waiter :: TVar Sync -> TBQueue Sequence -> PriorityTree Output -> IO ()
waiter tvar sq outQ = do
    -- waiting for actions other than SyncNone
    mx <- atomically $ do
        mout <- readTVar tvar
        case mout of
            SyncNone     -> retry
            SyncNext out -> do
                writeTVar tvar SyncNone
                return $ Just out
            SyncFinish   -> return Nothing
    case mx of
        Nothing -> return ()
        Just out -> do
            -- ensuring that the streaming queue is not empty.
            atomically $ do
                isEmpty <- isEmptyTBQueue sq
                when isEmpty retry
            -- ensuring that stream window is greater than 0.
            enqueueWhenWindowIsOpen outQ out
            waiter tvar sq outQ

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
