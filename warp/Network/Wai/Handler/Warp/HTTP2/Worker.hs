{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE PatternGuards, BangPatterns #-}
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

type Responder = ThreadContinue -> T.Handle -> Stream -> Priority -> Request -> Response -> IO ResponseReceived

response :: Context -> Manager -> Responder
response Context{outputQ} mgr tconf th strm pri req rsp = do
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
            enqueue outputQ (OResponse strm rsp (Persist sq tvar)) pri
            let push b = do
                    atomically $ writeTBQueue sq (SBuilder b)
                    T.tickle th
                flush  = atomically $ writeTBQueue sq SFlush
            -- Since we must not enqueue an empty queue to the priority
            -- queue, we spawn a thread to ensure that the designated
            -- queue is not empty.
            void $ forkIO $ waiter tvar sq (enqueue outputQ) strm pri
            strmbdy push flush
            atomically $ writeTBQueue sq SFinish
        _ -> do
            setThreadContinue tconf True
            let hasBody = requestMethod req /= H.methodHead
                       || R.hasBody (responseStatus rsp)
            enqueue outputQ (OResponse strm rsp (Oneshot hasBody)) pri
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
            Input strm req pri <- atomically $ readTQueue inputQ
            setStreamInfo sinfo strm req
            T.resume th
            T.tickle th
            app req $ responder tcont th strm pri req
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
                enqueue outputQ (OFrame frame) highestPriority
                case me of
                    Nothing -> return ()
                    Just e  -> S.settingsOnException set (Just req) e
                clearStreamInfo sinfo

waiter :: TVar Sync -> TBQueue Sequence
       -> (Output -> Priority -> IO ()) -> Stream -> Priority
       -> IO ()
waiter tvar sq enq strm pri = do
    mx <- atomically $ do
        mout <- readTVar tvar
        case mout of
            SyncNone     -> retry
            SyncNext nxt -> do
                writeTVar tvar SyncNone
                return $ Just nxt
            SyncFinish   -> return Nothing
    case mx of
        Nothing -> return ()
        Just next -> do
            atomically $ do
                isEmpty <- isEmptyTBQueue sq
                when isEmpty retry
            enq (ONext strm next) pri
            waiter tvar sq enq strm pri

----------------------------------------------------------------

newtype ThreadContinue = ThreadContinue (IORef Bool)

newThreadContinue :: IO ThreadContinue
newThreadContinue = ThreadContinue <$> newIORef True

setThreadContinue :: ThreadContinue -> Bool -> IO ()
setThreadContinue (ThreadContinue ref) x = writeIORef ref x

getThreadContinue :: ThreadContinue -> IO Bool
getThreadContinue (ThreadContinue ref) = readIORef ref

----------------------------------------------------------------

newtype StreamInfo = StreamInfo (IORef (Maybe (Stream,Request)))

newStreamInfo :: IO StreamInfo
newStreamInfo = StreamInfo <$> newIORef Nothing

clearStreamInfo :: StreamInfo -> IO ()
clearStreamInfo (StreamInfo ref) = writeIORef ref Nothing

setStreamInfo :: StreamInfo -> Stream -> Request -> IO ()
setStreamInfo (StreamInfo ref) strm req = writeIORef ref $ Just (strm,req)

getStreamInfo :: StreamInfo -> IO (Maybe (Stream, Request))
getStreamInfo (StreamInfo ref) = readIORef ref
