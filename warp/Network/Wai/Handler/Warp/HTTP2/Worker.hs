{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.HTTP2.Worker (
    Respond
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
import Network.Wai.Handler.Warp.Response (hasBody)
import Network.Wai.HTTP2
    ( Chunk(..)
    , HTTP2Application
    , PushPromise
    , Responder(runResponder)
    , RespondFunc
    )
import qualified Network.Wai.Handler.Warp.Settings as S
import qualified Network.Wai.Handler.Warp.Timeout as T

----------------------------------------------------------------

-- | An 'HTTP2Application' takes a function of status, headers, trailers, and
-- body; this type implements that by currying some internal arguments.
--
-- The token type of the RespondFunc is set to be ().  This is a bit
-- anti-climactic, but the real benefit of the token type is that the
-- application is forced to call the responder, and making it a boring type
-- doesn't break that property.
--
-- This is the argument to a 'Responder'.
type Respond = Request -> IO () -> Stream -> RespondFunc ()

-- | This function is passed to workers.  They also pass responses from
-- 'HTTP2Application's to this function.  This function enqueues commands for
-- the HTTP/2 sender.
response :: Context -> Manager -> ThreadContinue -> Respond
response ctx mgr tconf req tickle strm s h strmbdy = do
    -- TODO(awpr) HEAD requests will still stream.

    -- We must not exit this WAI application.
    -- If the application exits, streaming would be also closed.
    -- So, this work occupies this thread.
    --
    -- We need to increase the number of workers.
    myThreadId >>= replaceWithAction mgr
    -- After this work, this thread stops to decrease the number of workers.
    setThreadContinue tconf False

    runStream ctx OResponse req tickle strm s h strmbdy

-- | Set up a waiter thread and run the stream body with functions to enqueue
-- 'Sequence's on the stream's queue.
runStream :: Context
          -> (Stream -> H.Status -> H.ResponseHeaders -> Aux -> Output)
          -> Respond
runStream Context{outputQ} mkOutput req tickle strm s h strmbdy = do
    -- Since 'Body' is loop, we cannot control it.
    -- So, let's serialize 'Builder' with a designated queue.
    sq <- newTBQueueIO 10 -- fixme: hard coding: 10
    tvar <- newTVarIO SyncNone
    let out = mkOutput strm s h (Persist sq tvar)
    -- Since we must not enqueue an empty queue to the priority
    -- queue, we spawn a thread to ensure that the designated
    -- queue is not empty.
    void $ forkIO $ waiter tvar sq strm outputQ
    atomically $ writeTVar tvar $ SyncNext out
    if hasBody req s then do
        let write chunk = do
                atomically $ writeTBQueue sq $ case chunk of
                    BuilderChunk b -> SBuilder b
                    FileChunk path part -> SFile path part
                tickle
            flush  = atomically $ writeTBQueue sq SFlush
        trailers <- strmbdy write flush
        atomically $ writeTBQueue sq $ SFinish trailers
      else
        atomically $ writeTBQueue sq $ SFinish []

-- | Handle abnormal termination of a stream: mark it as closed, send a reset
-- frame, and call the user's 'settingsOnException' handler if applicable.
cleanupStream :: Context -> S.Settings -> Stream -> Maybe Request -> Maybe SomeException -> IO ()
cleanupStream Context{outputQ} set strm req me = do
    closed strm Killed
    let sid = streamNumber strm
        frame = resetFrame InternalError sid
    enqueueControl outputQ sid $ OFrame frame
    case me of
        Nothing -> return ()
        Just e -> S.settingsOnException set req e

-- | Push the given 'Responder' to the client if the settings allow it
-- (specifically 'enablePush' and 'maxConcurrentStreams').  Returns 'True' if
-- the stream was actually pushed.
--
-- This is the push function given to an 'HTTP2Application'.
pushResponder :: Context -> S.Settings -> Stream -> Request -> PushPromise -> Responder -> IO Bool
pushResponder ctx set strm req promise responder = do
    let Context{ http2settings
               , pushConcurrency
               } = ctx
    cnt <- readIORef pushConcurrency
    settings <- readIORef http2settings
    let enabled = enablePush settings
        fits = maybe True (cnt <) $ maxConcurrentStreams settings
        canPush = fits && enabled
    if canPush then
        actuallyPushResponder ctx set strm req promise responder
      else
        return False

-- | Set up a pushed stream and run the 'Responder' in its own thread.  Waits
-- for the sender thread to handle the push request.  This can fail to push the
-- stream and return 'False' if the sender dequeued the push request after the
-- associated stream was closed.
actuallyPushResponder :: Context -> S.Settings -> Stream -> Request -> PushPromise -> Responder -> IO Bool
actuallyPushResponder ctx set strm req promise responder = do
    let Context{ http2settings
               , nextPushStreamId
               , pushConcurrency
               , streamTable
               } = ctx
    -- Claim the next outgoing stream.
    newSid <- atomicModifyIORef nextPushStreamId $ \sid -> (sid+2, sid)
    ws <- initialWindowSize <$> readIORef http2settings

    newStrm <- newStream pushConcurrency newSid ws
    -- Section 5.3.5 of RFC 7540 defines the weight of push promise is 16.
    -- But we need not to follow the spec. So, this value would change
    -- if necessary.
    writeIORef (streamPrecedence newStrm) $
        toPrecedence $ defaultPriority { streamDependency = streamNumber strm }
    opened newStrm
    insert streamTable newSid newStrm

    -- Set up a channel for the sender to report back whether it pushed the
    -- stream.
    mvar <- newEmptyMVar

    let mkOutput = OPush strm promise mvar
        tickle = return ()
        respond = runStream ctx mkOutput req

    -- TODO(awpr): synthesize a Request for 'settingsOnException'?
    _ <- forkIO $ runResponder responder (respond tickle newStrm) `E.catch`
        (cleanupStream ctx set strm Nothing . Just)

    takeMVar mvar

data Break = Break deriving (Show, Typeable)

instance Exception Break

worker :: Context
       -> S.Settings
       -> T.Manager
       -> HTTP2Application
       -> (ThreadContinue -> Respond)
       -> IO ()
worker ctx@Context{inputQ} set tm app respond = do
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
            let responder = app req $ pushResponder ctx set strm req
            runResponder responder $ respond tcont req (T.tickle th) strm
        cont1 <- case ex of
            Right () -> return True
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
                cleanupStream ctx set strm (Just req) me
                clearStreamInfo sinfo

-- | A dedicated waiter thread to re-enqueue the stream in the priority tree
-- whenever output becomes available.  When the sender drains the queue and
-- moves on to another stream, it drops a message in the 'TVar', and this
-- thread wakes up, waits for more output to become available, and re-enqueues
-- the stream.
waiter :: TVar Sync -> TBQueue Sequence -> Stream -> PriorityTree Output -> IO ()
waiter tvar sq strm outQ = do
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
        Nothing  -> return ()
        Just out -> do
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
--   So, a reference is shared by a 'Respond' and its worker.
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
