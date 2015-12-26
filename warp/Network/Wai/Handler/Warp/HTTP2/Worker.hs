{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.HTTP2.Worker (
    Responder
  , response
  , worker
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
import Data.Monoid (mempty)
#endif
import Control.Concurrent.STM
import Control.Exception (SomeException(..), AsyncException(..))
import qualified Control.Exception as E
import Control.Monad (when)
import Data.ByteString.Builder (byteString)
import Data.Hashable (hash)
import qualified Network.HTTP.Types as H
import Network.HTTP2
import Network.Wai
import Network.Wai.Handler.Warp.File
import Network.Wai.Handler.Warp.FileInfoCache
import Network.Wai.Handler.Warp.HTTP2.EncodeFrame
import Network.Wai.Handler.Warp.HTTP2.Manager
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.IORef
import qualified Network.Wai.Handler.Warp.Response as R
import qualified Network.Wai.Handler.Warp.Settings as S
import qualified Network.Wai.Handler.Warp.Timeout as T
import Network.Wai.Handler.Warp.Types
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
response :: InternalInfo -> S.Settings -> Context -> Manager -> Responder
response ii settings Context{outputQ} mgr tconf th strm req rsp
  | R.hasBody s0 = case rsp of
    ResponseStream _ _ strmbdy
      | isHead             -> responseNoBody s0 hs0
      | otherwise          -> responseStreaming strmbdy
    ResponseBuilder _ _ b
      | isHead             -> responseNoBody s0 hs0
      | otherwise          -> responseBuilderBody s0 hs0 b
    ResponseFile _ _ p mp  -> responseFileXXX p mp
    ResponseRaw _ _        -> error "HTTP/2 does not support ResponseRaw"
  | otherwise               = responseNoBody s0 hs0
  where
    !isHead = requestMethod req == H.methodHead
    !s0 = responseStatus rsp
    !hs0 = responseHeaders rsp
    !logger = S.settingsLogger settings

    -- Ideally, log messages should be written when responses are
    -- actually sent. But there is no way to keep good memory usage
    -- (resist to Request leak) and throughput. By compromise,
    -- log message are written here even the window size of streams
    -- is 0.

    responseNoBody s hs = do
        logger req s Nothing
        setThreadContinue tconf True
        let rspn = RspnNobody s hs
            out = ORspn strm rspn
        enqueueOutput outputQ out
        return ResponseReceived

    responseBuilderBody s hs bdy = do
        logger req s Nothing
        setThreadContinue tconf True
        let rspn = RspnBuilder s hs bdy
            out = ORspn strm rspn
        enqueueOutput outputQ out
        return ResponseReceived

    responseFileXXX path Nothing = do
        let !h = hash $ rawPathInfo req
        efinfo <- E.try $ fileInfo' ii h path
        case efinfo of
            Left (_ex :: E.IOException) -> response404
            Right finfo -> case conditionalRequest finfo hs0 (indexRequestHeader (requestHeaders req)) of
                 WithoutBody s         -> responseNoBody s hs0
                 WithBody s hs beg len -> responseFile2XX s hs h path (Just (FilePart beg len (fileInfoSize finfo)))

    responseFileXXX path mpart = responseFile2XX s0 hs0 h path mpart
      where
        !h = hash $ rawPathInfo req

    responseFile2XX s hs h path mpart
      | isHead    = do
          logger req s Nothing
          responseNoBody s hs
      | otherwise = do
          logger req s (filePartByteCount <$> mpart)
          setThreadContinue tconf True
          let rspn = RspnFile s hs h path mpart
              out = ORspn strm rspn
          enqueueOutput outputQ out
          return ResponseReceived

    response404 = responseBuilderBody s hs body
      where
        s = H.notFound404
        hs = R.replaceHeader H.hContentType "text/plain; charset=utf-8" hs0
        body = byteString "File not found"

    responseStreaming strmbdy = do
        logger req s0 Nothing
        -- We must not exit this WAI application.
        -- If the application exits, streaming would be also closed.
        -- So, this work occupies this thread.
        --
        -- We need to increase the number of workers.
        spawnAction mgr
        -- After this work, this thread stops to decease
        -- the number of workers.
        setThreadContinue tconf False
        -- Since 'StreamingBody' is loop, we cannot control it.
        -- So, let's serialize 'Builder' with a designated queue.
        tbq <- newTBQueueIO 10 -- fixme: hard coding: 10
        let rspn = RspnStreaming s0 hs0 tbq
            out = ORspn strm rspn
        enqueueOutput outputQ out
        let push b = do
              atomically $ writeTBQueue tbq (SBuilder b)
              T.tickle th
            flush  = atomically $ writeTBQueue tbq SFlush
        _ <- strmbdy push flush
        atomically $ writeTBQueue tbq SFinish
        deleteMyId mgr
        return ResponseReceived

worker :: Context -> S.Settings -> Application -> Responder -> T.Manager -> IO ()
worker ctx@Context{inputQ,controlQ} set app responder tm = do
    sinfo <- newStreamInfo
    tcont <- newThreadContinue
    E.bracket (T.registerKillThread tm) T.cancel $ go sinfo tcont
  where
    go sinfo tcont th = do
        setThreadContinue tcont True
        ex <- E.try $ do
            T.pause th
            inp@(Input strm req) <- atomically $ readTQueue inputQ
            setStreamInfo sinfo inp
            T.resume th
            T.tickle th
            app req $ responder tcont th strm req
        cont1 <- case ex of
            Right ResponseReceived -> return True
            Left  e@(SomeException _)
              -- killed by the local worker manager
              | Just ThreadKilled    <- E.fromException e -> return False
              -- killed by the local timeout manager
              | Just T.TimeoutThread <- E.fromException e -> do
                  cleanup sinfo Nothing
                  return True
              | otherwise -> do
                  cleanup sinfo $ Just e
                  return True
        cont2 <- getThreadContinue tcont
        clearStreamInfo sinfo
        when (cont1 && cont2) $ go sinfo tcont th
    cleanup sinfo me = do
        minp <- getStreamInfo sinfo
        case minp of
            Nothing               -> return ()
            Just (Input strm req) -> do
                closed ctx strm Killed
                let frame = resetFrame InternalError (streamNumber strm)
                enqueueControl controlQ $ CFrame frame
                case me of
                    Nothing -> return ()
                    Just e  -> S.settingsOnException set (Just req) e

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
newtype StreamInfo = StreamInfo (IORef (Maybe Input))

newStreamInfo :: IO StreamInfo
newStreamInfo = StreamInfo <$> newIORef Nothing

clearStreamInfo :: StreamInfo -> IO ()
clearStreamInfo (StreamInfo ref) = writeIORef ref Nothing

setStreamInfo :: StreamInfo -> Input -> IO ()
setStreamInfo (StreamInfo ref) inp = writeIORef ref $ Just inp

getStreamInfo :: StreamInfo -> IO (Maybe Input)
getStreamInfo (StreamInfo ref) = readIORef ref
