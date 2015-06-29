{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.HTTP2.Receiver (frameReceiver) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Concurrent (takeMVar)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (when, unless, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import Network.HTTP2
import Network.HTTP2.Priority
import Network.Wai.Handler.Warp.HTTP2.EncodeFrame
import Network.Wai.Handler.Warp.HTTP2.HPACK
import Network.Wai.Handler.Warp.HTTP2.Request
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.IORef
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

frameReceiver :: Context -> MkReq -> (BufSize -> IO ByteString) -> IO ()
frameReceiver ctx@Context{..} mkreq recvN =
    E.handle sendGoaway loop `E.finally` takeMVar wait
  where
    sendGoaway (ConnectionError err msg) = do
        csid <- readIORef currentStreamId
        let frame = goawayFrame csid err msg
        enqueue outputQ (OGoaway frame) highestPriority
    sendGoaway _                         = return ()

    sendReset err sid = do
        let frame = resetFrame err sid
        enqueue outputQ (OFrame frame) highestPriority

    loop = do
        hd <- recvN frameHeaderLength
        if BS.null hd then
            enqueue outputQ OFinish highestPriority
          else do
            cont <- processStreamGuardingError $ decodeFrameHeader hd
            when cont loop

    processStreamGuardingError (_, FrameHeader{..})
      | isResponse streamId = E.throwIO $ ConnectionError ProtocolError "stream id should be odd"
    processStreamGuardingError (FrameUnknown _, FrameHeader{..}) = do
        mx <- readIORef continued
        case mx of
            Nothing -> do
                -- ignoring unknown frame
                consume payloadLength
                return True
            Just _  -> E.throwIO $ ConnectionError ProtocolError "unknown frame"
    processStreamGuardingError (FramePushPromise, _) =
        E.throwIO $ ConnectionError ProtocolError "push promise is not allowed"
    processStreamGuardingError typhdr@(ftyp, header@FrameHeader{..}) = do
        settings <- readIORef http2settings
        case checkFrameHeader settings typhdr of
            Left h2err -> case h2err of
                StreamError err sid -> do
                    sendReset err sid
                    consume payloadLength
                    return True
                connErr -> E.throwIO connErr
            Right _ -> do
                ex <- E.try $ controlOrStream ftyp header
                case ex of
                    Left (StreamError err sid) -> do
                        sendReset err sid
                        return True
                    Left connErr -> E.throw connErr
                    Right cont -> return cont

    controlOrStream ftyp header@FrameHeader{..}
      | isControl streamId = do
          pl <- recvN payloadLength
          control ftyp header pl ctx
      | otherwise = do
          checkContinued
          strm@Stream{..} <- getStream
          pl <- recvN payloadLength
          state <- readIORef streamState
          state' <- stream ftyp header pl ctx state strm
          case state' of
              Open (NoBody hdr pri) -> do
                  resetContinued
                  case validateHeaders hdr of
                      Just vh -> do
                          when (isJust (vhCL vh) && vhCL vh /= Just 0) $
                              E.throwIO $ StreamError ProtocolError streamId
                          writeIORef streamState HalfClosed
                          let req = mkreq vh (return "")
                          atomically $ writeTQueue inputQ $ Input strm req pri
                      Nothing -> E.throwIO $ StreamError ProtocolError streamId
              Open (HasBody hdr pri) -> do
                  resetContinued
                  case validateHeaders hdr of
                      Just vh -> do
                          q <- newTQueueIO
                          writeIORef streamState (Open (Body q))
                          writeIORef streamContentLength $ vhCL vh
                          readQ <- newReadBody q
                          bodySource <- mkSource readQ
                          let req = mkreq vh (readSource bodySource)
                          atomically $ writeTQueue inputQ $ Input strm req pri
                      Nothing -> E.throwIO $ StreamError ProtocolError streamId
              s@(Open Continued{}) -> do
                  setContinued
                  writeIORef streamState s
              s -> do -- Idle, Open Body, HalfClosed, Closed
                  resetContinued
                  writeIORef streamState s
          return True
       where
         setContinued = writeIORef continued (Just streamId)
         resetContinued = writeIORef continued Nothing
         checkContinued = do
             mx <- readIORef continued
             case mx of
                 Nothing  -> return ()
                 Just sid
                   | sid == streamId && ftyp == FrameContinuation -> return ()
                   | otherwise -> E.throwIO $ ConnectionError ProtocolError "continuation frame must follow"
         getStream = do
             mstrm0 <- search streamTable streamId
             case mstrm0 of
                 Just strm0 -> do
                     when (ftyp == FrameHeaders) $ do
                         st <- readIORef $ streamState strm0
                         when (isHalfClosed st) $ E.throwIO $ ConnectionError ProtocolError "header must not be sent to half closed"
                     return strm0
                 Nothing    -> do
                     when (ftyp `notElem` [FrameHeaders,FramePriority]) $
                         E.throwIO $ ConnectionError ProtocolError "this frame is not allowed in an idel stream"
                     when (ftyp == FrameHeaders) $ do
                         csid <- readIORef currentStreamId
                         if streamId <= csid then
                             E.throwIO $ ConnectionError ProtocolError "stream identifier must not decrease"
                           else
                             writeIORef currentStreamId streamId
                         cnt <- readIORef concurrency
                         when (cnt >= recommendedConcurrency) $
                             E.throwIO $ ConnectionError RefusedStream "over max concurrency"
                     ws <- initialWindowSize <$> readIORef http2settings
                     newstrm <- newStream streamId (fromIntegral ws)
                     when (ftyp == FrameHeaders) $ opened ctx newstrm
                     insert streamTable streamId newstrm
                     return newstrm

    consume = void . recvN

----------------------------------------------------------------

control :: FrameTypeId -> FrameHeader -> ByteString -> Context -> IO Bool
control FrameSettings header@FrameHeader{..} bs Context{..} = do
    SettingsFrame alist <- guardIt $ decodeSettingsFrame header bs
    case checkSettingsList alist of
        Just x  -> E.throwIO x
        Nothing -> return ()
    unless (testAck flags) $ do
        modifyIORef http2settings $ \old -> updateSettings old alist
        let frame = settingsFrame setAck []
        enqueue outputQ (OFrame frame) highestPriority
    return True

control FramePing FrameHeader{..} bs Context{..} =
    if testAck flags then
        E.throwIO $ ConnectionError ProtocolError "the ack flag of this ping frame must not be set"
      else do
        let frame = pingFrame bs
        enqueue outputQ (OFrame frame) defaultPriority
        return True

control FrameGoAway _ _ Context{..} = do
    enqueue outputQ OFinish highestPriority
    return False

control FrameWindowUpdate header@FrameHeader{..} bs Context{..} = do
    WindowUpdateFrame n <- guardIt $ decodeWindowUpdateFrame header bs
    w <- (n +) <$> atomically (readTVar connectionWindow)
    when (isWindowOverflow w) $ E.throwIO $ ConnectionError FlowControlError "control window should be less than 2^31"
    atomically $ writeTVar connectionWindow w
    return True

control _ _ _ _ =
    -- must not reach here
    return False

----------------------------------------------------------------

guardIt :: Either HTTP2Error a -> IO a
guardIt x = case x of
    Left err    -> E.throwIO err
    Right frame -> return frame

checkPriority :: Priority -> StreamId -> IO ()
checkPriority p me
  | dep == me = E.throwIO $ StreamError ProtocolError me
  | otherwise = return ()
  where
    dep = streamDependency p

stream :: FrameTypeId -> FrameHeader -> ByteString -> Context -> StreamState -> Stream -> IO StreamState
stream FrameHeaders header@FrameHeader{..} bs ctx (Open JustOpened) Stream{..} = do
    HeadersFrame mp frag <- guardIt $ decodeHeadersFrame header bs
    pri <- case mp of
        Nothing -> return defaultPriority
        Just p  -> do
            checkPriority p streamNumber
            return p
    let endOfStream = testEndStream flags
        endOfHeader = testEndHeader flags
    if endOfHeader then do
        hdr <- hpackDecodeHeader frag ctx
        return $ if endOfStream then
                    Open (NoBody hdr pri)
                   else
                    Open (HasBody hdr pri)
      else do
        let !siz = BS.length frag
        return $ Open $ Continued [frag] siz 1 endOfStream pri

stream FrameHeaders header@FrameHeader{..} bs _ s@(Open (Body q)) Stream{..} = do
    -- trailer is not supported.
    -- let's read and ignore it.
    HeadersFrame _ _ <- guardIt $ decodeHeadersFrame header bs
    let endOfStream = testEndStream flags
    if endOfStream then do
        atomically $ writeTQueue q ""
        return HalfClosed
      else
        return s

stream FrameData header@FrameHeader{..} bs Context{..} s@(Open (Body q)) Stream{..} = do
    DataFrame body <- guardIt $ decodeDataFrame header bs
    let endOfStream = testEndStream flags
    len0 <- readIORef streamBodyLength
    let !len = len0 + payloadLength
    writeIORef streamBodyLength len
    when (payloadLength /= 0) $ do
        let frame1 = windowUpdateFrame 0 payloadLength
            frame2 = windowUpdateFrame streamNumber payloadLength
            frame = frame1 `BS.append` frame2
        enqueue outputQ (OFrame frame) highestPriority
    atomically $ writeTQueue q body
    if endOfStream then do
        mcl <- readIORef streamContentLength
        case mcl of
            Nothing -> return ()
            Just cl -> when (cl /= len) $ E.throwIO $ StreamError ProtocolError streamId
        atomically $ writeTQueue q ""
        return HalfClosed
      else
        return s

stream FrameContinuation FrameHeader{..} frag ctx (Open (Continued rfrags siz n endOfStream pri)) _ = do
    let endOfHeader = testEndHeader flags
        rfrags' = frag : rfrags
        siz' = siz + BS.length frag
        n' = n + 1
    when (siz' > 51200) $ -- fixme: hard coding: 50K
      E.throwIO $ ConnectionError EnhanceYourCalm "Header is too big"
    when (n' > 10) $ -- fixme: hard coding
      E.throwIO $ ConnectionError EnhanceYourCalm "Header is too fragmented"
    if endOfHeader then do
        let hdrblk = BS.concat $ reverse rfrags'
        hdr <- hpackDecodeHeader hdrblk ctx
        return $ if endOfStream then
                    Open (NoBody hdr pri)
                   else
                    Open (HasBody hdr pri)
      else
        return $ Open $ Continued rfrags' siz' n' endOfStream pri

stream FrameContinuation _ _ _ _ _ = E.throwIO $ ConnectionError ProtocolError "continue frame cannot come here"

stream FrameWindowUpdate header@FrameHeader{..} bs Context{..} s Stream{..} = do
    WindowUpdateFrame n <- guardIt $ decodeWindowUpdateFrame header bs
    w <- (n +) <$> atomically (readTVar streamWindow)
    when (isWindowOverflow w) $
        E.throwIO $ StreamError FlowControlError streamId
    atomically $ writeTVar streamWindow w
    return s

stream FrameRSTStream header bs ctx _ strm = do
    RSTStreamFrame e <- guardIt $ decoderstStreamFrame header bs
    let cc = Reset e
    closed ctx strm cc
    return $ Closed cc -- will be written to streamState again

stream FramePriority header bs Context{..} s Stream{..} = do
    PriorityFrame p <- guardIt $ decodePriorityFrame header bs
    checkPriority p streamNumber
    prepare outputQ streamNumber p
    return s

-- this ordering is important
stream _ _ _ _ (Open Continued{}) _ = E.throwIO $ ConnectionError ProtocolError "an illegal frame follows header/continuation frames"
stream FrameData FrameHeader{..} _ _ _ _ = E.throwIO $ StreamError StreamClosed streamId
stream _ FrameHeader{..} _ _ _ _ = E.throwIO $ StreamError ProtocolError streamId
