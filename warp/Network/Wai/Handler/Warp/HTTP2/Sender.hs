{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns, CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.Wai.Handler.Warp.HTTP2.Sender (frameSender) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Extra as B
import Data.Maybe (isNothing)
import Foreign.Ptr
import Network.HPACK (setLimitForEncoding)
import Network.HTTP2
import Network.HTTP2.Priority (dequeueSTM)
import Network.Wai
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.HTTP2.EncodeFrame
import Network.Wai.Handler.Warp.HTTP2.HPACK
import Network.Wai.Handler.Warp.HTTP2.Manager (Manager)
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.IORef
import qualified Network.Wai.Handler.Warp.Settings as S
#ifndef WINDOWS
import qualified Network.Wai.Handler.Warp.Timeout as T
#endif
import Network.Wai.Handler.Warp.Types
import Network.Wai.Internal (Response(..))

#ifdef WINDOWS
import qualified System.IO as IO
#else
import Network.Wai.Handler.Warp.FdCache
import Network.Wai.Handler.Warp.SendFile (positionRead)
import System.Posix.IO (openFd, OpenFileFlags(..), defaultFileFlags, OpenMode(ReadOnly), closeFd)
import System.Posix.Types
#endif

----------------------------------------------------------------

data Leftover = LZero
              | LOne B.BufferWriter
              | LTwo BS.ByteString B.BufferWriter

----------------------------------------------------------------

getStreamWindowSize :: Stream -> IO WindowSize
getStreamWindowSize Stream{streamWindow} = atomically $ readTVar streamWindow

waitStreamWindowSize :: Stream -> STM ()
waitStreamWindowSize Stream{streamWindow} = do
    w <- readTVar streamWindow
    check (w > 0)

waitStreaming :: TBQueue a -> STM ()
waitStreaming tbq = do
    isEmpty <- isEmptyTBQueue tbq
    check (isEmpty == False)

frameSender :: Context -> Connection -> InternalInfo -> S.Settings -> Manager -> IO ()
frameSender ctx@Context{outputQ,controlQ,connectionWindow,encodeDynamicTable}
            conn@Connection{connWriteBuffer,connBufferSize,connSendAll}
            ii settings mgr = loop `E.catch` ignore
  where
    dequeueControl = Left <$> readTQueue controlQ
    dequeueOutput    = Right <$> do
        w <- readTVar connectionWindow
        check (w > 0)
        dequeueSTM outputQ

    loop = do
        ex <- atomically (dequeueControl `orElse` dequeueOutput)
        case ex of
            Left  ctl         -> control ctl
            Right (_,pre,out) -> do
                writeIORef (streamPrecedence (outputStream out)) pre
                output out

    control CFinish         = return ()
    control (CGoaway frame) = connSendAll frame
    control (CFrame frame)  = do
        connSendAll frame
        loop
    control (CSettings frame alist) = do
        connSendAll frame
        case lookup SettingsHeaderTableSize alist of
            Nothing  -> return ()
            Just siz -> do
                dyntbl <- readIORef encodeDynamicTable
                setLimitForEncoding siz dyntbl
        loop

    output out@(ONext strm binfo curr) = do
        whenReadyOrEnqueueAgain strm binfo out $ \sws -> do
            cws <- atomically $ readTVar connectionWindow
            let !lim = min cws sws
            -- Data frame payload
            Next datPayloadLen mnext <- curr lim
            fillDataHeaderSend strm 0 datPayloadLen mnext
            maybeEnqueueNext strm mnext binfo
        loop
    output out@(OResponse strm binfo rsp) = do
        whenReadyOrEnqueueAgain strm binfo out $ \sws -> do
            -- Header frame and Continuation frame
            let sid = streamNumber strm
                endOfStream = case binfo of
                    Persist{}          -> False
                    OneshotWithBody    -> False
                    OneshotWithoutBody -> True
            len <- headerContinue sid rsp endOfStream
            let total = len + frameHeaderLength
            case binfo of
                OneshotWithBody -> do
                    -- Data frame payload
                    (off, _) <- sendHeadersIfNecessary total
                    let payloadOff = off + frameHeaderLength
                    cws <- atomically $ readTVar connectionWindow
                    let !lim = min cws sws
                    Next datPayloadLen mnext <-
                        fillResponseBodyGetNext conn ii payloadOff lim rsp
                    fillDataHeaderSend strm total datPayloadLen mnext
                    maybeEnqueueNext strm mnext binfo
                OneshotWithoutBody -> do
                    -- "closed" must be before "connSendAll". If not,
                    -- the context would be switched to the receiver,
                    -- resulting the inconsistency of concurrency.
                    closed ctx strm Finished
                    flushN total
                Persist sq -> do
                    (off, needSend) <- sendHeadersIfNecessary total
                    let payloadOff = off + frameHeaderLength
                    cws <- atomically $ readTVar connectionWindow
                    let !lim = min cws sws
                    Next datPayloadLen mnext <-
                        fillStreamBodyGetNext conn payloadOff lim sq strm
                    -- If no data was immediately available, avoid sending an
                    -- empty data frame.
                    if datPayloadLen > 0 then
                        fillDataHeaderSend strm total datPayloadLen mnext
                      else
                        when needSend $ flushN off
                    maybeEnqueueNext strm mnext binfo
        loop

    whenReadyOrEnqueueAgain strm binfo out body = E.handle resetStream $ do
        state <- readIORef $ streamState strm
        unless (isClosed state) $ case binfo of
            Persist tbq -> checkStreaming tbq
            _           -> checkStreamWindowSize
      where
        checkStreaming tbq = do
            isEmpty <- atomically $ isEmptyTBQueue tbq
            if isEmpty then
                forkAndEnqueueWhenReady (waitStreaming tbq) outputQ out mgr
              else
                checkStreamWindowSize
        checkStreamWindowSize = do
            sws <- getStreamWindowSize strm
            if sws == 0 then do
                forkAndEnqueueWhenReady (waitStreamWindowSize strm) outputQ out mgr
              else
                body sws
        resetStream e = do
            closed ctx strm (ResetByMe e)
            let !rst = resetFrame InternalError $ streamNumber strm
            enqueueControl controlQ $ CFrame rst

    -- Flush the connection buffer to the socket, where the first 'n' bytes of
    -- the buffer are filled.
    flushN :: Int -> IO ()
    flushN n = bufferIO connWriteBuffer n connSendAll

    headerContinue sid rsp endOfStream = do
        builder <- hpackEncodeHeader ctx ii settings rsp
        (len, signal) <- B.runBuilder builder bufHeaderPayload headerPayloadLim
        let flag0 = case signal of
                B.Done -> setEndHeader defaultFlags
                _      -> defaultFlags
            flag = if endOfStream then setEndStream flag0 else flag0
        fillFrameHeader FrameHeaders len sid flag connWriteBuffer
        continue sid len signal

    continue _   len B.Done = return len
    continue sid len (B.More _ writer) = do
        flushN $ len + frameHeaderLength
        (len', signal') <- writer bufHeaderPayload headerPayloadLim
        let flag = case signal' of
                B.Done -> setEndHeader defaultFlags
                _      -> defaultFlags
        fillFrameHeader FrameContinuation len' sid flag connWriteBuffer
        continue sid len' signal'
    continue sid len (B.Chunk bs writer) = do
        flushN $ len + frameHeaderLength
        let (bs1,bs2) = BS.splitAt headerPayloadLim bs
            len' = BS.length bs1
        void $ copy bufHeaderPayload bs1
        fillFrameHeader FrameContinuation len' sid defaultFlags connWriteBuffer
        if bs2 == "" then
            continue sid len' (B.More 0 writer)
          else
            continue sid len' (B.Chunk bs2 writer)

    -- True if the connection buffer has room for a 1-byte data frame.
    canFitDataFrame total = total + frameHeaderLength < connBufferSize

    -- Re-enqueue the stream in the output queue.
    maybeEnqueueNext :: Stream -> Maybe DynaNext -> BodyInfo -> IO ()
    maybeEnqueueNext strm (Just next) binfo = do
        let !out = ONext strm binfo next
        enqueueOutput outputQ out
    maybeEnqueueNext _    _           _     = return ()


    -- Send headers if there is not room for a 1-byte data frame, and return
    -- the offset of the next frame's first header byte and whether the headers
    -- still need to be sent.
    sendHeadersIfNecessary total
      | canFitDataFrame total = return (total, True)
      | otherwise             = do
          flushN total
          return (0, False)

    fillDataHeaderSend strm otherLen datPayloadLen mnext = do
        -- Data frame header
        let sid = streamNumber strm
            buf = connWriteBuffer `plusPtr` otherLen
            total = otherLen + frameHeaderLength + datPayloadLen
            flag = case mnext of
                Nothing -> setEndStream defaultFlags
                _       -> defaultFlags
        fillFrameHeader FrameData datPayloadLen sid flag buf
        -- "closed" must be before "flushN". If not,
        -- the context would be switched to the receiver,
        -- resulting the inconsistency of concurrency.
        when (isNothing mnext) $ closed ctx strm Finished
        flushN total
        atomically $ modifyTVar' connectionWindow (subtract datPayloadLen)
        atomically $ modifyTVar' (streamWindow strm) (subtract datPayloadLen)

    fillFrameHeader ftyp len sid flag buf = encodeFrameHeaderBuf ftyp hinfo buf
      where
        hinfo = FrameHeader len flag sid

    bufHeaderPayload = connWriteBuffer `plusPtr` frameHeaderLength
    headerPayloadLim = connBufferSize - frameHeaderLength

    ignore :: E.SomeException -> IO ()
    ignore _ = return ()


----------------------------------------------------------------

{-
ResponseFile Status ResponseHeaders FilePath (Maybe FilePart)
ResponseBuilder Status ResponseHeaders Builder
ResponseStream Status ResponseHeaders StreamingBody
ResponseRaw (IO ByteString -> (ByteString -> IO ()) -> IO ()) Response
-}

fillResponseBodyGetNext :: Connection -> InternalInfo -> Int -> WindowSize -> Response -> IO Next
fillResponseBodyGetNext Connection{connWriteBuffer,connBufferSize}
                        _ off lim (ResponseBuilder _ _ bb) = do
    let datBuf = connWriteBuffer `plusPtr` off
        room = min (connBufferSize - off) lim
    (len, signal) <- B.runBuilder bb datBuf room
    return $ nextForBuilder connWriteBuffer connBufferSize len signal

#ifdef WINDOWS
fillResponseBodyGetNext Connection{connWriteBuffer,connBufferSize}
                        _ off lim (ResponseFile _ _ path mpart) = do
    let datBuf = connWriteBuffer `plusPtr` off
        room = min (connBufferSize - off) lim
    (start, bytes) <- fileStartEnd path mpart
    -- fixme: how to close Handle? GC does it at this moment.
    h <- IO.openBinaryFile path IO.ReadMode
    IO.hSeek h IO.AbsoluteSeek start
    len <- IO.hGetBufSome h datBuf (mini room bytes)
    let bytes' = bytes - fromIntegral len
    return $ nextForFile len connWriteBuffer connBufferSize h bytes' (return ())
#else
fillResponseBodyGetNext Connection{connWriteBuffer,connBufferSize}
                        ii off lim (ResponseFile _ _ path mpart) = do
    (fd, refresh) <- case fdCacher ii of
        Just fdcache -> getFd fdcache path
        Nothing      -> do
            fd' <- openFd path ReadOnly Nothing defaultFileFlags{nonBlock=True}
            th <- T.register (timeoutManager ii) (closeFd fd')
            return (fd', T.tickle th)
    let datBuf = connWriteBuffer `plusPtr` off
        room = min (connBufferSize - off) lim
    (start, bytes) <- fileStartEnd path mpart
    len <- positionRead fd datBuf (mini room bytes) start
    refresh
    let len' = fromIntegral len
    return $ nextForFile len connWriteBuffer connBufferSize fd (start + len') (bytes - len') refresh
#endif

fillResponseBodyGetNext _ _ _ _ _ = error "fillResponseBodyGetNext"

fileStartEnd :: FilePath -> Maybe FilePart -> IO (Integer, Integer)
fileStartEnd _ (Just part) =
    return (filePartOffset part, filePartByteCount part)
fileStartEnd _ _ = error "fileStartEnd"

----------------------------------------------------------------

fillStreamBodyGetNext :: Connection -> Int -> WindowSize -> TBQueue Sequence -> Stream -> IO Next
fillStreamBodyGetNext Connection{connWriteBuffer,connBufferSize}
                      off lim sq strm = do
    let datBuf = connWriteBuffer `plusPtr` off
        room = min (connBufferSize - off) lim
    (leftover, cont, len) <- runStreamBuilder datBuf room sq
    return $ nextForStream connWriteBuffer connBufferSize sq strm leftover cont len

----------------------------------------------------------------

fillBufBuilder :: Buffer -> BufSize -> Leftover -> DynaNext
fillBufBuilder buf0 siz0 leftover lim = do
    let payloadBuf = buf0 `plusPtr` frameHeaderLength
        room = min (siz0 - frameHeaderLength) lim
    case leftover of
        LZero -> error "fillBufBuilder: LZero"
        LOne writer -> do
            (len, signal) <- writer payloadBuf room
            getNext len signal
        LTwo bs writer
          | BS.length bs <= room -> do
              buf1 <- copy payloadBuf bs
              let len1 = BS.length bs
              (len2, signal) <- writer buf1 (room - len1)
              getNext (len1 + len2) signal
          | otherwise -> do
              let (bs1,bs2) = BS.splitAt room bs
              void $ copy payloadBuf bs1
              getNext room (B.Chunk bs2 writer)
  where
    getNext l s = return $ nextForBuilder buf0 siz0 l s

nextForBuilder :: Buffer -> BufSize -> BytesFilled -> B.Next -> Next
nextForBuilder _   _   len B.Done
    = Next len Nothing
nextForBuilder buf siz len (B.More _ writer)
    = Next len $ Just (fillBufBuilder buf siz (LOne writer))
nextForBuilder buf siz len (B.Chunk bs writer)
    = Next len $ Just (fillBufBuilder buf siz (LTwo bs writer))

----------------------------------------------------------------

runStreamBuilder :: Buffer -> BufSize -> TBQueue Sequence
                 -> IO (Leftover, Bool, BytesFilled)
runStreamBuilder buf0 room0 sq = loop buf0 room0 0
  where
    loop !buf !room !total = do
        mbuilder <- atomically $ tryReadTBQueue sq
        case mbuilder of
            Nothing      -> return (LZero, True, total)
            Just (SBuilder builder) -> do
                (len, signal) <- B.runBuilder builder buf room
                let !total' = total + len
                case signal of
                    B.Done -> loop (buf `plusPtr` len) (room - len) total'
                    B.More  _ writer  -> return (LOne writer, True, total')
                    B.Chunk bs writer -> return (LTwo bs writer, True, total')
            Just SFlush  -> return (LZero, True, total)
            Just SFinish -> return (LZero, False, total)

fillBufStream :: Buffer -> BufSize -> Leftover -> TBQueue Sequence -> Stream -> DynaNext
fillBufStream buf0 siz0 leftover0 sq strm lim0 = do
    let payloadBuf = buf0 `plusPtr` frameHeaderLength
        room0 = min (siz0 - frameHeaderLength) lim0
    case leftover0 of
        LZero -> do
            (leftover, cont, len) <- runStreamBuilder payloadBuf room0 sq
            getNext leftover cont len
        LOne writer -> write writer payloadBuf room0 0
        LTwo bs writer
          | BS.length bs <= room0 -> do
              buf1 <- copy payloadBuf bs
              let len = BS.length bs
              write writer buf1 (room0 - len) len
          | otherwise -> do
              let (bs1,bs2) = BS.splitAt room0 bs
              void $ copy payloadBuf bs1
              getNext (LTwo bs2 writer) True room0
  where
    getNext l b r = return $ nextForStream buf0 siz0 sq strm l b r
    write writer1 buf room sofar = do
        (len, signal) <- writer1 buf room
        case signal of
            B.Done -> do
                (leftover, cont, extra) <- runStreamBuilder (buf `plusPtr` len) (room - len) sq
                let !total = sofar + len + extra
                getNext leftover cont total
            B.More  _ writer -> do
                let !total = sofar + len
                getNext (LOne writer) True total
            B.Chunk bs writer -> do
                let !total = sofar + len
                getNext (LTwo bs writer) True total

nextForStream :: Buffer -> BufSize -> TBQueue Sequence -> Stream
              -> Leftover -> Bool -> BytesFilled
              -> Next
nextForStream _ _ _ _ _ False len = Next len Nothing
nextForStream buf siz sq strm leftOrZero True len =
    Next len $ Just (fillBufStream buf siz leftOrZero sq strm)

----------------------------------------------------------------

#ifdef WINDOWS
fillBufFile :: Buffer -> BufSize -> IO.Handle -> Integer -> IO () -> DynaNext
fillBufFile buf siz h bytes refresh lim = do
    let payloadBuf = buf `plusPtr` frameHeaderLength
        room = min (siz - frameHeaderLength) lim
    len <- IO.hGetBufSome h payloadBuf room
    refresh
    let bytes' = bytes - fromIntegral len
    return $ nextForFile len buf siz h bytes' refresh

nextForFile :: BytesFilled -> Buffer -> BufSize -> IO.Handle -> Integer -> IO () -> Next
nextForFile 0   _   _   _  _    _       = Next 0   Nothing
nextForFile len _   _   _  0    _       = Next len Nothing
nextForFile len buf siz h bytes refresh =
    Next len $ Just (fillBufFile buf siz h bytes refresh)
#else
fillBufFile :: Buffer -> BufSize -> Fd -> Integer -> Integer -> IO () -> DynaNext
fillBufFile buf siz fd start bytes refresh lim = do
    let payloadBuf = buf `plusPtr` frameHeaderLength
        room = min (siz - frameHeaderLength) lim
    len <- positionRead fd payloadBuf (mini room bytes) start
    let len' = fromIntegral len
    refresh
    return $ nextForFile len buf siz fd (start + len') (bytes - len') refresh

nextForFile :: BytesFilled -> Buffer -> BufSize -> Fd -> Integer -> Integer -> IO () -> Next
nextForFile 0   _   _   _  _     _     _       = Next 0   Nothing
nextForFile len _   _   _  _     0     _       = Next len Nothing
nextForFile len buf siz fd start bytes refresh =
    Next len $ Just (fillBufFile buf siz fd start bytes refresh)
#endif

mini :: Int -> Integer -> Int
mini i n
  | fromIntegral i < n = i
  | otherwise          = fromIntegral n
