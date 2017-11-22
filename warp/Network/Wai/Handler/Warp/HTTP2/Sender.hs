{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns, CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.Wai.Handler.Warp.HTTP2.Sender (frameSender) where

import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Extra as B
import Data.IORef
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)
import Network.HPACK (setLimitForEncoding, toHeaderTable)
import Network.HTTP2
import Network.HTTP2.Priority (isEmptySTM, dequeueSTM, Precedence)
import Network.Wai

#ifdef WINDOWS
import qualified System.IO as IO
#else
import Network.Wai.Handler.Warp.FdCache
import Network.Wai.Handler.Warp.SendFile (positionRead)
import qualified Network.Wai.Handler.Warp.Timeout as T
#endif

import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.HTTP2.EncodeFrame
import Network.Wai.Handler.Warp.HTTP2.HPACK
import Network.Wai.Handler.Warp.HTTP2.Manager (Manager)
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.Imports hiding (readInt)
import qualified Network.Wai.Handler.Warp.Settings as S
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

data Leftover = LZero
              | LOne B.BufferWriter
              | LTwo ByteString B.BufferWriter

----------------------------------------------------------------

{-# INLINE getStreamWindowSize #-}
getStreamWindowSize :: Stream -> IO WindowSize
getStreamWindowSize Stream{streamWindow} = atomically $ readTVar streamWindow

{-# INLINE waitStreamWindowSize #-}
waitStreamWindowSize :: Stream -> IO ()
waitStreamWindowSize Stream{streamWindow} = atomically $ do
    w <- readTVar streamWindow
    check (w > 0)

{-# INLINE waitStreaming #-}
waitStreaming :: TBQueue a -> IO ()
waitStreaming tbq = atomically $ do
    isEmpty <- isEmptyTBQueue tbq
    check (not isEmpty)

data Switch = C Control
            | O (StreamId,Precedence,Output)
            | Flush

frameSender :: Context -> Connection -> S.Settings -> Manager -> IO ()
frameSender ctx@Context{outputQ,controlQ,connectionWindow,encodeDynamicTable}
            conn@Connection{connWriteBuffer,connBufferSize,connSendAll}
            settings mgr = loop 0 `E.catch` ignore
  where
    dequeue off = do
        isEmpty <- isEmptyTQueue controlQ
        if isEmpty then do
            w <- readTVar connectionWindow
            check (w > 0)
            emp <- isEmptySTM outputQ
            if emp then
                if off /= 0 then return Flush else retry
               else
                O <$> dequeueSTM outputQ
          else
            C <$> readTQueue controlQ

    loop off = do
        x <- atomically $ dequeue off
        case x of
            C ctl -> do
                when (off /= 0) $ flushN off
                off' <- control ctl off
                when (off' >= 0) $ loop off'
            O (_,pre,out) -> do
                let strm = outputStream out
                writeIORef (streamPrecedence strm) pre
                off' <- outputOrEnqueueAgain out off
                case off' of
                    0                -> loop 0
                    _ | off' > 15872 -> flushN off' >> loop 0 -- fixme: hard-coding
                      | otherwise    -> loop off'
            Flush -> flushN off >> loop 0

    control CFinish         _ = return (-1)
    control (CGoaway frame) _ = connSendAll frame >> return (-1)
    control (CFrame frame)  _ = connSendAll frame >> return 0
    control (CSettings frame alist) _ = do
        connSendAll frame
        setLimit alist
        return 0
    control (CSettings0 frame1 frame2 alist) off = do -- off == 0, just in case
        let !buf = connWriteBuffer `plusPtr` off
            !off' = off + BS.length frame1 + BS.length frame2
        buf' <- copy buf frame1
        void $ copy buf' frame2
        setLimit alist
        return off'

    {-# INLINE setLimit #-}
    setLimit alist = case lookup SettingsHeaderTableSize alist of
        Nothing  -> return ()
        Just siz -> setLimitForEncoding siz encodeDynamicTable

    output out@(Output strm _ _ tell getH2D (ONext curr)) off0 lim = do
        -- Data frame payload
        let !buf = connWriteBuffer `plusPtr` off0
            !siz = connBufferSize - off0
        Next datPayloadLen mnext <- curr buf siz lim
        off <- fillDataHeader strm off0 datPayloadLen mnext tell getH2D
        maybeEnqueueNext out mnext
        return off

    output out@(Output strm rspn ii tell getH2D ORspn) off0 lim = do
        -- Header frame and Continuation frame
        let !sid = streamNumber strm
            !endOfStream = case rspn of
                RspnNobody _ _ -> True
                _              -> False
        ths <- addNecessaryHeaders ctx rspn ii settings
        kvlen <- headerContinue sid ths endOfStream off0
        off <- sendHeadersIfNecessary $ off0 + frameHeaderLength + kvlen
        case rspn of
            RspnNobody _ _ -> do
                closed ctx strm Finished
                return off
            RspnFile _ _ path mpart -> do
                -- Data frame payload
                let payloadOff = off + frameHeaderLength
                Next datPayloadLen mnext <-
                    fillFileBodyGetNext conn ii payloadOff lim path mpart
                off' <- fillDataHeader strm off datPayloadLen mnext tell getH2D
                maybeEnqueueNext out mnext
                return off'
            RspnBuilder _ _ builder -> do
                -- Data frame payload
                let payloadOff = off + frameHeaderLength
                Next datPayloadLen mnext <-
                    fillBuilderBodyGetNext conn ii payloadOff lim builder
                off' <- fillDataHeader strm off datPayloadLen mnext tell getH2D
                maybeEnqueueNext out mnext
                return off'
            RspnStreaming _ _ tbq -> do
                let payloadOff = off + frameHeaderLength
                Next datPayloadLen mnext <-
                    fillStreamBodyGetNext conn payloadOff lim tbq strm
                off' <- fillDataHeader strm off datPayloadLen mnext tell getH2D
                maybeEnqueueNext out mnext
                return off'

    output out@(Output strm _ _ _ _ (OPush ths pid)) off0 lim = do
        -- Creating a push promise header
        -- Frame id should be associated stream id from the client.
        let !sid = streamNumber strm
        len <- pushPromise pid sid ths off0
        off <- sendHeadersIfNecessary $ off0 + frameHeaderLength + len
        output out{ outputType = ORspn }  off lim

    output _ _ _ = undefined -- never reach

    outputOrEnqueueAgain out off = E.handle resetStream $ do
        state <- readIORef $ streamState strm
        if isClosed state then
            return off
          else case out of
                 Output _ _ _ wait _ OWait -> do
                     -- Checking if all push are done.
                     let out' = out {
                             outputHook = return ()
                           , outputType = ORspn
                           }
                     forkAndEnqueueWhenReady wait outputQ out' mgr
                     return off
                 _ -> case mtbq of
                        Just tbq -> checkStreaming tbq
                        _        -> checkStreamWindowSize
      where
        strm = outputStream out
        mtbq = outputMaybeTBQueue out
        checkStreaming tbq = do
            isEmpty <- atomically $ isEmptyTBQueue tbq
            if isEmpty then do
                forkAndEnqueueWhenReady (waitStreaming tbq) outputQ out mgr
                return off
              else
                checkStreamWindowSize
        checkStreamWindowSize = do
            sws <- getStreamWindowSize strm
            if sws == 0 then do
                forkAndEnqueueWhenReady (waitStreamWindowSize strm) outputQ out mgr
                return off
              else do
                cws <- atomically $ readTVar connectionWindow -- not 0
                let !lim = min cws sws
                output out off lim
        resetStream e = do
            closed ctx strm (ResetByMe e)
            let !rst = resetFrame InternalError $ streamNumber strm
            enqueueControl controlQ $ CFrame rst
            return off

    {-# INLINE flushN #-}
    -- Flush the connection buffer to the socket, where the first 'n' bytes of
    -- the buffer are filled.
    flushN :: Int -> IO ()
    flushN n = bufferIO connWriteBuffer n connSendAll

    headerContinue sid ths endOfStream off = do
        let !offkv = off + frameHeaderLength
        let !bufkv = connWriteBuffer `plusPtr` offkv
            !limkv = connBufferSize - offkv
        (hs,kvlen) <- hpackEncodeHeader ctx bufkv limkv ths
        let flag0 = case hs of
                [] -> setEndHeader defaultFlags
                _  -> defaultFlags
            flag = if endOfStream then setEndStream flag0 else flag0
        let buf = connWriteBuffer `plusPtr` off
        fillFrameHeader FrameHeaders kvlen sid flag buf
        continue sid kvlen hs

    !bufHeaderPayload = connWriteBuffer `plusPtr` frameHeaderLength
    !headerPayloadLim = connBufferSize - frameHeaderLength

    continue _   kvlen [] = return kvlen
    continue sid kvlen ths = do
        flushN $ kvlen + frameHeaderLength
        -- Now off is 0
        (ths', kvlen') <- hpackEncodeHeaderLoop ctx bufHeaderPayload headerPayloadLim ths
        when (ths == ths') $ E.throwIO $ ConnectionError CompressionError "cannot compress the header"
        let flag = case ths' of
                [] -> setEndHeader defaultFlags
                _  -> defaultFlags
        fillFrameHeader FrameContinuation kvlen' sid flag connWriteBuffer
        continue sid kvlen' ths'

    {-# INLINE maybeEnqueueNext #-}
    -- Re-enqueue the stream in the output queue.
    maybeEnqueueNext :: Output -> Maybe DynaNext -> IO ()
    maybeEnqueueNext _   Nothing     = return ()
    maybeEnqueueNext out (Just next) = enqueueOutput outputQ out'
      where
        !out' = out { outputType = ONext next }

    {-# INLINE sendHeadersIfNecessary #-}
    -- Send headers if there is not room for a 1-byte data frame, and return
    -- the offset of the next frame's first header byte.
    sendHeadersIfNecessary off
      -- True if the connection buffer has room for a 1-byte data frame.
      | off + frameHeaderLength < connBufferSize = return off
      | otherwise = do
          flushN off
          return 0

    fillDataHeader strm@Stream{streamWindow,streamNumber}
                   off datPayloadLen mnext tell getH2D = do
        -- Data frame header
        mh2d <- getH2D
        let (!trailers,!noTrailers) = case http2dataTrailers <$> mh2d of
              Nothing -> ([], True)
              Just ts -> (ts, null ts)
            !buf = connWriteBuffer `plusPtr` off
            !off' = off + frameHeaderLength + datPayloadLen
            !noMoreBody = isNothing mnext
            flag | noMoreBody && noTrailers = setEndStream defaultFlags
                 | otherwise                = defaultFlags
        fillFrameHeader FrameData datPayloadLen streamNumber flag buf
        off'' <- handleEndOfBody noMoreBody off' noTrailers trailers
        atomically $ modifyTVar' connectionWindow (subtract datPayloadLen)
        atomically $ modifyTVar' streamWindow (subtract datPayloadLen)
        return off''
      where
        handleTrailers True off0 _        = return off0
        handleTrailers _    off0 trailers = do
            (ths,_) <- toHeaderTable trailers
            kvlen <- headerContinue streamNumber ths True off0
            sendHeadersIfNecessary $ off0 + frameHeaderLength + kvlen
        handleEndOfBody True off0 noTrailers trailers = do
            off1 <- handleTrailers noTrailers off0 trailers
            void tell
            closed ctx strm Finished
            return off1
        handleEndOfBody False off0 _ _ = return off0


    pushPromise pid sid ths off = do
        let !offsid = off + frameHeaderLength
            !bufsid = connWriteBuffer `plusPtr` offsid
        poke32 bufsid $ fromIntegral sid
        let !offkv  = offsid + 4
            !bufkv  = connWriteBuffer `plusPtr` offkv
            !limkv  = connBufferSize - offkv
        (_,kvlen) <- hpackEncodeHeader ctx bufkv limkv ths
        let !flag = setEndHeader defaultFlags -- No EndStream flag
            !buf = connWriteBuffer `plusPtr` off
            !len = kvlen + 4
        fillFrameHeader FramePushPromise len pid flag buf
        return len

    {-# INLINE fillFrameHeader #-}
    fillFrameHeader ftyp len sid flag buf = encodeFrameHeaderBuf ftyp hinfo buf
      where
        hinfo = FrameHeader len flag sid

    {-# INLINE ignore #-}
    ignore :: E.SomeException -> IO ()
    ignore _ = return ()

----------------------------------------------------------------

{-
ResponseFile Status ResponseHeaders FilePath (Maybe FilePart)
ResponseBuilder Status ResponseHeaders Builder
ResponseStream Status ResponseHeaders StreamingBody
ResponseRaw (IO ByteString -> (ByteString -> IO ()) -> IO ()) Response
-}

fillBuilderBodyGetNext :: Connection -> InternalInfo -> Int -> WindowSize -> Builder -> IO Next
fillBuilderBodyGetNext Connection{connWriteBuffer,connBufferSize}
                        _ off lim bb = do
    let datBuf = connWriteBuffer `plusPtr` off
        room = min (connBufferSize - off) lim
    (len, signal) <- B.runBuilder bb datBuf room
    return $ nextForBuilder len signal

fillFileBodyGetNext :: Connection -> InternalInfo -> Int -> WindowSize -> FilePath -> Maybe FilePart -> IO Next
#ifdef WINDOWS
fillFileBodyGetNext Connection{connWriteBuffer,connBufferSize}
                        _ off lim path mpart = do
    let datBuf = connWriteBuffer `plusPtr` off
        room = min (connBufferSize - off) lim
    (start, bytes) <- fileStartEnd path mpart
    -- fixme: how to close Handle? GC does it at this moment.
    hdl <- IO.openBinaryFile path IO.ReadMode
    IO.hSeek hdl IO.AbsoluteSeek start
    len <- IO.hGetBufSome hdl datBuf (mini room bytes)
    let bytes' = bytes - fromIntegral len
    -- fixme: connWriteBuffer connBufferSize
    return $ nextForFile len hdl bytes' (return ())
#else
fillFileBodyGetNext Connection{connWriteBuffer,connBufferSize}
                        ii off lim path mpart = do
    (mfd, refresh') <- getFd ii path
    (fd, refresh) <- case mfd of
        Nothing -> do
            fd' <- openFile path
            th <- T.register (timeoutManager ii) (closeFile fd')
            return (fd', T.tickle th)
        Just fd  -> return (fd, refresh')
    let datBuf = connWriteBuffer `plusPtr` off
        room = min (connBufferSize - off) lim
    (start, bytes) <- fileStartEnd path mpart
    len <- positionRead fd datBuf (mini room bytes) start
    refresh
    let len' = fromIntegral len
    return $ nextForFile len fd (start + len') (bytes - len') refresh
#endif

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
    return $ nextForStream sq strm leftover cont len

----------------------------------------------------------------

fillBufBuilder :: Leftover -> DynaNext
fillBufBuilder leftover buf0 siz0 lim = do
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
    getNext l s = return $ nextForBuilder l s

nextForBuilder :: BytesFilled -> B.Next -> Next
nextForBuilder len B.Done
    = Next len Nothing
nextForBuilder len (B.More _ writer)
    = Next len $ Just (fillBufBuilder (LOne writer))
nextForBuilder len (B.Chunk bs writer)
    = Next len $ Just (fillBufBuilder (LTwo bs writer))

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

fillBufStream :: Leftover -> TBQueue Sequence -> Stream -> DynaNext
fillBufStream leftover0 sq strm buf0 siz0 lim0 = do
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
    getNext l b r = return $ nextForStream sq strm l b r
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

nextForStream :: TBQueue Sequence -> Stream
              -> Leftover -> Bool -> BytesFilled
              -> Next
nextForStream _ _ _ False len = Next len Nothing
nextForStream sq strm leftOrZero True len =
    Next len $ Just (fillBufStream leftOrZero sq strm)

----------------------------------------------------------------

#ifdef WINDOWS
fillBufFile :: IO.Handle -> Integer -> IO () -> DynaNext
fillBufFile h bytes refresh buf siz lim = do
    let payloadBuf = buf `plusPtr` frameHeaderLength
        room = min (siz - frameHeaderLength) lim
    len <- IO.hGetBufSome h payloadBuf room
    refresh
    let bytes' = bytes - fromIntegral len
    return $ nextForFile len h bytes' refresh

nextForFile :: BytesFilled -> IO.Handle -> Integer -> IO () -> Next
nextForFile 0   _ _     _       = Next 0   Nothing
nextForFile len _ 0     _       = Next len Nothing
nextForFile len h bytes refresh =
    Next len $ Just (fillBufFile h bytes refresh)
#else
fillBufFile :: Fd -> Integer -> Integer -> IO () -> DynaNext
fillBufFile fd start bytes refresh buf siz lim = do
    let payloadBuf = buf `plusPtr` frameHeaderLength
        room = min (siz - frameHeaderLength) lim
    len <- positionRead fd payloadBuf (mini room bytes) start
    let len' = fromIntegral len
    refresh
    return $ nextForFile len fd (start + len') (bytes - len') refresh

nextForFile :: BytesFilled -> Fd -> Integer -> Integer -> IO () -> Next
nextForFile 0   _  _     _     _       = Next 0   Nothing
nextForFile len _  _     0     _       = Next len Nothing
nextForFile len fd start bytes refresh =
    Next len $ Just (fillBufFile fd start bytes refresh)
#endif

{-# INLINE mini #-}
mini :: Int -> Integer -> Int
mini i n
  | fromIntegral i < n = i
  | otherwise          = fromIntegral n


----------------------------------------------------------------

poke32 :: Ptr Word8 -> Word32 -> IO ()
poke32 ptr i = do
    poke ptr w0
    poke8 ptr 1 w1
    poke8 ptr 2 w2
    poke8 ptr 3 w3
  where
    w0 = fromIntegral ((i `shiftR` 24) .&. 0xff)
    w1 = fromIntegral ((i `shiftR` 16) .&. 0xff)
    w2 = fromIntegral ((i `shiftR`  8) .&. 0xff)
    w3 = fromIntegral  (i              .&. 0xff)
    poke8 :: Ptr Word8 -> Int -> Word8 -> IO ()
    poke8 ptr0 n w = poke (ptr0 `plusPtr` n) w
