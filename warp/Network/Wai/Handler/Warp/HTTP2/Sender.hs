{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns, CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.Wai.Handler.Warp.HTTP2.Sender (frameSender) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Concurrent.MVar (putMVar)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B (int32BE)
import qualified Data.ByteString.Builder.Extra as B
import Data.Monoid ((<>))
import Foreign.Ptr
import qualified Network.HTTP.Types as H
import Network.HPACK (setLimitForEncoding)
import Network.HTTP2
import Network.HTTP2.Priority
import Network.Wai (FilePart(..))
import Network.Wai.HTTP2 (Trailers, promiseHeaders)
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.HTTP2.EncodeFrame
import Network.Wai.Handler.Warp.HTTP2.HPACK
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.IORef
import qualified Network.Wai.Handler.Warp.Settings as S
import Network.Wai.Handler.Warp.Types

#ifdef WINDOWS
import qualified System.IO as IO
#else
import Network.Wai.Handler.Warp.FdCache (getFd)
import Network.Wai.Handler.Warp.SendFile (positionRead)
import qualified Network.Wai.Handler.Warp.Timeout as T
import System.Posix.IO (openFd, OpenFileFlags(..), defaultFileFlags, OpenMode(ReadOnly), closeFd)
import System.Posix.Types (Fd)
#endif

----------------------------------------------------------------

-- | The platform-specific type of an open file to stream from.  On Windows we
-- don't have pread, so this is just a Handle; on Unix platforms with pread,
-- this is a file descriptor supplied by the fd cache.
#ifdef WINDOWS
type OpenFile = IO.Handle
#else
type OpenFile = Fd
#endif

data Leftover = LZero
              | LOne B.BufferWriter
              | LTwo BS.ByteString B.BufferWriter
              | LFile OpenFile Integer Integer (IO ())

----------------------------------------------------------------

-- | Run the given action if the stream is not closed; handle any exceptions by
-- resetting the stream.
unlessClosed :: Connection -> Stream -> IO () -> IO Bool
unlessClosed Connection{connSendAll}
             strm@Stream{streamState,streamNumber}
             body = E.handle resetStream $ do
    state <- readIORef streamState
    if (isClosed state) then return False else body >> return True
  where
    resetStream e = do
        closed strm (ResetByMe e)
        let rst = resetFrame InternalError streamNumber
        connSendAll rst
        return False

getWindowSize :: TVar WindowSize -> TVar WindowSize -> IO WindowSize
getWindowSize connWindow strmWindow = do
   -- Waiting that the connection window gets open.
   cw <- atomically $ do
       w <- readTVar connWindow
       check (w > 0)
       return w
   -- This stream window is greater than 0 thanks to the invariant.
   sw <- atomically $ readTVar strmWindow
   return $ min cw sw

frameSender :: Context -> Connection -> InternalInfo -> S.Settings -> IO ()
frameSender ctx@Context{outputQ,connectionWindow,encodeDynamicTable}
            conn@Connection{connWriteBuffer,connBufferSize,connSendAll}
            ii settings = go `E.catch` ignore
  where
    initialSettings = [(SettingsMaxConcurrentStreams,recommendedConcurrency)]
    initialFrame = settingsFrame id initialSettings
    bufHeaderPayload = connWriteBuffer `plusPtr` frameHeaderLength
    headerPayloadLim = connBufferSize - frameHeaderLength

    go = do
        connSendAll initialFrame
        loop

    -- ignoring the old priority because the value might be changed.
    loop = dequeue outputQ >>= \(_sid,pre,out) -> switch out pre

    ignore :: E.SomeException -> IO ()
    ignore _ = return ()

    switch OFinish         _ = return ()
    switch (OGoaway frame) _ = connSendAll frame
    switch (OSettings frame alist) _ = do
        connSendAll frame
        case lookup SettingsHeaderTableSize alist of
            Nothing  -> return ()
            Just siz -> do
                dyntbl <- readIORef encodeDynamicTable
                setLimitForEncoding siz dyntbl
        loop
    switch (OFrame frame)  _ = do
        connSendAll frame
        loop
    switch (OResponse strm s h aux) pre = do
        writeIORef (streamPrecedence strm) pre -- fixme
        _ <- unlessClosed conn strm $
            getWindowSize connectionWindow (streamWindow strm) >>=
                sendResponse strm s h aux
        loop
    switch (ONext strm curr) pre = do
        writeIORef (streamPrecedence strm) pre
        _ <- unlessClosed conn strm $ do
            lim <- getWindowSize connectionWindow (streamWindow strm)
            -- Data frame payload
            Next datPayloadLen mnext <- curr lim
            fillDataHeaderSend strm 0 datPayloadLen mnext
            dispatchNext strm mnext
        loop
    switch (OPush oldStrm push mvar strm s h aux) pre = do
        writeIORef (streamPrecedence strm) pre -- fixme
        pushed <- unlessClosed conn oldStrm $ do
            lim <- getWindowSize connectionWindow (streamWindow strm)
            -- Write and send the promise.
            builder <- hpackEncodeCIHeaders ctx $ promiseHeaders push
            off <- pushContinue (streamNumber oldStrm) (streamNumber strm) builder
            flushN $ off + frameHeaderLength
            -- TODO(awpr): refactor sendResponse to be able to handle non-zero
            -- initial offsets and use that to potentially avoid the extra syscall.
            sendResponse strm s h aux lim
        putMVar mvar pushed
        loop

    -- Send the response headers and as much of the response as is immediately
    -- available; shared by normal responses and pushed streams.
    sendResponse :: Stream -> H.Status -> H.ResponseHeaders -> Aux -> WindowSize -> IO ()
    sendResponse strm s h (Persist sq tvar) lim = do
        -- Header frame and Continuation frame
        let sid = streamNumber strm
        builder <- hpackEncodeHeader ctx ii settings s h
        len <- headerContinue sid builder False
        let total = len + frameHeaderLength
        (off, needSend) <- sendHeadersIfNecessary total
        let payloadOff = off + frameHeaderLength
        Next datPayloadLen mnext <-
            fillStreamBodyGetNext ii conn payloadOff lim sq tvar strm
        -- If no data was immediately available, avoid sending an
        -- empty data frame.
        if datPayloadLen > 0 then
            fillDataHeaderSend strm total datPayloadLen mnext
          else
            when needSend $ flushN off
        dispatchNext strm mnext

    -- Send the stream's trailers and close the stream.
    sendTrailers :: Stream -> Trailers -> IO ()
    sendTrailers strm [] = closed strm Finished
    sendTrailers strm trailers = do
        builder <- hpackEncodeCIHeaders ctx trailers
        off <- headerContinue (streamNumber strm) builder True
        let !toFlush = off + frameHeaderLength
        -- 'closed' must be before 'flushN'. If not, the context would be
        -- switched to the receiver, resulting in the inconsistency of
        -- concurrency.
        closed strm Finished
        flushN toFlush

    -- Flush the connection buffer to the socket, where the first 'n' bytes of
    -- the buffer are filled.
    flushN :: Int -> IO ()
    flushN n = bufferIO connWriteBuffer n connSendAll

    -- A flags value with the end-header flag set iff the argument is B.Done.
    maybeEndHeaders B.Done = setEndHeader defaultFlags
    maybeEndHeaders _      = defaultFlags

    -- Write PUSH_PROMISE and possibly CONTINUATION frames into the connection
    -- buffer, using the given builder as their contents; flush them to the
    -- socket as necessary.
    pushContinue sid newSid builder = do
        let builder' = B.int32BE (fromIntegral newSid) <> builder
        (len, signal) <- B.runBuilder builder' bufHeaderPayload headerPayloadLim
        let flag = maybeEndHeaders signal
        fillFrameHeader FramePushPromise len sid flag connWriteBuffer
        continue sid len signal

    -- Write HEADER and possibly CONTINUATION frames.
    headerContinue sid builder endOfStream = do
        (len, signal) <- B.runBuilder builder bufHeaderPayload headerPayloadLim
        let flag0 = maybeEndHeaders signal
            flag = if endOfStream then setEndStream flag0 else flag0
        fillFrameHeader FrameHeaders len sid flag connWriteBuffer
        continue sid len signal

    continue _   len B.Done = return len
    continue sid len (B.More _ writer) = do
        flushN $ len + frameHeaderLength
        (len', signal') <- writer bufHeaderPayload headerPayloadLim
        let flag = maybeEndHeaders signal'
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

    -- Take the appropriate action based on the given 'Control':
    -- - If more output is immediately available, re-enqueue the stream in the
    -- output queue.
    -- - If the output is over and trailers are available, send them now and
    -- end the stream.
    -- - If we've drained the queue and handed the stream back to its waiter,
    -- do nothing.
    --
    -- This is done after sending any part of the stream body, so it's shared
    -- by 'sendResponse' and @switch (ONext ...)@.
    dispatchNext :: Stream -> Control DynaNext -> IO ()
    dispatchNext _    CNone              = return ()
    dispatchNext strm (CFinish trailers) = sendTrailers strm trailers
    dispatchNext strm (CNext next)       = do
        let out = ONext strm next
        enqueueOrSpawnTemporaryWaiter strm outputQ out


    -- Send headers if there is not room for a 1-byte data frame, and return
    -- the offset of the next frame's first header byte and whether the headers
    -- still need to be sent.
    sendHeadersIfNecessary total
      | canFitDataFrame total = return (total, True)
      | otherwise             = do
          flushN total
          return (0, False)

    fillDataHeaderSend strm otherLen datPayloadLen cnext = do
        let flag = case cnext of
                CFinish [] -> setEndStream defaultFlags
                _          -> defaultFlags
        -- Data frame header
        let sid = streamNumber strm
            buf = connWriteBuffer `plusPtr` otherLen
            total = otherLen + frameHeaderLength + datPayloadLen
        fillFrameHeader FrameData datPayloadLen sid flag buf
        flushN total
        atomically $ do
           modifyTVar' connectionWindow (subtract datPayloadLen)
           modifyTVar' (streamWindow strm) (subtract datPayloadLen)

    fillFrameHeader ftyp len sid flag buf = encodeFrameHeaderBuf ftyp hinfo buf
      where
        hinfo = FrameHeader len flag sid

----------------------------------------------------------------

fillStreamBodyGetNext :: InternalInfo -> Connection -> Int -> WindowSize
                      -> TBQueue Sequence -> TVar Sync -> Stream -> IO Next
fillStreamBodyGetNext ii Connection{connWriteBuffer,connBufferSize}
                      off lim sq tvar strm = do
    let datBuf = connWriteBuffer `plusPtr` off
        room = min (connBufferSize - off) lim
    (leftover, cont, len) <- runStreamBuilder ii datBuf room sq
    nextForStream ii connWriteBuffer connBufferSize sq tvar strm leftover cont len

----------------------------------------------------------------

runStreamBuilder :: InternalInfo -> Buffer -> BufSize -> TBQueue Sequence
                 -> IO (Leftover, Maybe Trailers, BytesFilled)
runStreamBuilder ii buf0 room0 sq = loop buf0 room0 0
  where
    loop !buf !room !total = do
        mbuilder <- atomically $ tryReadTBQueue sq
        case mbuilder of
            Nothing      -> return (LZero, Nothing, total)
            Just (SBuilder builder) -> do
                (len, signal) <- B.runBuilder builder buf room
                let !total' = total + len
                case signal of
                    B.Done -> loop (buf `plusPtr` len) (room - len) total'
                    B.More  _ writer  -> return (LOne writer, Nothing, total')
                    B.Chunk bs writer -> return (LTwo bs writer, Nothing, total')
            Just (SFile path part) -> do
                (leftover, len) <- runStreamFile ii buf room path part
                let !total' = total + len
                case leftover of
                    LZero -> loop (buf `plusPtr` len) (room - len) total'
                    _     -> return (leftover, Nothing, total')

            Just SFlush  -> return (LZero, Nothing, total)
            Just (SFinish trailers) -> return (LZero, Just trailers, total)

-- | Open the file and start reading into the send buffer.
runStreamFile :: InternalInfo -> Buffer -> BufSize -> FilePath -> FilePart
              -> IO (Leftover, BytesFilled)

-- | Read the given (OS-specific) file representation into the buffer.  On
-- non-Windows systems this uses pread; on Windows this ignores the position
-- because we use the Handle's internal read position instead (because it's not
-- potentially shared with other readers).
readOpenFile :: OpenFile -> Buffer -> BufSize -> Integer -> IO Int

#ifdef WINDOWS
runStreamFile _ buf room path part = do
    let start = filePartOffset part
        bytes = filePartByteCount part
    -- fixme: how to close Handle? GC does it at this moment.
    h <- IO.openBinaryFile path IO.ReadMode
    IO.hSeek h IO.AbsoluteSeek start
    fillBufFile buf room h start bytes (return ())

readOpenFile h buf room _ = IO.hGetBufSome h buf room
#else
runStreamFile ii buf room path part = do
    let start = filePartOffset part
        bytes = filePartByteCount part
    (fd, refresh) <- case fdCacher ii of
        Just fdcache -> getFd fdcache path
        Nothing      -> do
            fd' <- openFd path ReadOnly Nothing defaultFileFlags{nonBlock=True}
            th <- T.register (timeoutManager ii) (closeFd fd')
            return (fd', T.tickle th)
    fillBufFile buf room fd start bytes refresh

readOpenFile = positionRead
#endif

-- | Read as much of the file as is currently available into the buffer, then
-- return a 'Leftover' to indicate whether this file chunk has more data to
-- send.  If this read hit the end of the file range, return 'LZero'; otherwise
-- return 'LFile' so this stream will continue reading from the file the next
-- time it's pulled from the queue.
fillBufFile :: Buffer -> BufSize -> OpenFile -> Integer -> Integer -> (IO ())
            -> IO (Leftover, BytesFilled)
fillBufFile buf room f start bytes refresh = do
    len <- readOpenFile f buf (mini room bytes) start
    refresh
    let len' = fromIntegral len
        leftover = if bytes > len' then
            LFile f (start + len') (bytes - len') refresh
          else
            LZero
    return (leftover, len)

mini :: Int -> Integer -> Int
mini i n
  | fromIntegral i < n = i
  | otherwise          = fromIntegral n

fillBufStream :: InternalInfo -> Buffer -> BufSize -> Leftover
              -> TBQueue Sequence -> TVar Sync -> Stream -> DynaNext
fillBufStream ii buf0 siz0 leftover0 sq tvar strm lim0 = do
    let payloadBuf = buf0 `plusPtr` frameHeaderLength
        room0 = min (siz0 - frameHeaderLength) lim0
    case leftover0 of
        LZero -> do
            (leftover, end, len) <- runStreamBuilder ii payloadBuf room0 sq
            getNext leftover end len
        LOne writer -> write writer payloadBuf room0 0
        LTwo bs writer
          | BS.length bs <= room0 -> do
              buf1 <- copy payloadBuf bs
              let len = BS.length bs
              write writer buf1 (room0 - len) len
          | otherwise -> do
              let (bs1,bs2) = BS.splitAt room0 bs
              void $ copy payloadBuf bs1
              getNext (LTwo bs2 writer) Nothing room0
        LFile fd start bytes refresh -> do
            (leftover, len) <- fillBufFile payloadBuf room0 fd start bytes refresh
            getNext leftover Nothing len
  where
    getNext = nextForStream ii buf0 siz0 sq tvar strm
    write writer1 buf room sofar = do
        (len, signal) <- writer1 buf room
        case signal of
            B.Done -> do
                (leftover, end, extra) <- runStreamBuilder ii (buf `plusPtr` len) (room - len) sq
                let !total = sofar + len + extra
                getNext leftover end total
            B.More  _ writer -> do
                let !total = sofar + len
                getNext (LOne writer) Nothing total
            B.Chunk bs writer -> do
                let !total = sofar + len
                getNext (LTwo bs writer) Nothing total

nextForStream :: InternalInfo -> Buffer -> BufSize -> TBQueue Sequence
              -> TVar Sync -> Stream -> Leftover -> Maybe Trailers
              -> BytesFilled -> IO Next
nextForStream _ _  _ _  tvar _ _ (Just trailers) len = do
    atomically $ writeTVar tvar $ SyncFinish
    return $ Next len $ CFinish trailers
nextForStream ii buf siz sq tvar strm LZero Nothing len = do
    let out = ONext strm (fillBufStream ii buf siz LZero sq tvar strm)
    atomically $ writeTVar tvar $ SyncNext out
    return $ Next len CNone
nextForStream ii buf siz sq tvar strm leftover Nothing len =
    return $ Next len (CNext (fillBufStream ii buf siz leftover sq tvar strm))
