{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Conduit where

import Control.Exception (assert, throwIO)
import qualified Data.ByteString as S
import qualified Data.IORef as I
import Data.Word8 (_0, _9, _A, _F, _a, _cr, _f, _lf, _semicolon)

import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

-- | Contains a @Source@ and a byte count that is still to be read in.
data ISource = ISource !Source !(I.IORef Int)

mkISource :: Source -> Int -> IO ISource
mkISource src cnt = do
    ref <- I.newIORef cnt
    return $! ISource src ref

-- | Given an @IsolatedBSSource@ provide a @Source@ that only allows up to the
-- specified number of bytes to be passed downstream. All leftovers should be
-- retained within the @Source@. If there are not enough bytes available,
-- throws a @ConnectionClosedByPeer@ exception.
readISource :: ISource -> IO ByteString
readISource (ISource src ref) = do
    count <- I.readIORef ref
    if count == 0
        then return S.empty
        else do
            bs <- readSource src

            -- If no chunk available, then there aren't enough bytes in the
            -- stream. Throw a ConnectionClosedByPeer
            when (S.null bs) $ throwIO ConnectionClosedByPeer

            let -- How many of the bytes in this chunk to send downstream
                toSend = min count (S.length bs)
                -- How many bytes will still remain to be sent downstream
                count' = count - toSend

            -- [WRITE_IOREF_NOTE]
            -- This doesn't need to be "atomic", since it is only used in
            -- 'recvRequest', which creates the 'Source' and doesn't fork it,
            -- so the 'IORef' is not shared outside of 'recvRequest'.
            I.writeIORef ref count'

            if count' > 0
                then -- The expected count is greater than the size of the
                -- chunk we just read. Send the entire chunk
                -- downstream, and then loop on this function for the
                -- next chunk.
                    return bs
                else do
                    -- Some of the bytes in this chunk should not be sent
                    -- downstream. Split up the chunk into the sent and
                    -- not-sent parts, add the not-sent parts onto the new
                    -- source, and send the rest of the chunk downstream.
                    let (x, y) = S.splitAt toSend bs
                    leftoverSource src y
                    assert (count' == 0) $ return x

----------------------------------------------------------------

data CSource = CSource !Source !(I.IORef ChunkState)

data ChunkState
    = NeedLen
    | NeedLenNewline
    | HaveLen Word
    | DoneChunking
    deriving (Show)

mkCSource :: Source -> IO CSource
mkCSource src = do
    ref <- I.newIORef NeedLen
    return $! CSource src ref

-- | A chunk-size line is a few bytes of hex, and chunk extensions are
--   all but unused; a trailer section is a handful of fields. Neither is
--   a reason to hold on to everything a client cares to send.
maxChunkLineLength :: Int
maxChunkLineLength = 1024

maxTrailerLength :: Int
maxTrailerLength = 8192

readCSource :: CSource -> IO ByteString
readCSource (CSource src ref) = do
    mlen <- I.readIORef ref
    go mlen
  where
    go NeedLen = getLen
    go NeedLenNewline = dropCRLF >> getLen
    go (HaveLen len) = readSource src >>= withLen len
    go DoneChunking = return S.empty

    getLen = do
        len <- chunkSize =<< requireLine
        if len == 0
            then do
                -- Everything up to the empty line which ends the trailer
                -- section belongs to this request. Warp used to drop one
                -- CRLF and leave the trailers in the source, where they
                -- were read as the next request on the connection.
                dropTrailers 0
                yield' S.empty DoneChunking
            else readSource src >>= withLen len

    withLen len bs
        -- The chunk promised more than the client went on to send.
        | S.null bs = throwIO ConnectionClosedByPeer
        | otherwise =
            case S.length bs `compare` fromIntegral len of
                EQ -> yield' bs NeedLenNewline
                LT -> yield' bs $ HaveLen $ len - fromIntegral (S.length bs)
                GT -> do
                    let (x, y) = S.splitAt (fromIntegral len) bs
                    leftoverSource src y
                    yield' x NeedLenNewline

    yield' bs mlen = do
        I.writeIORef ref mlen -- [WRITE_IOREF_NOTE]
        return bs

    -- A chunk size is 1*HEXDIG, with nothing after it but chunk
    -- extensions. Warp used to fold whatever hex digits it could find
    -- and take 0 for the rest, so a line which is no chunk size at all
    -- -- a request line, say -- ended the body and left the rest of it
    -- to be read as the next request.
    chunkSize line
        | S.null digits = throwIO BadChunkedBody
        | not (S.null ext), S.head ext /= _semicolon = throwIO BadChunkedBody
        | otherwise =
            maybe (throwIO BadChunkedBody) (return . fromIntegral) $
                S.foldl' accumulate (Just (0 :: Int)) digits
      where
        (digits, ext) = S.span isHexDigit line
        accumulate Nothing _ = Nothing
        -- A chunk warp cannot measure is a chunk warp cannot read.
        accumulate (Just n) w
            | n > (maxBound - d) `div` 16 = Nothing
            | otherwise = Just $! n * 16 + d
          where
            d = fromIntegral (hexToWord w)

    -- The CRLF which ends chunk-data. Warp used to put anything else
    -- back and read it as the next chunk size, so a chunk that was not
    -- closed off ended the body early.
    dropCRLF = do
        line <- requireLine
        unless (S.null line) $ throwIO BadChunkedBody

    dropTrailers total = do
        mline <- readLine
        case mline of
            -- A client which leaves out the last CRLF and closes has
            -- nothing left to hide behind it.
            Nothing -> return ()
            Just line -> unless (S.null line) $ do
                let total' = total + S.length line
                when (total' > maxTrailerLength) $ throwIO OverLargeHeader
                dropTrailers total'

    requireLine = readLine >>= maybe (throwIO ConnectionClosedByPeer) return

    -- One line of the chunked framing: up to the next LF, without it or
    -- the CR before it, leaving the rest of what was read in the source.
    -- 'Nothing' when the connection ends where a line was to begin.
    -- Warp used to give up one read short of the LF, which mis-read
    -- every size line that arrived in more than two pieces.
    readLine = loop 0 id
      where
        loop total front = do
            bs <- readSource src
            if S.null bs
                then
                    if total == 0
                        then return Nothing
                        else throwIO ConnectionClosedByPeer
                else case S.elemIndex _lf bs of
                    Nothing -> do
                        let total' = total + S.length bs
                        when (total' > maxChunkLineLength) $ throwIO OverLargeHeader
                        loop total' (front . (bs :))
                    Just ix -> do
                        leftoverSource src $ S.drop (ix + 1) bs
                        let line = S.concat $ front [S.take ix bs]
                        when (S.length line > maxChunkLineLength) $
                            throwIO OverLargeHeader
                        return $ Just $ dropCR line
        dropCR bs = case S.unsnoc bs of
            Just (bs', w) | w == _cr -> bs'
            _ -> bs

    hexToWord w
        | w <= _9 = w - _0
        | w <= _F = w - 55
        | otherwise = w - 87

isHexDigit :: Word8 -> Bool
isHexDigit w =
    w >= _0 && w <= _9
        || w >= _A && w <= _F
        || w >= _a && w <= _f
