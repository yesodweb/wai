{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Conduit where

import Control.Exception
import qualified Data.ByteString as S
import qualified Data.IORef as I

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
        case () of
            ()
                -- The expected count is greater than the size of the
                -- chunk we just read. Send the entire chunk
                -- downstream, and then loop on this function for the
                -- next chunk.
                | count' > 0 -> do
                    I.writeIORef ref count'
                    return bs

                -- Some of the bytes in this chunk should not be sent
                -- downstream. Split up the chunk into the sent and
                -- not-sent parts, add the not-sent parts onto the new
                -- source, and send the rest of the chunk downstream.
                | otherwise -> do
                    let (x, y) = S.splitAt toSend bs
                    leftoverSource src y
                    assert (count' == 0) $ I.writeIORef ref count'
                    return x

----------------------------------------------------------------

data CSource = CSource !Source !(I.IORef ChunkState)

data ChunkState = NeedLen
                | NeedLenNewline
                | HaveLen Word
                | DoneChunking
    deriving Show

mkCSource :: Source -> IO CSource
mkCSource src = do
    ref <- I.newIORef NeedLen
    return $! CSource src ref

readCSource :: CSource -> IO ByteString
readCSource (CSource src ref) = do
    mlen <- I.readIORef ref
    go mlen
  where
    withLen 0 bs = do
        leftoverSource src bs
        dropCRLF
        yield' S.empty DoneChunking
    withLen len bs
        | S.null bs = do
            -- FIXME should this throw an exception if len > 0?
            I.writeIORef ref DoneChunking
            return S.empty
        | otherwise =
            case S.length bs `compare` fromIntegral len of
                EQ -> yield' bs NeedLenNewline
                LT -> yield' bs $ HaveLen $ len - fromIntegral (S.length bs)
                GT -> do
                    let (x, y) = S.splitAt (fromIntegral len) bs
                    leftoverSource src y
                    yield' x NeedLenNewline

    yield' bs mlen = do
        I.writeIORef ref mlen
        return bs

    dropCRLF = do
        bs <- readSource src
        case S.uncons bs of
            Nothing -> return ()
            Just (13, bs') -> dropLF bs'
            Just (10, bs') -> leftoverSource src bs'
            Just _ -> leftoverSource src bs

    dropLF bs =
        case S.uncons bs of
            Nothing -> do
                bs2 <- readSource' src
                unless (S.null bs2) $ dropLF bs2
            Just (10, bs') -> leftoverSource src bs'
            Just _ -> leftoverSource src bs

    go NeedLen = getLen
    go NeedLenNewline = dropCRLF >> getLen
    go (HaveLen 0) = do
        -- Drop the final CRLF
        dropCRLF
        I.writeIORef ref DoneChunking
        return S.empty
    go (HaveLen len) = do
        bs <- readSource src
        withLen len bs
    go DoneChunking = return S.empty

    -- Get the length from the source, and then pass off control to withLen
    getLen = do
        bs <- readSource src
        if S.null bs
            then do
                I.writeIORef ref $ assert False $ HaveLen 0
                return S.empty
            else do
                (x, y) <-
                    case S.break (== 10) bs of
                        (x, y)
                            | S.null y -> do
                                bs2 <- readSource' src
                                return $ if S.null bs2
                                    then (x, y)
                                    else S.break (== 10) $ bs `S.append` bs2
                            | otherwise -> return (x, y)
                let w =
                        S.foldl' (\i c -> i * 16 + fromIntegral (hexToWord c)) 0
                        $ S.takeWhile isHexDigit x

                let y' = S.drop 1 y
                y'' <-
                    if S.null y'
                        then readSource src
                        else return y'
                withLen w y''

    hexToWord w
        | w < 58 = w - 48
        | w < 71 = w - 55
        | otherwise = w - 87

isHexDigit :: Word8 -> Bool
isHexDigit w = w >= 48 && w <= 57
            || w >= 65 && w <= 70
            || w >= 97 && w <= 102
