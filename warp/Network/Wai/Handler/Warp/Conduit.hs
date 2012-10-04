module Network.Wai.Handler.Warp.Conduit where

import Control.Applicative
import Control.Exception
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Internal (ResumableSource (..))
import qualified Data.Conduit.List as CL
import qualified Data.IORef as I
import Data.Word (Word, Word8)
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

-- | Contains a @Source@ and a byte count that is still to be read in.
newtype IsolatedBSSource = IsolatedBSSource (I.IORef (Int, ResumableSource (ResourceT IO) ByteString))

-- | Given an @IsolatedBSSource@ provide a @Source@ that only allows up to the
-- specified number of bytes to be passed downstream. All leftovers should be
-- retained within the @Source@. If there are not enough bytes available,
-- throws a @ConnectionClosedByPeer@ exception.
ibsIsolate :: IsolatedBSSource -> Source (ResourceT IO) ByteString
ibsIsolate ibs@(IsolatedBSSource ref) = do
    (count, src) <- liftIO $ I.readIORef ref
    unless (count == 0) $ do
        -- Get the next chunk (if available) and the updated source
        (src', mbs) <- lift $ src $$++ CL.head

        -- If no chunk available, then there aren't enough bytes in the
        -- stream. Throw a ConnectionClosedByPeer
        bs <- maybe (liftIO $ throwIO ConnectionClosedByPeer) return mbs

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
                    liftIO $ I.writeIORef ref (count', src')
                    yield bs
                    ibsIsolate ibs

                -- The expected count is the total size of the chunk we
                -- just read. Send this chunk downstream, and then
                -- terminate the stream.
                | count == S.length bs -> do
                    liftIO $ I.writeIORef ref (count', src')
                    yield bs

                -- Some of the bytes in this chunk should not be sent
                -- downstream. Split up the chunk into the sent and
                -- not-sent parts, add the not-sent parts onto the new
                -- source, and send the rest of the chunk downstream.
                | otherwise -> do
                    let (x, y) = S.splitAt toSend bs
                    liftIO $ I.writeIORef ref (count', fmapResume (yield y >>) src')
                    yield x

-- | Extract the underlying @Source@ from an @IsolatedBSSource@, which will not
-- perform any more isolation.
ibsDone :: IsolatedBSSource -> IO (ResumableSource (ResourceT IO) ByteString)
ibsDone (IsolatedBSSource ref) = snd <$> I.readIORef ref

----------------------------------------------------------------

data ChunkState = NeedLen
                | NeedLenNewline
                | HaveLen Word

bsCRLF :: L.ByteString
bsCRLF = pack "\r\n"

chunkedSource :: MonadIO m
              => I.IORef (ResumableSource m ByteString, ChunkState)
              -> Source m ByteString
chunkedSource ipair = do
    (src, mlen) <- liftIO $ I.readIORef ipair
    go src mlen
  where
    go' src front = do
        (src', (len, bs)) <- lift $ src $$++ front getLen
        let src''
                | S.null bs = src'
                | otherwise = fmapResume (yield bs >>) src'
        go src'' $ HaveLen len

    go src NeedLen = go' src id
    go src NeedLenNewline = go' src (CB.take 2 >>)
    go src (HaveLen 0) = do
        -- Drop the final CRLF
        (src', ()) <- lift $ src $$++ do
            crlf <- CB.take 2
            unless (crlf == bsCRLF) $ leftover $ S.concat $ L.toChunks crlf
        liftIO $ I.writeIORef ipair (src', HaveLen 0)
    go src (HaveLen len) = do
        (src', mbs) <- lift $ src $$++ CL.head
        case mbs of
            Nothing -> liftIO $ I.writeIORef ipair (src', HaveLen 0)
            Just bs ->
                case S.length bs `compare` fromIntegral len of
                    EQ -> yield' src' NeedLenNewline bs
                    LT -> do
                        let mlen = HaveLen $ len - fromIntegral (S.length bs)
                        yield' src' mlen bs
                    GT -> do
                        let (x, y) = S.splitAt (fromIntegral len) bs
                        let src'' = fmapResume (yield y >>) src'
                        yield' src'' NeedLenNewline x

    yield' src mlen bs = do
        liftIO $ I.writeIORef ipair (src, mlen)
        yield bs
        go src mlen

    getLen :: Monad m => Sink ByteString m (Word, ByteString)
    getLen = do
        mbs <- CL.head
        case mbs of
            Nothing -> return (0, S.empty)
            Just bs -> do
                (x, y) <-
                    case S.breakByte 10 bs of
                        (x, y)
                            | S.null y -> do
                                mbs2 <- CL.head
                                case mbs2 of
                                    Nothing -> return (x, y)
                                    Just bs2 -> return $ S.breakByte 10 $ bs `S.append` bs2
                            | otherwise -> return (x, y)
                let w =
                        S.foldl' (\i c -> i * 16 + fromIntegral (hexToWord c)) 0
                        $ S.takeWhile isHexDigit x
                return (w, S.drop 1 y)

    hexToWord w
        | w < 58 = w - 48
        | w < 71 = w - 55
        | otherwise = w - 87

isHexDigit :: Word8 -> Bool
isHexDigit w = w >= 48 && w <= 57
            || w >= 65 && w <= 70
            || w >= 97 && w <= 102

----------------------------------------------------------------

fmapResume :: (Source m o1 -> Source m o2) -> ResumableSource m o1 -> ResumableSource m o2
fmapResume f (ResumableSource src m) = ResumableSource (f src) m
