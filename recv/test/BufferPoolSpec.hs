module BufferPoolSpec where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (ByteString (PS))
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr)

import Network.Socket.BufferPool
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldReturn)

main :: IO ()
main = hspec spec

-- Two ByteStrings each big enough to fill a buffer (16K).
wantData, otherData :: B.ByteString
wantData = B.replicate 16384 0xac
otherData = B.replicate 16384 0x77

spec :: Spec
spec = do
    describe "withBufferPool" $ do
        it "does not clobber buffers" $ do
            pool <- newBufferPool 2048 16384
            -- 'pool' contains B.empty; prime it to contain a real buffer.
            _ <- withBufferPool pool $ \_ _ -> return 0
            -- 'pool' contains a 16K buffer; fill it with \xac and keep the result.
            got <- withBufferPool pool $ blitBuffer wantData
            got `shouldBe` wantData
            -- 'pool' should now be empty and reallocate, rather than clobber the
            -- previous buffer.
            _ <- withBufferPool pool $ blitBuffer otherData
            got `shouldBe` wantData

    describe "tryWithBufferPool" $ do
        it "returns the filled prefix when the filler fills" $ do
            pool <- newBufferPool 2048 16384
            tryWithBufferPool pool (blitBuffer (B.take 10 wantData))
                `shouldReturn` Just (B.take 10 wantData)

        it "returns an empty ByteString when the filler consumes nothing" $ do
            -- 'receiveNoWait' reports EOF this way, so it must not be 'Nothing'.
            pool <- newBufferPool 2048 16384
            tryWithBufferPool pool (\_ _ -> return 0) `shouldReturn` Just B.empty

        it "returns Nothing when the filler declines" $ do
            pool <- newBufferPool 2048 16384
            tryWithBufferPool pool (\_ _ -> return (-1)) `shouldReturn` Nothing

        it "keeps the leftover buffer when the filler declines" $ do
            pool <- newBufferPool 2048 16384
            -- Consume 10000 of the 16384 bytes, leaving 6384 in the pool.
            _ <- tryWithBufferPool pool $ \_ _ -> return 10000
            tryWithBufferPool pool (\_ _ -> return (-1)) `shouldReturn` Nothing
            -- The declined call must neither consume, drop nor reallocate the
            -- leftover: the next filler is offered exactly those 6384 bytes.
            offered <- offeredSize pool
            offered `shouldBe` 6384

        it "does not corrupt buffered data across a decline" $ do
            pool <- newBufferPool 2048 16384
            _ <- tryWithBufferPool pool $ \_ _ -> return 0
            tryWithBufferPool pool (\_ _ -> return (-1)) `shouldReturn` Nothing
            tryWithBufferPool pool (blitBuffer wantData)
                `shouldReturn` Just wantData

-- The 'BufSize' the pool offers to the next filler, leaving the pool as it was.
offeredSize :: BufferPool -> IO Int
offeredSize pool = do
    ref <- newIORef 0
    _ <- tryWithBufferPool pool $ \_ size -> writeIORef ref size >> return 0
    readIORef ref

-- Fill the Buffer with the contents of the ByteString and return the number of
-- bytes written.  To be used with 'withBufferPool'.
blitBuffer :: B.ByteString -> Buffer -> BufSize -> IO Int
blitBuffer (B.PS fp off len) dst len' = withForeignPtr fp $ \ptr -> do
    let src = ptr `plusPtr` off
        n = min len len'
    copyBytes dst src n
    return n
