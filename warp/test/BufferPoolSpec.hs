module BufferPoolSpec where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (ByteString(PS))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr)

import Test.Hspec (Spec, hspec, shouldBe, describe, it)

import Network.Wai.Handler.Warp.Buffer
    ( bufferSize
    , newBufferPool
    , withBufferPool
    )
import Network.Wai.Handler.Warp.Types (Buffer, BufSize)

main :: IO ()
main = hspec spec

-- Two ByteStrings each big enough to fill a 'bufferSize' buffer (16K).
wantData, otherData :: B.ByteString
wantData = B.replicate bufferSize 0xac
otherData = B.replicate bufferSize 0x77

spec :: Spec
spec = describe "withBufferPool" $ do
    it "does not clobber buffers" $ do
        pool <- newBufferPool
        -- 'pool' contains B.empty; prime it to contain a real buffer.
        _ <- withBufferPool pool $ const $ return 0
        -- 'pool' contains a 16K buffer; fill it with \xac and keep the result.
        got <- withBufferPool pool $ blitBuffer wantData
        got `shouldBe` wantData
        -- 'pool' should now be empty and reallocate, rather than clobber the
        -- previous buffer.
        _ <- withBufferPool pool $ blitBuffer otherData
        got `shouldBe` wantData

-- Fill the Buffer with the contents of the ByteString and return the number of
-- bytes written.  To be used with 'withBufferPool'.
blitBuffer :: B.ByteString -> (Buffer, BufSize) -> IO Int
blitBuffer (B.PS fp off len) (dst, len') = withForeignPtr fp $ \ptr -> do
    let src = ptr `plusPtr` off
        n = min len len'
    copyBytes dst src n
    return n
