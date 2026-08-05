{-# LANGUAGE OverloadedStrings #-}

module BufferSpec (main, spec) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as BLD
import Data.IORef as I
import Network.Wai.Handler.Warp.Buffer (createWriteBuffer)
import Network.Wai.Handler.Warp.IO (toBufIOWith)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (NonNegative (..), withMaxSize)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "toBufIOWith" $ do
    it "counts short bytestrings" $
        testBufIOWith 10
    -- This failed before fixing 'toBufIOWith'
    it "counts long bytestrings" $ do
        testBufIOWith 1000000
    prop "counts bytestrings of different sizes" . withMaxSize 10000000 $
        \(NonNegative i) -> testBufIOWith i

testBufIOWith :: Int -> Expectation
testBufIOWith bsLen = do
    len <- toBufIOWithBuilder $ BLD.byteString $ S.replicate bsLen 0
    len `shouldBe` fromIntegral bsLen

toBufIOWithBuilder :: BLD.Builder -> IO Integer
toBufIOWithBuilder bld = do
    countRef <- newIORef 0 :: IO (IORef Int)
    buf <- createWriteBuffer 16384
    bufRef <- newIORef buf
    let go bs = modifyIORef' countRef (+ S.length bs)
    toBufIOWith 1049000000 bufRef go bld
