module PackIntSpec (spec) where

import qualified Data.ByteString.Char8 as C8
import Network.Wai.Handler.Warp.PackInt
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as QC

spec :: Spec
spec = describe "readInt64" $ do
    prop "" $ \n -> packIntegral (abs n :: Int) == C8.pack (show (abs n))
    prop "" $ \(QC.Large n) ->
        let n' = fromIntegral (abs n :: Int)
         in packIntegral (n' :: Int) == C8.pack (show n')
    -- Every power of ten from 10^15 up used to be written into a buffer
    -- one byte too small for it.
    it "writes a number of any size" $
        let ns = [10 ^ e + d | e <- [0 .. 40 :: Int], d <- [-1, 0, 1 :: Integer]]
         in map packIntegral (filter (>= 0) ns)
                `shouldBe` map (C8.pack . show) (filter (>= 0) ns)
