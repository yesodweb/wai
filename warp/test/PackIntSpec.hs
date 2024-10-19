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
