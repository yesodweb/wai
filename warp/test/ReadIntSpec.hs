module ReadIntSpec (main, spec) where

import Data.ByteString (ByteString)
import Test.Hspec
import Network.Wai.Handler.Warp.ReadInt
import qualified Data.ByteString.Char8 as B
import qualified Test.QuickCheck as QC

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "readInt64" $ do
    it "converts ByteString to Int" $ QC.property (prop_read_show_idempotent readInt64)

-- A QuickCheck property. Test that for a number >= 0, converting it to
-- a string using show and then reading the value back with the function
-- under test returns the original value.
-- The functions under test only work on Natural numbers (the Conent-Length
-- field in a HTTP header is always >= 0) so we check the absolute value of
-- the value that QuickCheck generates for us.
prop_read_show_idempotent :: (Integral a, Show a) => (ByteString -> a) -> a -> Bool
prop_read_show_idempotent freader x = px == freader (toByteString px)
  where
    px = abs x
    toByteString = B.pack . show
