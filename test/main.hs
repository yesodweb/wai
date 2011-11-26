import qualified WaiAppStaticTest
import qualified WaiExtraTest
import Test.Hspec.Monadic

main :: IO a
main = hspecX $ do
  WaiAppStaticTest.specs
  WaiExtraTest.specs
