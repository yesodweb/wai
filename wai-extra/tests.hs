import Test.Hspec.Monadic
import qualified WaiExtraTest

main :: IO ()
main = hspecX WaiExtraTest.specs
