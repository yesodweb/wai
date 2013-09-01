import WaiAppStaticTest (spec)
import WaiAppEmbeddedTest (embSpec)
import Test.Hspec.Monadic

main :: IO ()
main = hspec $ spec >> embSpec
