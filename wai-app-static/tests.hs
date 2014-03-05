import WaiAppStaticTest (spec)
import WaiAppEmbeddedTest (embSpec)
import Test.Hspec

main :: IO ()
main = hspec $ spec >> embSpec
