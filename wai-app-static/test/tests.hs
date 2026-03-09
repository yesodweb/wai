import Test.Hspec
import WaiAppEmbeddedTest (embSpec)
import WaiAppStaticTest (spec)

main :: IO ()
main = hspec $ spec >> embSpec
