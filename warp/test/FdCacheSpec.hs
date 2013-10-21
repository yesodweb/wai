module FdCacheSpec where

import Data.IORef
import Network.Wai.Handler.Warp.FdCache
import System.Posix.IO (fdRead)
import System.Posix.Types (Fd(..))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "withFdCache" $ do
    it "clean up Fd" $ do
        ref <- newIORef (Fd (-1))
        withFdCache 30000000 $ \(Just mfc) -> do
            (fd,_) <- getFd mfc "warp.cabal"
            writeIORef ref fd
        nfd <- readIORef ref
        fdRead nfd 1 `shouldThrow` anyIOException
