{-# LANGUAGE CPP #-}

module FdCacheSpec where

import Test.Hspec
#ifndef WINDOWS
import Data.IORef
import Network.Wai.Handler.Warp.FdCache
import System.Posix.IO (fdRead)
import System.Posix.Types (Fd(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "withFdCache" $ do
    it "clean up Fd" $ do
        ref <- newIORef (Fd (-1))
        withFdCache 30000000 $ \getFd -> do
            (Just fd,_) <- getFd 0 "warp.cabal"
            writeIORef ref fd
        nfd <- readIORef ref
        fdRead nfd 1 `shouldThrow` anyIOException
#else
spec :: Spec
spec = return ()
#endif
