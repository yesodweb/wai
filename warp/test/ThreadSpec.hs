{-# LANGUAGE BangPatterns #-}

module ThreadSpec where

import Control.Concurrent (threadDelay)
import Data.IORef (readIORef, writeIORef, atomicModifyIORef)
import Network.Wai.Handler.Warp.Thread
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "forkIOwithBreakableForever" $ do
    it "can be breakable" $ do
        ref' <- forkIOwithBreakableForever True $ \ref -> do
            threadDelay 1000
            !_ <- atomicModifyIORef ref (\x -> (False, x))
            return ()
        threadDelay 100000
        readIORef ref' `shouldReturn` False
        _ <- breakForever ref'
        threadDelay 100000
        writeIORef ref' True
        threadDelay 100000
        readIORef ref' `shouldReturn` True
