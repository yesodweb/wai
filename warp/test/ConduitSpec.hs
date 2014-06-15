{-# LANGUAGE OverloadedStrings #-}
module ConduitSpec (main, spec) where

import Network.Wai.Handler.Warp.Conduit
import Network.Wai.Handler.Warp.Types
import Control.Monad (replicateM)
import Test.Hspec
import Data.IORef as I
import qualified Data.ByteString as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "conduit" $ do
    it "IsolatedBSSource" $ do
        ref <- newIORef $ map S.singleton [1..50]
        src <- mkSource $ do
            x <- readIORef ref
            case x of
                [] -> return S.empty
                y:z -> do
                    writeIORef ref z
                    return y
        isrc <- mkISource src 40
        x <- replicateM 20 $ readISource isrc
        S.concat x `shouldBe` S.pack [1..20]

        y <- replicateM 40 $ readISource isrc
        S.concat y `shouldBe` S.pack [21..40]

        z <- replicateM 40 $ readSource src
        S.concat z `shouldBe` S.pack [41..50]
    it "chunkedSource" $ do
        ref <- newIORef $ "5\r\n12345\r\n3\r\n678\r\n0\r\n\r\nBLAH"
        src <- mkSource $ do
            x <- readIORef ref
            writeIORef ref S.empty
            return x
        csrc <- mkCSource src

        x <- replicateM 15 $ readCSource csrc
        S.concat x `shouldBe` "12345678"

        y <- replicateM 15 $ readSource src
        S.concat y `shouldBe` "BLAH"
    it "chunk boundaries" $ do
        ref <- newIORef
            [ "5\r\n"
            , "12345\r\n3\r"
            , "\n678\r\n0\r\n"
            , "\r\nBLAH"
            ]
        src <- mkSource $ do
            x <- readIORef ref
            case x of
                [] -> return S.empty
                y:z -> do
                    writeIORef ref z
                    return y
        csrc <- mkCSource src

        x <- replicateM 15 $ readCSource csrc
        S.concat x `shouldBe` "12345678"

        y <- replicateM 15 $ readSource src
        S.concat y `shouldBe` "BLAH"
