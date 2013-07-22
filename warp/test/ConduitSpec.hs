{-# LANGUAGE OverloadedStrings #-}
module ConduitSpec (main, spec) where

import Network.Wai.Handler.Warp.Conduit
import Data.Conduit
import qualified Data.Conduit.List as CL
import Test.Hspec
import qualified Data.IORef as I
import qualified Data.ByteString as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "conduit" $ do
    it "IsolatedBSSource" $ do
        (rsrc, ()) <- mapM_ yield (map S.singleton [1..50]) $$+ return ()
        ibs <- fmap IsolatedBSSource $ I.newIORef (40, rsrc)
        x <- ibsIsolate ibs $$ CL.take 20
        S.concat x `shouldBe` S.pack [1..20]

        y <- ibsIsolate ibs $$ CL.consume
        S.concat y `shouldBe` S.pack [21..40]

        rsrc' <- ibsDone ibs
        z <- rsrc' $$+- CL.consume
        S.concat z `shouldBe` S.pack [41..50]
    it "chunkedSource" $ do
        (rsrc, ()) <- yield "5\r\n12345\r\n3\r\n678\r\n0\r\nBLAH" $$+ return ()
        ref <- I.newIORef (rsrc, NeedLen)
        x <- chunkedSource ref $$ CL.consume
        S.concat x `shouldBe` "12345678"
