{-# LANGUAGE OverloadedStrings #-}
module ConduitSpec where

import Network.Wai.Handler.Warp.Conduit
import Data.Conduit
import qualified Data.Conduit.List as CL
import Test.Hspec
import qualified Data.IORef as I
import qualified Data.ByteString as S
import Control.Monad.Trans.Class (lift)

spec :: Spec
spec = describe "conduit" $ do
    it "IsolatedBSSource" $ runResourceT $ do
        (rsrc, ()) <- mapM_ yield (map S.singleton [1..50]) $$+ return ()
        ibs <- lift $ fmap IsolatedBSSource $ I.newIORef (40, rsrc)
        x <- ibsIsolate ibs $$ CL.take 20
        lift $ S.concat x `shouldBe` S.pack [1..20]

        y <- ibsIsolate ibs $$ CL.consume
        lift $ S.concat y `shouldBe` S.pack [21..40]

        rsrc' <- lift $ ibsDone ibs
        z <- rsrc' $$+- CL.consume
        lift $ S.concat z `shouldBe` S.pack [41..50]
    it "chunkedSource" $ do
        (rsrc, ()) <- yield "5\r\n12345\r\n3\r\n678\r\n0\r\nBLAH" $$+ return ()
        ref <- I.newIORef (rsrc, NeedLen)
        x <- chunkedSource ref $$ CL.consume
        S.concat x `shouldBe` "12345678"
