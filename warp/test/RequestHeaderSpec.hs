{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module RequestHeaderSpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Network.Wai.Handler.Warp.RequestHeader
import Network.Wai.Handler.Warp.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parseHeaderLines" $ do
        it "throws BadHostHeader when missing host" $
            parseHeaderLines ["GET / HTTP/1.1", "Dave: lisa"] `shouldThrow` badHostHeader
        it "throws BadHostHeader when multiple hosts" $
            parseHeaderLines ["GET / HTTP/1.1", "Host: dave", "Host: lisa"] `shouldThrow` badHostHeader

badHostHeader :: Selector InvalidRequest
badHostHeader e = e == BadHostHeader
