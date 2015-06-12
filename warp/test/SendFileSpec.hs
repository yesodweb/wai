{-# LANGUAGE OverloadedStrings #-}

module SendFileSpec where

import Control.Exception
import Data.ByteString (ByteString)
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.SendFile
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "packHeader" $ do
    it "returns how much the buffer is consumed (1)" $
        tryPackHeader 10 ["foo"] `shouldReturn` 3
    it "returns how much the buffer is consumed (2)" $
        tryPackHeader 10 ["foo", "bar"] `shouldReturn` 6
    it "returns how much the buffer is consumed (3)" $
        tryPackHeader 10 ["0123456789"] `shouldReturn` 0
    it "returns how much the buffer is consumed (4)" $
        tryPackHeader 10 ["01234", "56789"] `shouldReturn` 0
    it "returns how much the buffer is consumed (5)" $
        tryPackHeader 10 ["01234567890", "12"] `shouldReturn` 3
    it "returns how much the buffer is consumed (6)" $
        tryPackHeader 10 ["012345678901234567890123456789012", "34"] `shouldReturn` 5

tryPackHeader :: Int -> [ByteString] -> IO Int
tryPackHeader siz hdrs = bracket (allocateBuffer siz) freeBuffer $ \buf ->
    packHeader buf siz send hook hdrs 0
  where
    send _ = return ()
    hook = return ()
