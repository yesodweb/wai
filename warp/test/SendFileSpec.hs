{-# LANGUAGE OverloadedStrings #-}

module SendFileSpec where

import Control.Exception
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.SendFile
import System.Directory
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
    it "returns how much the buffer is consumed (1)" $
        tryPackHeader2 10 ["foo"] "" `shouldReturn` True
    it "returns how much the buffer is consumed (2)" $
        tryPackHeader2 10 ["foo", "bar"] "" `shouldReturn` True
    it "returns how much the buffer is consumed (3)" $
        tryPackHeader2 10 ["0123456789"] "0123456789" `shouldReturn` True
    it "returns how much the buffer is consumed (4)" $
        tryPackHeader2 10 ["01234", "56789"] "0123456789" `shouldReturn` True
    it "returns how much the buffer is consumed (5)" $
        tryPackHeader2 10 ["01234567890", "12"] "0123456789" `shouldReturn` True
    it "returns how much the buffer is consumed (6)" $
        tryPackHeader2 10 ["012345678901234567890123456789012", "34"] "012345678901234567890123456789" `shouldReturn` True

tryPackHeader :: Int -> [ByteString] -> IO Int
tryPackHeader siz hdrs = bracket (allocateBuffer siz) freeBuffer $ \buf ->
    packHeader buf siz send hook hdrs 0
  where
    send _ = return ()
    hook = return ()

tryPackHeader2 :: Int -> [ByteString] -> ByteString -> IO Bool
tryPackHeader2 siz hdrs ans = bracket setup teardown $ \buf -> do
    _ <- packHeader buf siz send hook hdrs 0
    checkFile outputFile ans
  where
    setup = allocateBuffer siz
    teardown buf = freeBuffer buf >> removeFileIfExists outputFile
    outputFile = "tempfile"
    send = BS.appendFile outputFile
    hook = return ()

checkFile :: FilePath -> ByteString -> IO Bool
checkFile path bs = do
    exist <- doesFileExist path
    if exist then do
        bs' <- BS.readFile path
        return $ bs == bs'
      else
        return $ bs == ""

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file = do
    exist <- doesFileExist file
    when exist $ removeFile file
