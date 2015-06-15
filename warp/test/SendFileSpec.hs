{-# LANGUAGE OverloadedStrings #-}

module SendFileSpec where

import Control.Exception
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.SendFile
import Network.Wai.Handler.Warp.Types
import System.Directory
import System.Exit
import qualified System.IO as IO
import System.Process (system)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "packHeader" $ do
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

    it "sends headers correctly (1)" $
        tryPackHeader2 10 ["foo"] "" `shouldReturn` True
    it "sends headers correctly (2)" $
        tryPackHeader2 10 ["foo", "bar"] "" `shouldReturn` True
    it "sends headers correctly (3)" $
        tryPackHeader2 10 ["0123456789"] "0123456789" `shouldReturn` True
    it "sends headers correctly (4)" $
        tryPackHeader2 10 ["01234", "56789"] "0123456789" `shouldReturn` True
    it "sends headers correctly (5)" $
        tryPackHeader2 10 ["01234567890", "12"] "0123456789" `shouldReturn` True
    it "sends headers correctly (6)" $
        tryPackHeader2 10 ["012345678901234567890123456789012", "34"] "012345678901234567890123456789" `shouldReturn` True

  describe "readSendFile" $ do
    it "sends a file correctly (1)" $
        tryReadSendFile 10 0 1474 ["foo"] `shouldReturn` ExitSuccess
    it "sends a file correctly (2)" $
        tryReadSendFile 10 0 1474 ["012345678", "901234"] `shouldReturn` ExitSuccess
    it "sends a file correctly (3)" $
        tryReadSendFile 10 20 100 ["012345678", "901234"] `shouldReturn` ExitSuccess

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

tryReadSendFile :: Int -> Integer -> Integer -> [ByteString] -> IO ExitCode
tryReadSendFile siz off len hdrs = bracket setup teardown $ \buf -> do
    mapM_ (BS.appendFile expectedFile) hdrs
    copyfile inputFile expectedFile off len
    readSendFile buf siz send fid off len hook hdrs
    compareFiles expectedFile outputFile
  where
    hook = return ()
    setup = allocateBuffer siz
    teardown buf = do
        freeBuffer buf
        removeFileIfExists outputFile
        removeFileIfExists expectedFile
    inputFile = "test/inputFile"
    outputFile = "outputFile"
    expectedFile = "expectedFile"
    fid = FileId inputFile Nothing
    send = BS.appendFile outputFile

checkFile :: FilePath -> ByteString -> IO Bool
checkFile path bs = do
    exist <- doesFileExist path
    if exist then do
        bs' <- BS.readFile path
        return $ bs == bs'
      else
        return $ bs == ""

compareFiles :: FilePath -> FilePath -> IO ExitCode
compareFiles file1 file2 = system $ "cmp -s " ++ file1 ++ " " ++ file2

copyfile :: FilePath -> FilePath -> Integer -> Integer -> IO ()
copyfile src dst off len =
  IO.withBinaryFile src IO.ReadMode $ \h -> do
     IO.hSeek h IO.AbsoluteSeek off
     BS.hGet h (fromIntegral len) >>= BS.appendFile dst

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file = do
    exist <- doesFileExist file
    when exist $ removeFile file
