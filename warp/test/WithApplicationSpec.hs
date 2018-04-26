{-# LANGUAGE OverloadedStrings #-}

module WithApplicationSpec where

import           Control.Exception
import           Network.HTTP.Types
import           Network.Wai
import           System.Environment
import           System.IO
import           System.IO.Silently
import           System.Process
import           Test.Hspec

import           Network.Wai.Handler.Warp.WithApplication

spec :: Spec
spec = do
  runIO $ do
      unsetEnv "http_proxy"
      unsetEnv "https_proxy"
  describe "withApplication" $ do
    it "runs a wai Application while executing the given action" $ do
      let mkApp = return $ \ _request respond -> respond $ responseLBS ok200 [] "foo"
      withApplication mkApp $ \ port -> do
        output <- readProcess "curl" ["-s", "localhost:" ++ show port] ""
        output `shouldBe` "foo"

    it "does not propagate exceptions from the server to the executing thread" $
      hSilence [stderr] $ do
        let mkApp = return $ \ _request _respond -> throwIO $ ErrorCall "foo"
        withApplication mkApp $ \ port -> do
          output <- readProcess "curl" ["-s", "localhost:" ++ show port] ""
          output `shouldContain` "Something went wron"

  describe "testWithApplication" $ do
    it "propagates exceptions from the server to the executing thread" $
      hSilence [stderr] $ do
        let mkApp = return $ \ _request _respond -> throwIO $ ErrorCall "foo"
        (testWithApplication mkApp $ \ port -> do
            readProcess "curl" ["-s", "localhost:" ++ show port] "")
          `shouldThrow` (errorCall "foo")

{- The future netwrok library will not export MkSocket.
  describe "withFreePort" $ do
    it "closes the socket before exiting" $ do
      MkSocket _ _ _ _ statusMVar <- withFreePort $ \ (_, sock) -> do
        return sock
      readMVar statusMVar `shouldReturn` Closed
-}
