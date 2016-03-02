{-# LANGUAGE OverloadedStrings #-}

module WithApplicationSpec where

import           Control.Exception
import           Network.HTTP.Types
import           Network.Wai
import           System.IO
import           System.IO.Silently
import           System.Process
import           Test.Hspec

import           Network.Wai.Handler.Warp.WithApplication

spec :: Spec
spec = do
  describe "withApplication" $ do
    it "runs a wai Application while executing the given action" $ do
      let mkApp = return $ \ _request respond -> respond $ responseLBS ok200 [] "foo"
      withApplication mkApp $ \ port -> do
        output <- readProcess "curl" ["-s", "localhost:" ++ show port] ""
        output `shouldBe` "foo"

    it "propagates exceptions from the server to the executing thread" $
      hSilence [stderr] $ do
        let mkApp = return $ \ _request _respond -> throwIO $ ErrorCall "foo"
        (withApplication mkApp $ \ port -> do
            readProcess "curl" ["-s", "localhost:" ++ show port] "")
          `shouldThrow` (errorCall "foo")
