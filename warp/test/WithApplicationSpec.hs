{-# LANGUAGE OverloadedStrings #-}

module WithApplicationSpec where

import Control.Exception
import Network.HTTP.Types
import Network.Wai
import System.Environment
import System.Process
import Test.Hspec

import Network.Wai.Handler.Warp (defaultSettings, setOnException)
import Network.Wai.Handler.Warp.WithApplication

-- All these tests assume the "curl" process can be called directly.
spec :: Spec
spec = do
    runIO $ do
        unsetEnv "http_proxy"
        unsetEnv "https_proxy"
    describe "\"curl\" dependency" $
        let msg =
                "All \"WithApplication\" tests assume the \"curl\" process can be called directly."
            underline = replicate (length msg) '^'
         in it (msg ++ "\n    " ++ underline) True
    describe "withApplication" $ do
        it "runs a wai Application while executing the given action" $ do
            let mkApp = return $ \_request respond -> respond $ responseLBS ok200 [] "foo"
            withApplication mkApp $ \port -> do
                output <- readProcess "curl" ["-s", "localhost:" ++ show port] ""
                output `shouldBe` "foo"

        it "does not propagate exceptions from the server to the executing thread" $ do
            let mkApp = return $ \_request _respond -> throwIO $ ErrorCall "foo"
            withApplicationSettings silentSettings mkApp $ \port -> do
                output <- readProcess "curl" ["-s", "localhost:" ++ show port] ""
                output `shouldContain` "Something went wrong"

    describe "testWithApplication" $ do
        it "propagates exceptions from the server to the executing thread" $ do
            let mkApp = return $ \_request _respond -> throwIO $ ErrorCall "foo"
            testWithApplicationSettings
                silentSettings
                mkApp
                ( \port -> do
                    readProcess "curl" ["-s", "localhost:" ++ show port] ""
                )
                `shouldThrow` (errorCall "foo")
  where
    -- So that we don't muddy the test result screen.
    -- (normally, 'defaultSettings' use 'defaultOnException', sending to 'stderr')
    silentSettings = setOnException (\_ _ -> pure ()) defaultSettings
