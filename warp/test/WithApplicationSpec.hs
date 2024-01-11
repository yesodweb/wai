{-# LANGUAGE OverloadedStrings #-}

module WithApplicationSpec where

import Network.HTTP.Types
import Network.Wai
import System.Environment
import System.Process
import Test.Hspec
import UnliftIO.Exception

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
            let mkApp = return $ \_request _respond -> throwString "foo"
            withApplication mkApp $ \port -> do
                output <- readProcess "curl" ["-s", "localhost:" ++ show port] ""
                output `shouldContain` "Something went wron"

    describe "testWithApplication" $ do
        it "propagates exceptions from the server to the executing thread" $ do
            let mkApp = return $ \_request _respond -> throwString "foo"
            testWithApplication
                mkApp
                ( \port -> do
                    readProcess "curl" ["-s", "localhost:" ++ show port] ""
                )
                `shouldThrow` (\(StringException str _) -> str == "foo")
