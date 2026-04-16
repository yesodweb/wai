{-# LANGUAGE OverloadedStrings #-}

module ServerStateSpec where

import Network.Wai.Handler.Warp (getServerState)
import Network.Wai.Handler.Warp.Counter (getCount, increase)
import Network.Wai.Handler.Warp.Settings (
    ServerState (..),
    currentOpenConnections,
    currentShuttingDownState,
    defaultSettings,
    makeServerState,
    newServerState,
 )
import Network.Wai.Handler.Warp.ShuttingDown (writeShuttingDown)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "ServerState" $ do
        it "has the correct initialization" $ do
            ss <- newServerState
            currentOpenConnections ss `shouldReturn` 0
            currentShuttingDownState ss `shouldReturn` False
    describe "makeServerState" $ do
        it "has the same state in settings" $ do
            (outerSS, set) <- makeServerState defaultSettings
            case getServerState set of
                Nothing -> expectationFailure "'makeServerState' should set the 'ServerState'"
                Just innerSS -> do
                    let bothCount i = do
                            a <- currentOpenConnections outerSS
                            b <- currentOpenConnections innerSS
                            (a, b) `shouldBe` (i, i)
                    increase $ serverConnectionCounter outerSS
                    bothCount 1
                    increase $ serverConnectionCounter innerSS
                    bothCount 2
                    let bothDown bool = do
                            a <- currentShuttingDownState outerSS
                            b <- currentShuttingDownState innerSS
                            (a, b) `shouldBe` (bool, bool)
                    writeShuttingDown (serverShuttingDown outerSS) True
                    bothDown True
                    writeShuttingDown (serverShuttingDown innerSS) False
                    bothDown False
        it "is idempotent" $ do
            let incAndCheck ss i = do
                    increase $ serverConnectionCounter ss
                    currentOpenConnections ss `shouldReturn` i
            (ss1, set1) <- makeServerState defaultSettings
            incAndCheck ss1 1
            (ss2, _set2) <- makeServerState set1
            incAndCheck ss2 2
            incAndCheck ss1 3
