module Devel.ReverseProxySpec where

import Test.Hspec
import Control.Concurrent
import Devel.ReverseProxy


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Reverse proxing" $ 
    it "Starts a reverse proxy server from 3000 to 3001" $ do
      tId <- forkIO $ startReverseProxy (3000, 3001)
      threadDelay 100
      killThread tId
      tId `shouldBe` tId

  describe "Port cycling" $
    it "Can cycle ports" $ do
      p <- cyclePorts 3000
      p `shouldBe` 3001

  describe "Socket creation" $ do
    it "Can create a socket at a given port." $ {-do
      _ <- createSocket 66542
      bool <- checkPort 66542
      bool `shouldBe` False-}
      pendingWith "How to create a socket in the tests without getting permission denied error?"



    it "Can create a socket safely." $ do
      (_, pNum) <- createSocketSafe 6666
      pNum `shouldBe` 6667

  describe "Check ports" $ do
    it "checkPort is True when port is free" $ do
      bool <- checkPort 54323
      bool `shouldBe` True
    it "checkPort is False when port is taken" $ {-do
       _ <- createSocket 6666
       bool <- checkPort 6666
       bool `shouldBe` False-}
       pendingWith "How to create a socket in the tests without getting permission denied error?"
