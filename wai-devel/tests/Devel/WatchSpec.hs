module Devel.WatchSpec where

import Test.Hspec
import Devel.Watch

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Checking for change watching." $
    it "checkForChange successfully changes value of isDirty." $ do
      isDirty <- newTVarIO False
      _ <- forkIO $ changeIsDirty isDirty
      checkForChange isDirty
      isDirtyBool <- atomically $ readTVar isDirty
      isDirtyBool `shouldBe` False

  describe "File watching" $
    it "Modifies isDirty when required file changes."
      pending



changeIsDirty :: TVar Bool -> IO ()
changeIsDirty isDirty = do
  _ <- threadDelay 10000
  atomically $ writeTVar isDirty True
