{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.EventSource (
    ServerEvent(..),
    eventSourceApp
    ) where

import           Blaze.ByteString.Builder (Builder)
import           Control.Concurrent.Chan (Chan, dupChan, readChan)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Conduit as C
import           Network.HTTP.Types (statusOK)
import           Network.Wai (Application, Response(..))

import Network.Wai.EventSource.EventStream

-- | Make a new WAI EventSource application reading events from
-- the given channel.
eventSourceApp :: Chan ServerEvent -> Application
eventSourceApp chan _ = do
  chan' <- liftIO $ dupChan chan
  return $ res chan'

res :: Chan ServerEvent -> Response
res chan = ResponseSource statusOK [("Content-Type", "text/event-stream")] $ resE chan

resE :: Chan ServerEvent -> C.Source IO (C.Flush Builder)
resE chan =
    C.sourceState Nothing pull
  where
    pull Nothing = do
        x <- liftIO $ readChan chan
        return $ case eventToBuilder x of
            Nothing -> C.StateClosed
            Just y -> C.StateOpen (Just C.Flush) (C.Chunk y)
    pull (Just x) = return $ C.StateOpen Nothing x
