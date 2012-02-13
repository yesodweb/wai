{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.EventSource (
    ServerEvent(..),
    eventSourceApp,
    eventSourceAppChan,
    eventSourceAppSource,
    eventSourceAppIO
    ) where

import           Blaze.ByteString.Builder (Builder)
import           Control.Concurrent.Chan (Chan, dupChan, readChan)
import           Control.Monad.IO.Class (liftIO)
import           Data.Conduit (($=))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Network.HTTP.Types (statusOK)
import           Network.Wai (Application, Response(..))

import Network.Wai.EventSource.EventStream

-- | Make a new WAI EventSource application reading events from
-- the given channel.
eventSourceApp :: Chan ServerEvent -> Application
eventSourceApp = eventSourceAppChan
{-# DEPRECATED eventSourceApp "Use evnetSourceAppChan instead." #-}

-- | Make a new WAI EventSource application reading events from
-- the given channel.
eventSourceAppChan :: Chan ServerEvent -> Application
eventSourceAppChan chan _ = do
  chan' <- liftIO $ dupChan chan
  return $ response chanToSource chan'

-- | Make a new WAI EventSource application reading events from
-- the given source.
eventSourceAppSource :: C.Source IO ServerEvent -> Application
eventSourceAppSource src _ = return $ response sourceToSource src

-- | Make a new WAI EventSource application reading events from
-- the given IO action.
eventSourceAppIO :: IO ServerEvent -> Application
eventSourceAppIO act _ = return $ response ioToSource act

response :: (a -> C.Source IO (C.Flush Builder)) -> a -> Response
response f a = ResponseSource statusOK [("Content-Type", "text/event-stream")] $ f a

chanToSource :: Chan ServerEvent -> C.Source IO (C.Flush Builder)
chanToSource chan =
    C.sourceState Nothing pull
  where
    pull Nothing = do
        x <- liftIO $ readChan chan
        return $ case eventToBuilder x of
            Nothing -> C.StateClosed
            Just y -> C.StateOpen (Just C.Flush) (C.Chunk y)
    pull (Just x) = return $ C.StateOpen Nothing x

ioToSource :: IO ServerEvent -> C.Source IO (C.Flush Builder)
ioToSource act =
    C.sourceState Nothing pull
  where
    pull Nothing = do
        x <- liftIO act
        return $ case eventToBuilder x of
            Nothing -> C.StateClosed
            Just y -> C.StateOpen (Just C.Flush) (C.Chunk y)
    pull (Just x) = return $ C.StateOpen Nothing x

sourceToSource :: C.Source IO ServerEvent -> C.Source IO (C.Flush Builder)
sourceToSource src = src $= CL.concatMap eventToFlushBuilder
  where
    eventToFlushBuilder event =
        case eventToBuilder event of
            Nothing -> []
            Just x -> [C.Chunk x, C.Flush]
