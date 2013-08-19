{-# LANGUAGE OverloadedStrings #-}
{-|
    A WAI adapter to the HTML5 Server-Sent Events API.
-}
module Network.Wai.EventSource (
    ServerEvent(..),
    eventSourceAppChan,
    eventSourceAppSource,
    eventSourceAppIO,
    sourceToSource
    ) where

import           Blaze.ByteString.Builder (Builder)
import           Control.Concurrent.Chan (Chan, dupChan, readChan)
import           Control.Monad.IO.Class (liftIO)
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Network.HTTP.Types (status200)
import           Network.Wai (Application, Response, responseSource)

import Network.Wai.EventSource.EventStream

-- | Make a new WAI EventSource application reading events from
-- the given channel.
eventSourceAppChan :: Chan ServerEvent -> Application
eventSourceAppChan chan _ = do
  chan' <- liftIO $ dupChan chan
  return $ response chanToSource chan'

-- | Make a new WAI EventSource application reading events from
-- the given source.
eventSourceAppSource :: Source IO ServerEvent -> Application
eventSourceAppSource src _ = return $ response sourceToSource src

-- | Make a new WAI EventSource application reading events from
-- the given IO action.
eventSourceAppIO :: IO ServerEvent -> Application
eventSourceAppIO act _ = return $ response ioToSource act

response :: (a -> Source IO (Flush Builder)) -> a -> Response
response f a = responseSource status200 [("Content-Type", "text/event-stream")] $ f a

chanToSource :: Chan ServerEvent -> Source IO (Flush Builder)
chanToSource = ioToSource . readChan

ioToSource :: IO ServerEvent -> Source IO (Flush Builder)
ioToSource act =
    loop
  where
    loop = do
        x <- liftIO act
        case eventToBuilder x of
            Nothing -> return ()
            Just y -> do
                yield $ Chunk y
                yield Flush
                loop

-- | Convert a ServerEvent source into a Builder source of serialized
-- events.
sourceToSource :: Monad m => Source m ServerEvent -> Source m (Flush Builder)
sourceToSource src = src $= CL.concatMap eventToFlushBuilder
  where
    eventToFlushBuilder event =
        case eventToBuilder event of
            Nothing -> []
            Just x -> [Chunk x, Flush]
