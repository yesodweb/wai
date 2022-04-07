{-|
    A WAI adapter to the HTML5 Server-Sent Events API.

    If running through a proxy like Nginx you might need to add the
    headers:

    > [ ("X-Accel-Buffering", "no"), ("Cache-Control", "no-cache")]

    There is a small example using these functions in the @example@ directory.
-}
module Network.Wai.EventSource (
    ServerEvent (..),
    eventSourceAppChan,
    eventSourceAppIO,
    eventStreamAppRaw,
) where

import Control.Concurrent.Chan (Chan, dupChan, readChan)
import Control.Monad.IO.Class (liftIO)
import Data.Function (fix)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Application, responseStream)

import Network.Wai.EventSource.EventStream

-- | Make a new WAI EventSource application reading events from
-- the given channel.
eventSourceAppChan :: Chan ServerEvent -> Application
eventSourceAppChan chan req sendResponse = do
    chan' <- liftIO $ dupChan chan
    eventSourceAppIO (readChan chan') req sendResponse

-- | Make a new WAI EventSource application reading events from
-- the given IO action.
eventSourceAppIO :: IO ServerEvent -> Application
eventSourceAppIO src _ sendResponse =
    sendResponse $ responseStream
        status200
        [(hContentType, "text/event-stream")]
        $ \sendChunk flush -> do
            flush
            fix $ \loop -> do
                se <- src
                case eventToBuilder se of
                    Nothing -> return ()
                    Just b  -> sendChunk b >> flush >> loop

-- | Make a new WAI EventSource application with a handler that emits events.
--
-- @since 3.0.28
eventStreamAppRaw :: ((ServerEvent -> IO()) -> IO () -> IO ()) -> Application
eventStreamAppRaw handler _ sendResponse =
    sendResponse $ responseStream
        status200
        [(hContentType, "text/event-stream")]
        $ \sendChunk flush -> handler (sendEvent sendChunk) flush
    where
        sendEvent sendChunk event =
            case eventToBuilder event of
                Nothing -> return ()
                Just b  -> sendChunk b
