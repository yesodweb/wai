{-|
    A WAI adapter to the HTML5 Server-Sent Events API.

    If running through a proxy like Nginx you might need to add the
    headers:

    > [ ("X-Accel-Buffering", "no"), ("Cache-Control", "no-cache")]
-}
module Network.Wai.EventSource (
    ServerEvent(..),
    eventSourceAppChan,
    eventSourceAppIO
    ) where

import           Data.Function (fix)
import           Control.Concurrent.Chan (Chan, dupChan, readChan)
import           Control.Monad.IO.Class (liftIO)
import           Network.HTTP.Types (status200, hContentType)
import           Network.Wai (Application, responseStream)

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
        $ \sendChunk flush -> fix $ \loop -> do
            se <- src
            case eventToBuilder se of
                Nothing -> return ()
                Just b  -> sendChunk b >> flush >> loop
