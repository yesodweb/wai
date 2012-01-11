{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.WebSockets
    ( intercept
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Network.Socket (Socket)
import Network.Wai (Request, requestHeaders, rawPathInfo, requestHeaders)
import qualified Data.ByteString.Char8 as S
import qualified Data.Conduit as C
import qualified Data.Enumerator as E
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Internal as WS

-- | For use with 'settingsIntercept' from the Warp web server.
intercept :: WS.Protocol p
          => (WS.Request -> WS.WebSockets p ())
          -> Request
          -> Maybe (C.BufferedSource IO ByteString -> Socket -> C.ResourceT IO ())
intercept app req = case lookup "upgrade" $ requestHeaders req of
    Just s
        | S.map toLower s == "websocket" -> Just $ runWebSockets req' app
        | otherwise                      -> Nothing
    _                                    -> Nothing
  where
    req' = WS.RequestHttpPart (rawPathInfo req) (requestHeaders req)

-- | Internal function to run the WebSocket iteratee using the conduit library
runWebSockets :: WS.Protocol p
              => WS.RequestHttpPart
              -> (WS.Request -> WS.WebSockets p ())
              -> C.BufferedSource IO ByteString
              -> Socket
              -> C.ResourceT IO ()
runWebSockets req app source sock = do
    step <- liftIO $ E.runIteratee $ WS.runWebSockets req app send
    source C.$$ C.sinkState (E.returnI step) push close
  where
    send  = WS.iterSocket sock
    close = const $ return ()

    push iter bs = do
        step <- liftIO $ E.runIteratee $ E.enumList 1 [bs] E.$$ iter
        return (E.returnI step, C.Processing)
