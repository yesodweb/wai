{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.WebSockets
    ( intercept
    , interceptWith
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Network.Wai (Request, requestHeaders, rawPathInfo, requestHeaders)
import qualified Data.ByteString.Char8 as S
import qualified Data.Conduit as C
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Network.WebSockets as WS
import Network.Wai.Handler.Warp (Connection (..))

-- | For use with 'settingsIntercept' from the Warp web server.
intercept :: WS.Protocol p
          => (WS.Request -> WS.WebSockets p ())
          -> Request
          -> Maybe (C.BufferedSource (C.ResourceT IO) ByteString -> Connection -> C.ResourceT IO ())
intercept = interceptWith WS.defaultWebSocketsOptions

-- | Variation of 'intercept' which allows custom options.
interceptWith :: WS.Protocol p
              => WS.WebSocketsOptions
              -> (WS.Request -> WS.WebSockets p ())
              -> Request
              -> Maybe (C.BufferedSource (C.ResourceT IO) ByteString -> Connection -> C.ResourceT IO ())
interceptWith opts app req = case lookup "upgrade" $ requestHeaders req of
    Just s
        | S.map toLower s == "websocket" -> Just $ runWebSockets opts req' app
        | otherwise                      -> Nothing
    _                                    -> Nothing
  where
    req' = WS.RequestHttpPart (rawPathInfo req) (requestHeaders req)

-- | Internal function to run the WebSocket iteratee using the conduit library
runWebSockets :: WS.Protocol p
              => WS.WebSocketsOptions
              -> WS.RequestHttpPart
              -> (WS.Request -> WS.WebSockets p ())
              -> C.BufferedSource (C.ResourceT IO) ByteString
              -> Connection
              -> C.ResourceT IO ()
runWebSockets opts req app source conn = do
    step <- liftIO $ E.runIteratee $ WS.runWebSocketsWith opts req app send
    source C.$$ C.sinkState (E.returnI step) push close
  where
    send  = EL.mapM_ (connSendAll conn)
    close = const $ return ()

    push iter bs = do
        step <- liftIO $ E.runIteratee $ E.enumList 1 [bs] E.$$ iter
        return $ C.StateProcessing $ E.returnI step
