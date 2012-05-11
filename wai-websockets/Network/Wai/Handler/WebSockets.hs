{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.WebSockets
    ( intercept
    , interceptWith
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as S
import qualified Data.Conduit as C
import qualified Data.Enumerator as E
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

-- | For use with 'settingsIntercept' from the Warp web server.
intercept :: WS.Protocol p
          => (WS.Request -> WS.WebSockets p ())
          -> Wai.Request
          -> Maybe (C.Source (C.ResourceT IO) ByteString -> Warp.Connection -> C.ResourceT IO ())
intercept = interceptWith WS.defaultWebSocketsOptions

-- | Variation of 'intercept' which allows custom options.
interceptWith :: WS.Protocol p
              => WS.WebSocketsOptions
              -> (WS.Request -> WS.WebSockets p ())
              -> Wai.Request
              -> Maybe (C.Source (C.ResourceT IO) ByteString -> Warp.Connection -> C.ResourceT IO ())
interceptWith opts app req = case lookup "upgrade" $ Wai.requestHeaders req of
    Just s
        | S.map toLower s == "websocket" -> Just $ runWebSockets opts req' app
        | otherwise                      -> Nothing
    _                                    -> Nothing
  where
    req' = WS.RequestHttpPart (Wai.rawPathInfo req) (Wai.requestHeaders req)
        (Wai.isSecure req)

-- | Internal function to run the WebSocket iteratee using the conduit library
runWebSockets :: WS.Protocol p
              => WS.WebSocketsOptions
              -> WS.RequestHttpPart
              -> (WS.Request -> WS.WebSockets p ())
              -> C.Source (C.ResourceT IO) ByteString
              -> Warp.Connection
              -> C.ResourceT IO ()
runWebSockets opts req app source conn = do
    step <- liftIO $ E.runIteratee $ WS.runWebSocketsWith opts req app send
    source C.$$ C.sinkState (E.returnI step) push close
  where
    send  = iterConnection conn

    push iter bs = do
        step <- liftIO $ E.runIteratee $ E.enumList 1 [bs] E.$$ iter
        case step of
            E.Continue _    -> return $ C.StateProcessing $ E.returnI step
            E.Yield out inp -> return $ C.StateDone (streamToMaybe inp) out
            E.Error e       -> C.monadThrow e
    close iter   = do
        _ <- liftIO $ E.runIteratee $ E.enumEOF E.$$ iter
        return ()

iterConnection :: Warp.Connection -> E.Iteratee ByteString IO ()
iterConnection c = E.continue go
  where
    go (E.Chunks []) = E.continue go
    go (E.Chunks cs) = E.tryIO (Warp.connSendMany c cs) >> E.continue go
    go E.EOF         = E.continue go

streamToMaybe :: E.Stream S.ByteString -> Maybe S.ByteString
streamToMaybe E.EOF         = Nothing
streamToMaybe (E.Chunks bs) = Just $ S.concat bs
