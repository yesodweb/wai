{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.WebSockets
    ( intercept
    , interceptWith
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as S
import Data.Conduit
import qualified Data.Enumerator as E
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

-- | For use with 'settingsIntercept' from the Warp web server.
intercept :: WS.Protocol p
          => (WS.Request -> WS.WebSockets p ())
          -> Wai.Request
          -> Maybe (Source (ResourceT IO) ByteString -> Warp.Connection -> ResourceT IO ())
intercept = interceptWith WS.defaultWebSocketsOptions

-- | Variation of 'intercept' which allows custom options.
interceptWith :: WS.Protocol p
              => WS.WebSocketsOptions
              -> (WS.Request -> WS.WebSockets p ())
              -> Wai.Request
              -> Maybe (Source (ResourceT IO) ByteString -> Warp.Connection -> ResourceT IO ())
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
              -> Source (ResourceT IO) ByteString
              -> Warp.Connection
              -> ResourceT IO ()
runWebSockets opts req app source conn = do
    step <- liftIO $ E.runIteratee $ WS.runWebSocketsWith opts req app send
    source $$ sink (E.returnI step)
  where
    send  = iterConnection conn

    sink iter = await >>= maybe (close iter) (push iter)

    push iter bs = do
        step <- liftIO $ E.runIteratee $ E.enumList 1 [bs] E.$$ iter
        case step of
            E.Continue _    -> sink $ E.returnI step
            E.Yield out inp -> maybe (return ()) leftover (streamToMaybe inp) >> return out
            E.Error e       -> liftIO $ monadThrow e
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
