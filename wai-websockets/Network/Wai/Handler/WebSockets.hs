{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.WebSockets
    ( intercept
    ) where

import Network.Wai (Request, requestHeaders, rawPathInfo, requestHeaders)
import Network.Socket (Socket)
import Data.Char (toLower)
import Data.Enumerator (Iteratee)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Network.WebSockets (WebSockets, runWebSockets, RequestHttpPart(RequestHttpPart), Protocol)
import qualified Network.WebSockets as WS
import Network.Socket.Enumerator (iterSocket)

intercept :: Protocol p
          => (WS.Request -> WebSockets p ())
          -> Request
          -> Maybe (Socket -> Iteratee ByteString IO ())
intercept app req =
    case lookup "upgrade" $ requestHeaders req of
        Just s | S.map toLower s=="websocket" -> Just $ \socket -> do
            runWebSockets req' app (iterSocket socket)
        _ -> Nothing
  where
    req' = RequestHttpPart (rawPathInfo req) (requestHeaders req)
