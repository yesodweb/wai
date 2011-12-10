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
import Network.Wai.Handler.Warp (iterSocket, Handle)

intercept :: Protocol p
          => (WS.Request -> WebSockets p ())
          -> Request
          -> Maybe (Socket -> Handle -> Iteratee ByteString IO ())
intercept app req =
    case lookup "upgrade" $ requestHeaders req of
        Just s | S.map toLower s=="websocket" -> Just $ \socket th -> do
            runWebSockets req' app (iterSocket th socket)
        _ -> Nothing
  where
    req' = RequestHttpPart (rawPathInfo req) (requestHeaders req)
