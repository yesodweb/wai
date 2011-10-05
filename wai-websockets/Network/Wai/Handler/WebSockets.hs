{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.WebSockets
    ( intercept
    ) where

import Network.Wai (Request, requestHeaders)
import Network.Socket (Socket)
import Data.Enumerator (Iteratee, Stream (Chunks), yield)
import Data.ByteString (ByteString)
import Network.WebSockets (WebSockets, runWebSockets)
import Network.Socket.Enumerator (iterSocket)
import Data.Monoid (mappend)
import Blaze.ByteString.Builder (toLazyByteString, copyByteString)
import Data.CaseInsensitive (original)
import Data.ByteString.Lazy (toChunks)

intercept :: WebSockets ()
          -> Request
          -> Maybe (Socket -> Iteratee ByteString IO ())
intercept app req =
    case lookup "upgrade" $ requestHeaders req of
        Just "websocket" -> Just $ \socket -> do
            -- put back the request in the buffer
            yield () $ Chunks $ toChunks $ toLazyByteString reqBuilder
            runWebSockets app (iterSocket socket)
        _ -> Nothing
  where
    reqBuilder =
        copyByteString "GET / HTTP/1.1\r\n" `mappend` -- dummy value
        foldr mappend (copyByteString "\r\n") headers
    headers = map headerToBuilder $ requestHeaders req
    headerToBuilder (k, v) =
        copyByteString (original k)
        `mappend` copyByteString ": "
        `mappend` copyByteString v
        `mappend` copyByteString "\r\n"
