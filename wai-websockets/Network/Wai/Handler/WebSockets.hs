{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.WebSockets
    ( websocketsApp
    , websocketsOr
    , isWebSocketsReq
    , getRequestHead
    , runWebSockets
    ) where

import              Data.ByteString                 (ByteString)
import qualified    Data.ByteString.Char8           as BC
import qualified    Data.ByteString.Lazy            as BL
import qualified    Data.CaseInsensitive            as CI
import              Network.HTTP.Types              (status500)
import qualified    Network.Wai                     as Wai
import qualified    Network.WebSockets              as WS
import qualified    Network.WebSockets.Connection   as WS
import qualified    Network.WebSockets.Stream       as WS

--------------------------------------------------------------------------------
isWebSocketsReq :: Wai.Request -> Bool
isWebSocketsReq req =
    fmap CI.mk (lookup "upgrade" $ Wai.requestHeaders req) == Just "websocket"

--------------------------------------------------------------------------------
websocketsOr :: WS.ConnectionOptions
             -> WS.ServerApp
             -> Wai.Application
             -> Wai.Application
websocketsOr opts app backup req sendResponse =
    case websocketsApp opts app req of
        Nothing -> backup req sendResponse
        Just res -> sendResponse res

--------------------------------------------------------------------------------
websocketsApp :: WS.ConnectionOptions
              -> WS.ServerApp
              -> Wai.Request
              -> Maybe Wai.Response
websocketsApp opts app req
    | isWebSocketsReq req =
        Just $ flip Wai.responseRaw backup $ \src sink ->
            runWebSockets opts req' app src sink
    | otherwise = Nothing
  where
    req' = getRequestHead req
    backup = Wai.responseLBS status500 [("Content-Type", "text/plain")]
                "The web application attempted to send a WebSockets response, but WebSockets are not supported by your WAI handler."

--------------------------------------------------------------------------------
getRequestHead :: Wai.Request -> WS.RequestHead
getRequestHead req = WS.RequestHead
    (Wai.rawPathInfo req `BC.append` Wai.rawQueryString req)
    (Wai.requestHeaders req)
    (Wai.isSecure req)

--------------------------------------------------------------------------------
---- | Internal function to run the WebSocket io-streams using the conduit library
runWebSockets :: WS.ConnectionOptions
              -> WS.RequestHead
              -> (WS.PendingConnection -> IO a)
              -> IO ByteString
              -> (ByteString -> IO ())
              -> IO a
runWebSockets opts req app src sink = do
    stream <- WS.makeStream
        (do
            bs <- src
            return $ if BC.null bs then Nothing else Just bs)
        (\mbBl -> case mbBl of
            Nothing -> return ()
            Just bl -> mapM_ sink (BL.toChunks bl))

    let pc = WS.PendingConnection
                { WS.pendingOptions     = opts
                , WS.pendingRequest     = req
                , WS.pendingOnAccept    = \_ -> return ()
                , WS.pendingStream      = stream
                }

    app pc
