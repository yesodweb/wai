{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.WebSockets
    ( websocketsOr
    , websocketsApp
    , isWebSocketsReq
    , getRequestHead
    , runWebSockets
    ) where

import              Control.Exception               (bracket, tryJust)
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
-- | Returns whether or not the given 'Wai.Request' is a WebSocket request.
isWebSocketsReq :: Wai.Request -> Bool
isWebSocketsReq req =
    fmap CI.mk (lookup "upgrade" $ Wai.requestHeaders req) == Just "websocket"

--------------------------------------------------------------------------------
-- | Upgrade a @websockets@ 'WS.ServerApp' to a @wai@ 'Wai.Application'. Uses
-- the given backup 'Wai.Application' to handle 'Wai.Request's that are not
-- WebSocket requests.
--
-- @
-- websocketsOr opts ws_app backup_app = \\req respond ->
--     __case__ 'websocketsApp' opts ws_app req __of__
--         'Nothing'  -> backup_app req send_response
--         'Just' res -> respond res
-- @
--
-- For example, below is an 'Wai.Application' that sends @"Hello, client!"@ to
-- each connected client.
--
-- @
-- app :: 'Wai.Application'
-- app = 'websocketsOr' 'WS.defaultConnectionOptions' wsApp backupApp
--   __where__
--     wsApp :: 'WS.ServerApp'
--     wsApp pending_conn = do
--         conn <- 'WS.acceptRequest' pending_conn
--         'WS.sendTextData' conn ("Hello, client!" :: 'Data.Text.Text')
--
--     backupApp :: 'Wai.Application'
--     backupApp _ respond = respond $ 'Wai.responseLBS' 'Network.HTTP.Types.status400' [] "Not a WebSocket request"
-- @
websocketsOr :: WS.ConnectionOptions
             -> WS.ServerApp
             -> Wai.Application
             -> Wai.Application
websocketsOr opts app backup req sendResponse =
    case websocketsApp opts app req of
        Nothing -> backup req sendResponse
        Just res -> sendResponse res

--------------------------------------------------------------------------------
-- | Handle a single @wai@ 'Wai.Request' with the given @websockets@
-- 'WS.ServerApp'. Returns 'Nothing' if the 'Wai.Request' is not a WebSocket
-- request, 'Just' otherwise.
--
-- Usually, 'websocketsOr' is more convenient.
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
-- | Internal function to run the WebSocket io-streams using the conduit library.
runWebSockets :: WS.ConnectionOptions
              -> WS.RequestHead
              -> (WS.PendingConnection -> IO a)
              -> IO ByteString
              -> (ByteString -> IO ())
              -> IO a
runWebSockets opts req app src sink = bracket mkStream ensureClose (app . pc)
  where
    ensureClose = tryJust onConnectionException . WS.close
    onConnectionException :: WS.ConnectionException -> Maybe ()
    onConnectionException WS.ConnectionClosed = Just ()
    onConnectionException _                   = Nothing
    mkStream =
        WS.makeStream
            (do
                bs <- src
                return $ if BC.null bs then Nothing else Just bs)
            -- 'mapM_' works here because it's 'return ()' on Nothing
            (mapM_ $ \bl -> mapM_ sink (BL.toChunks bl))

    pc stream = WS.PendingConnection
        { WS.pendingOptions     = opts
        , WS.pendingRequest     = req
        , WS.pendingOnAccept    = \_ -> return ()
        , WS.pendingStream      = stream
        }
