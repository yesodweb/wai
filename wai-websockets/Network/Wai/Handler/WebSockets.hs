{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.WebSockets
    ( websocketsApp
    , websocketsOr
    ) where

import              Data.ByteString                 (ByteString)
import qualified    Data.ByteString.Char8           as BC
import qualified    Data.CaseInsensitive            as CI
import              Data.Conduit
import              Data.IORef                      (newIORef, readIORef, writeIORef)
import              Network.HTTP.Types              (status500)
import qualified    Network.Wai                     as Wai
import qualified    Network.WebSockets              as WS
import qualified    Network.WebSockets.Connection   as WS
import              System.IO.Streams               (InputStream, OutputStream)
import qualified    System.IO.Streams               as Streams

--------------------------------------------------------------------------------
websocketsOr :: WS.ConnectionOptions
             -> WS.ServerApp
             -> Wai.Application
             -> Wai.Application
websocketsOr opts app backup req =
    case websocketsApp opts app req of
        Nothing -> backup req
        Just res -> return res

--------------------------------------------------------------------------------
websocketsApp :: WS.ConnectionOptions
              -> WS.ServerApp
              -> Wai.Request
              -> Maybe Wai.Response
websocketsApp opts app req
    | fmap CI.mk (lookup "upgrade" $ Wai.requestHeaders req) == Just "websocket" =
        Just $ flip Wai.responseRaw backup $ \src sink ->
            runWebSockets opts req' app src sink
    | otherwise = Nothing
  where
    req' = WS.RequestHead
        (Wai.rawPathInfo req `BC.append` Wai.rawQueryString req)
        (Wai.requestHeaders req)
        (Wai.isSecure req)
    backup = Wai.responseLBS status500 [("Content-Type", "text/plain")]
                "The web application attempted to send a WebSockets response, but WebSockets are not supported by your WAI handler."

--------------------------------------------------------------------------------
---- | Internal function to run the WebSocket io-streams using the conduit library
runWebSockets :: WS.ConnectionOptions
              -> WS.RequestHead
              -> WS.ServerApp
              -> Source IO ByteString
              -> Sink ByteString IO ()
              -> IO ()
runWebSockets opts req app src sink = do

    is <- srcToInput src
    os <- sinkToOutput sink >>= Streams.builderStream

    let pc = WS.PendingConnection
                { WS.pendingOptions     = opts
                , WS.pendingRequest     = req
                , WS.pendingOnAccept    = \_ -> return ()
                , WS.pendingIn          = is
                , WS.pendingOut         = os
                }

    app pc

srcToInput :: Source IO ByteString -> IO (InputStream ByteString)
srcToInput src0 = do
    (rsrc0, ()) <- src0 $$+ return ()
    ref <- newIORef rsrc0
    Streams.makeInputStream $ do
        rsrc <- readIORef ref
        (rsrc', mbs) <- rsrc $$++ await
        writeIORef ref rsrc'
        return mbs

sinkToOutput :: Sink ByteString IO () -> IO (OutputStream ByteString)
sinkToOutput sink =
    Streams.makeOutputStream output
  where
    output Nothing = return ()
    output (Just bs) = yield bs $$ sink
