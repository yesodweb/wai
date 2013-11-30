{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.WebSockets
    ( intercept
    , interceptWith
    ) where

import              Control.Monad.IO.Class          (liftIO)
import              Blaze.ByteString.Builder        (Builder)
import qualified    Blaze.ByteString.Builder        as Builder
import              Data.ByteString                 (ByteString)
import qualified    Data.ByteString.Char8           as BC
import              Data.Char                       (toLower)
import              Data.Conduit
import qualified    Network.Wai                     as Wai
import qualified    Network.Wai.Handler.Warp        as Warp
import qualified    Network.WebSockets              as WS
import qualified    Network.WebSockets.Connection   as WS
import              System.IO.Streams               (InputStream, OutputStream)
import qualified    System.IO.Streams               as Streams

--------------------------------------------------------------------------------
-- | For use with 'settingsIntercept' from the Warp web server.
intercept :: WS.ServerApp
          -> Wai.Request
          -> Maybe (Source (ResourceT IO) ByteString -> Warp.Connection -> ResourceT IO ())
intercept = interceptWith WS.defaultConnectionOptions

--------------------------------------------------------------------------------
-- | Variation of 'intercept' which allows custom options.
interceptWith :: WS.ConnectionOptions
              -> WS.ServerApp
              -> Wai.Request
              -> Maybe (Source (ResourceT IO) ByteString -> Warp.Connection -> ResourceT IO ())
interceptWith opts app req = case lookup "upgrade" (Wai.requestHeaders req) of
    Just s
        | BC.map toLower s == "websocket" -> Just $ runWebSockets opts req' app
        | otherwise                      -> Nothing
    _                                    -> Nothing
    where
        req' = WS.RequestHead (Wai.rawPathInfo req) (Wai.requestHeaders req) (Wai.isSecure req)

--------------------------------------------------------------------------------
---- | Internal function to run the WebSocket io-streams using the conduit library
runWebSockets :: WS.ConnectionOptions
              -> WS.RequestHead
              -> WS.ServerApp
              -> Source (ResourceT IO) ByteString
              -> Warp.Connection
              -> ResourceT IO ()
runWebSockets opts req app _ conn = do

    (is, os) <- liftIO $ connectionToStreams conn

    let pc = WS.PendingConnection 
                { WS.pendingOptions     = opts
                , WS.pendingRequest     = req
                , WS.pendingOnAccept    = \_ -> return ()
                , WS.pendingIn          = is
                , WS.pendingOut         = os
                }

    liftIO $ app pc

------------------------------------------------------------------------------
-- | Converts a 'Connection' to an 'InputStream' \/ 'OutputStream' pair. Note that,
-- as is usually the case in @io-streams@, writing a 'Nothing' to the generated
-- 'OutputStream' does not cause the underlying 'Connection' to be closed.
connectionToStreams :: Warp.Connection
                -> IO (InputStream ByteString, OutputStream Builder)
connectionToStreams connection = do
    is <- Streams.makeInputStream input
    os <- Streams.makeOutputStream output
    return $! (is, os)
    
    where
        input = do
            s <- Warp.connRecv connection
            return $! if BC.null s then Nothing else Just s

        output Nothing  = return $! ()
        output (Just s') = if BC.null s then return $! () else Warp.connSendAll connection s
            where s = Builder.toByteString s'