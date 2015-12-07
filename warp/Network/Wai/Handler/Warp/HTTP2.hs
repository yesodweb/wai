{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Wai.Handler.Warp.HTTP2 (isHTTP2, http2) where

import Control.Concurrent (forkIO, killThread)
import qualified Control.Exception as E
import Control.Monad (when, unless, replicateM_)
import Data.ByteString (ByteString)
import Network.HTTP2
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Handler.Warp.HTTP2.EncodeFrame
import Network.Wai.Handler.Warp.HTTP2.Manager
import Network.Wai.Handler.Warp.HTTP2.Receiver
import Network.Wai.Handler.Warp.HTTP2.Request
import Network.Wai.Handler.Warp.HTTP2.Sender
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.HTTP2.Worker
import qualified Network.Wai.Handler.Warp.Settings as S (Settings)
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

http2 :: Connection -> InternalInfo -> SockAddr -> Transport -> S.Settings -> (BufSize -> IO ByteString) -> Application -> IO ()
http2 conn ii addr transport settings readN app = do
    checkTLS
    ok <- checkPreface
    when ok $ do
        ctx <- newContext
        -- Workers & Manager
        mgr <- start
        let responder = response ctx mgr
            action = worker ctx settings tm app responder
        setAction mgr action
        -- fixme: hard coding: 10
        replicateM_ 10 $ spawnAction mgr
        -- Receiver
        let mkreq = mkRequest ii settings addr
        tid <- forkIO $ frameReceiver ctx mkreq readN
        -- Sender
        -- frameSender is the main thread because it ensures to send
        -- a goway frame.
        frameSender ctx conn ii settings `E.finally` do
            clearContext ctx
            stop mgr
            killThread tid
  where
    tm = timeoutManager ii
    checkTLS = case transport of
        TCP -> return () -- direct
        tls -> unless (tls12orLater tls) $ goaway conn InadequateSecurity "Weak TLS"
    tls12orLater tls = tlsMajorVersion tls == 3 && tlsMinorVersion tls >= 3
    checkPreface = do
        preface <- readN connectionPrefaceLength
        if connectionPreface /= preface then do
            goaway conn ProtocolError "Preface mismatch"
            return False
          else
            return True

-- connClose must not be called here since Run:fork calls it
goaway :: Connection -> ErrorCodeId -> ByteString -> IO ()
goaway Connection{..} etype debugmsg = connSendAll bytestream
  where
    bytestream = goawayFrame 0 etype debugmsg
