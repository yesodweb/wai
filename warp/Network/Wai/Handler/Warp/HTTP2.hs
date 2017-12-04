{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Wai.Handler.Warp.HTTP2 (isHTTP2, http2) where

import Control.Concurrent (forkIO, killThread)
import qualified Control.Exception as E
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
import Network.Wai.Handler.Warp.Imports
import qualified Network.Wai.Handler.Warp.Settings as S (Settings)
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

http2 :: Connection -> InternalInfo1 -> SockAddr -> Transport -> S.Settings -> (BufSize -> IO ByteString) -> Application -> IO ()
http2 conn ii1 addr transport settings readN app = do
    checkTLS
    ok <- checkPreface
    when ok $ do
        ctx <- newContext
        -- Workers, worker manager and timer manager
        mgr <- start settings
        let responder = response settings ctx mgr
            action = worker ctx settings app responder
        setAction mgr action
        -- The number of workers is 3.
        -- This was carefully chosen based on a lot of benchmarks.
        -- If it is 1, we cannot avoid head-of-line blocking.
        -- If it is large, huge memory is consumed and many
        -- context switches happen.
        replicateM_ 3 $ spawnAction mgr
        -- Receiver
        let mkreq = mkRequest ii1 settings addr
        tid <- forkIO $ frameReceiver ctx mkreq readN
        -- Sender
        -- frameSender is the main thread because it ensures to send
        -- a goway frame.
        frameSender ctx conn settings mgr `E.finally` do
            clearContext ctx
            stop mgr
            killThread tid
  where
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
