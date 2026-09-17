{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end benchmark of the response path: everything 'sendResponse'
-- does per response except the actual socket write (the Connection is a
-- sink). Covers header sanitization, indexing, Server/Date insertion,
-- header composition, chunking, buffer management and timeout handling.
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad (replicateM, replicateM_)
import Criterion.Main
import Data.ByteString.Builder (byteString)
import Data.IORef (newIORef)
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Header as H
import Network.Socket (SockAddr (..))
import Network.Wai (defaultRequest)
import Network.Wai.Internal (Request (..), Response (..))
import qualified System.TimeManager as T

import Network.Wai.Handler.Warp.Buffer (createWriteBuffer)
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.Response (sendResponse)
import Network.Wai.Handler.Warp.ResponseHeader (composeHeader)
import Network.Wai.Handler.Warp.Settings (defaultSettings)
import Network.Wai.Handler.Warp.Types
import Network.Wai.Handler.Warp.Watchdog

main :: IO ()
main = do
    writeBuf <- createWriteBuffer 16384 >>= newIORef
    http2Ref <- newIORef False
    apps <- newTVarIO (0 :: Int)
    let conn =
            Connection
                { connSendMany = \_ -> return ()
                , connSendAll = \_ -> return ()
                , connSendFile = \_ _ _ _ _ -> return ()
                , connClose = return ()
                , connRecv = return ""
                , connRecvBuf = \_ _ -> return True
                , connWriteBuffer = writeBuf
                , connHTTP2 = http2Ref
                , connMySockAddr = SockAddrInet 0 0
                , connAppsInProgress = apps
                }
    mgr <- T.initialize 30000000
    th <- T.register mgr (return ())
    let ii =
            InternalInfo
                { timeoutManager = mgr
                , getDate = return "Fri, 18 Jul 2026 12:00:00 GMT"
                , getFd = \_ -> return (Nothing, return ())
                , getFileInfo = \_ -> ioError (userError "no file info in bench")
                }
        req = defaultRequest{httpVersion = H.http11, requestMethod = H.methodGet}
        reqidxhdr = indexRequestHeader reqHdrs
    -- A real watchdog thread runs for the whole benchmark, so its background
    -- cost is included. The timeout is set absurdly high purely so criterion's
    -- own pauses between benchmarks cannot trip it; the value does not affect
    -- what 'tick' and 'enter' cost.
    withConnWatchdog bigTimeout $ \cs -> do
        let send = sendResponse defaultSettings conn ii cs req reqidxhdr (return "")
        defaultMain
            [ bgroup
                "sendResponse"
                [ bench "builder 4 headers content-length" $ whnfIO $ send (rspB hdrs4)
                , bench "builder 3 headers chunked" $ whnfIO $ send (rspB hdrs3NoCL)
                , bench "builder 20 headers content-length" $ whnfIO $ send (rspB hdrs20)
                , bench "no body 204" $ whnfIO $ send rsp204
                , bench "stream 64 fragments" $ whnfIO $ send (rspS 64)
                ]
            , -- What supervising one connection costs to set up and tear
              -- down. 'sendResponse' above never opens a connection, so this
              -- is the only place the per-connection cost shows up at all.
              --
              -- Serial and concurrent are both here on purpose. Serial is the
              -- worst case and badly unrepresentative: with one connection at
              -- a time the scheduler has nothing to overlap, so every fork and
              -- teardown is a full round trip. A server always has many
              -- connections in flight. The x64 numbers are for 64 connections,
              -- so divide by 64 for the per-connection figure.
              bgroup
                "per-connection supervision"
                [ bench "old: withHandleKillThread, serial" $
                    whnfIO $
                        T.withHandleKillThread mgr (return ()) $ \_ -> return ()
                , bench "new: withConnWatchdog, serial" $
                    whnfIO $
                        withConnWatchdog bigTimeout $ \_ -> return ()
                , bench "old: withHandleKillThread, x64 in flight" $
                    whnfIO $
                        inFlight 64 $
                            T.withHandleKillThread mgr (return ()) $ \_ -> return ()
                , bench "new: withConnWatchdog, x64 in flight" $
                    whnfIO $
                        inFlight 64 $ withConnWatchdog bigTimeout $ \_ -> return ()
                ]
            , -- The timeout primitives on their own. This is the thing that
              -- actually changed, so it is where a regression would hide:
              -- "old" is time-manager 0.3.2 as shipped on master.
              bgroup
                "timer"
                [ bench "old: tickle" $ whnfIO $ T.tickle th
                , bench "new: tick" $ whnfIO $ tick cs
                , bench "new: enter" $ whnfIO $ enter cs SendingResponse
                , -- What sendFragment costs per fragment, before and after.
                  bench "old: resume+pause" $ whnfIO $ T.resume th >> T.pause th
                , bench "new: enter+enter" $
                    whnfIO $
                        enter cs SendingResponse >> enter cs RunningApp
                ]
            , bgroup
                "headers"
                [ bench "composeHeader 5 headers" $
                    whnfIO $
                        composeHeader H.http11 H.status200 hdrs5
                , bench "indexRequestHeader" $ whnf indexRequestHeader reqHdrs
                , bench "indexResponseHeader" $ whnf indexResponseHeader hdrs5
                ]
            ]
  where
    -- Longer than any benchmark run, so criterion's own pauses between
    -- benchmarks cannot trip a watchdog.
    bigTimeout = 86400 * 1000000

    -- Run n copies of an action concurrently and wait for all of them, so the
    -- measurement sees the overlap a real server gets.
    inFlight n act = do
        ds <- replicateM n $ do
            d <- newEmptyMVar
            _ <- forkIO (act >> putMVar d ())
            return d
        mapM_ takeMVar ds

    body = byteString "Hello, World!"
    rspB hs = ResponseBuilder H.status200 hs body
    -- One fragment per write/flush pair, the shape an SSE-style body has.
    rspS n = ResponseStream H.status200 hdrs3NoCL $ \write flush ->
        replicateM_ n (write body >> flush)
    rsp204 = ResponseBuilder H.status204 [] mempty
    reqHdrs =
        [ (H.hHost, "127.0.0.1:3011")
        , (H.hUserAgent, "wrk/4.2.0")
        , (H.hAccept, "*/*")
        , ("Accept-Encoding", "gzip, deflate")
        , (H.hConnection, "keep-alive")
        ]
    hdrs4 =
        [ (H.hContentType, "text/plain; charset=utf-8")
        , (H.hContentLength, "13")
        , (H.hCacheControl, "no-cache")
        , ("X-Request-Id", "0123456789abcdef")
        ]
    hdrs3NoCL =
        [ (H.hContentType, "text/plain; charset=utf-8")
        , (H.hCacheControl, "no-cache")
        , ("X-Request-Id", "0123456789abcdef")
        ]
    -- what composeHeader sees after warp added Server and Date
    hdrs5 =
        (H.hServer, "Warp/3.4.15")
            : (H.hDate, "Fri, 18 Jul 2026 12:00:00 GMT")
            : hdrs3NoCL
    hdrs20 =
        hdrs4
            ++ [ (H.hCacheControl, "private, max-age=0")
               , ("ETag", "\"33a64df551425fcc55e4d42a148795d9f25f89d4\"")
               , (H.hLastModified, "Wed, 21 Oct 2015 07:28:00 GMT")
               , ("X-Frame-Options", "SAMEORIGIN")
               , ("X-Content-Type-Options", "nosniff")
               , ("X-XSS-Protection", "1; mode=block")
               , ("Strict-Transport-Security", "max-age=31536000; includeSubDomains")
               , ("Content-Security-Policy", "default-src 'self'")
               , ("Referrer-Policy", "strict-origin-when-cross-origin")
               , ("Access-Control-Allow-Origin", "*")
               , ("Vary", "Accept-Encoding")
               , ("Set-Cookie", "session=abc123; Path=/; HttpOnly; Secure")
               , ("X-Runtime", "0.012345")
               , ("X-Served-By", "cache-lhr-1234")
               , ("Age", "0")
               , ("Via", "1.1 varnish")
               ]
