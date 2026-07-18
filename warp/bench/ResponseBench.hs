{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end benchmark of the response path: everything 'sendResponse'
-- does per response except the actual socket write (the Connection is a
-- sink). Covers header sanitization, indexing, Server/Date insertion,
-- header composition, chunking, buffer management and timeout handling.
module Main (main) where

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

main :: IO ()
main = do
    writeBuf <- createWriteBuffer 16384 >>= newIORef
    http2Ref <- newIORef False
    apps <- newIORef (0 :: Int)
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
        send = sendResponse defaultSettings conn ii th req reqidxhdr (return "")
    defaultMain
        [ bgroup
            "sendResponse"
            [ bench "builder 4 headers content-length" $ whnfIO $ send (rspB hdrs4)
            , bench "builder 3 headers chunked" $ whnfIO $ send (rspB hdrs3NoCL)
            , bench "builder 20 headers content-length" $ whnfIO $ send (rspB hdrs20)
            , bench "no body 204" $ whnfIO $ send rsp204
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
    body = byteString "Hello, World!"
    rspB hs = ResponseBuilder H.status200 hs body
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
    hdrs5 = (H.hServer, "Warp/3.4.15") : (H.hDate, "Fri, 18 Jul 2026 12:00:00 GMT") : hdrs3NoCL
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
