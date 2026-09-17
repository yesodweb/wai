{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Warp.HTTP1 (
    http1,
) where

import qualified Control.Concurrent as Conc (yield)
import Control.Exception (SomeException, catch, fromException, throwIO, try)
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word8 (_cr, _space)
import Network.Socket (SockAddr (SockAddrInet, SockAddrInet6))
import Network.Wai
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import "iproute" Data.IP (toHostAddress, toHostAddress6)

import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.Imports hiding (readInt)
import Network.Wai.Handler.Warp.ReadInt
import Network.Wai.Handler.Warp.Request
import Network.Wai.Handler.Warp.Response
import Network.Wai.Handler.Warp.Settings
import Network.Wai.Handler.Warp.Types
import Network.Wai.Handler.Warp.Watchdog

http1
    :: Settings
    -> InternalInfo
    -> Connection
    -> Transport
    -> Application
    -> SockAddr
    -> ConnState
    -> ByteString
    -> IO ()
http1 settings ii conn transport app origAddr cs bs0 = do
    istatus <- newIORef True
    src <- mkSource (wrappedRecv conn istatus)
    leftoverSource src bs0
    addr <- getProxyProtocolAddr src
    http1server settings ii conn transport app addr cs istatus src
  where
    -- Every read counts as progress. There is deliberately no minimum size
    -- here: 'ReadingHeaders' is a 'Total' budget, so a client that dribbles
    -- headers runs out of time regardless of how it chunks them, and the
    -- @settingsSlowlorisSize@ heuristic this used to apply is redundant.
    wrappedRecv Connection{connRecv = recv} istatus = do
        bs <- recv
        unless (BS.null bs) $ do
            writeIORef istatus True
            tick cs
        return bs

    getProxyProtocolAddr src =
        case settingsProxyProtocol settings of
            ProxyProtocolNone ->
                return origAddr
            ProxyProtocolRequired -> do
                seg <- readSource src
                parseProxyProtocolHeader src seg
            ProxyProtocolOptional -> do
                seg <- readSource src
                if BS.isPrefixOf "PROXY " seg
                    then parseProxyProtocolHeader src seg
                    else do
                        leftoverSource src seg
                        return origAddr

    parseProxyProtocolHeader src seg = do
        let (header, seg') = BS.break (== _cr) seg
            maybeAddr = case BS.split _space header of
                ["PROXY", "TCP4", clientAddr, _, clientPort, _] ->
                    case [x | (x, t) <- reads (decodeAscii clientAddr), null t] of
                        [a] ->
                            Just
                                ( SockAddrInet
                                    (readInt clientPort)
                                    (toHostAddress a)
                                )
                        _ -> Nothing
                ["PROXY", "TCP6", clientAddr, _, clientPort, _] ->
                    case [x | (x, t) <- reads (decodeAscii clientAddr), null t] of
                        [a] ->
                            Just
                                ( SockAddrInet6
                                    (readInt clientPort)
                                    0
                                    (toHostAddress6 a)
                                    0
                                )
                        _ -> Nothing
                ("PROXY" : "UNKNOWN" : _) ->
                    Just origAddr
                _ ->
                    Nothing
        case maybeAddr of
            Nothing -> throwIO (BadProxyHeader (decodeAscii header))
            Just a -> do
                leftoverSource src (BS.drop 2 seg') -- drop CRLF
                return a

    decodeAscii = map (chr . fromEnum) . BS.unpack

http1server
    :: Settings
    -> InternalInfo
    -> Connection
    -> Transport
    -> Application
    -> SockAddr
    -> ConnState
    -> IORef Bool
    -> Source
    -> IO ()
http1server settings ii conn transport app addr cs istatus src =
    loop FirstRequest `catch` handler
  where
    handler e
        -- See comment below referencing
        -- https://github.com/yesodweb/wai/issues/618
        | Just NoKeepAliveRequest <- fromException e = return ()
        -- No valid request
        | Just (BadFirstLine _) <- fromException e = return ()
        | isAsyncException e = throwIO e
        | otherwise = do
            _ <-
                sendErrorResponse
                    settings
                    ii
                    conn
                    cs
                    istatus
                    defaultRequest{remoteHost = addr}
                    e
            throwIO e

    loop firstRequest = do
        (req, mremainingRef, idxhdr, nextBodyFlush) <-
            recvRequest firstRequest settings conn ii cs addr src transport
        keepAlive <-
            processRequest
                settings
                ii
                conn
                app
                cs
                istatus
                src
                req
                mremainingRef
                idxhdr
                nextBodyFlush
                `catch` \e -> do
                    settingsOnException settings (Just req) e
                    -- Don't throw the error again to prevent calling settingsOnException twice.
                    return CloseConnection

        -- When doing a keep-alive connection, the other side may just
        -- close the connection. We don't want to treat that as an
        -- exceptional situation, so we pass in SubsequentRequest to http1 (which
        -- in turn passes in SubsequentRequest to recvRequest), indicating that
        -- this is not the first request. If, when trying to read the
        -- request headers, no data is available, recvRequest will
        -- throw a NoKeepAliveRequest exception, which we catch here
        -- and ignore. See: https://github.com/yesodweb/wai/issues/618

        case keepAlive of
            ReuseConnection -> loop SubsequentRequest
            CloseConnection -> return ()

data ReuseConnection = ReuseConnection | CloseConnection

processRequest
    :: Settings
    -> InternalInfo
    -> Connection
    -> Application
    -> ConnState
    -> IORef Bool
    -> Source
    -> Request
    -> Maybe (IORef Int)
    -> IndexedRequestHeader
    -> IO ByteString
    -> IO ReuseConnection
processRequest settings ii conn app cs istatus src req mremainingRef idxhdr nextBodyFlush = do
    -- Let the application run for as long as it wants: 'RunningApp' is not
    -- subject to a timeout.
    enter cs RunningApp

    -- In the event that some scarce resource was acquired during
    -- creating the request, we need to make sure that we don't get
    -- an async exception before calling the ResponseSource.
    keepAliveRef <- newIORef $ error "keepAliveRef not filled"
    r <- try $ app req $ \res -> do
        -- The application handed us a response; Warp is back on the clock.
        enter cs SendingResponse
        -- FIXME consider forcing evaluation of the res here to
        -- send more meaningful error messages to the user.
        -- However, it may affect performance.
        writeIORef istatus False
        keepAlive <- sendResponse settings conn ii cs req idxhdr (readSource src) res
        writeIORef keepAliveRef keepAlive
        return ResponseReceived
    case r of
        Right ResponseReceived -> return ()
        Left (e :: SomeException)
            | Just (ExceptionInsideResponseBody e') <- fromException e -> throwIO e'
            | isAsyncException e -> throwIO e
            | otherwise -> do
                keepAlive <- sendErrorResponse settings ii conn cs istatus req e
                settingsOnException settings (Just req) e
                writeIORef keepAliveRef keepAlive

    keepAlive <- readIORef keepAliveRef

    -- We just send a Response and it takes a time to
    -- receive a Request again. If we immediately call recv,
    -- it is likely to fail and cause the IO manager to do some work.
    -- It is very costly, so we yield to another Haskell
    -- thread hoping that the next Request will arrive
    -- when this Haskell thread will be re-scheduled.
    -- This improves performance at least when
    -- the number of cores is small.
    Conc.yield

    if keepAlive
        then -- If there is an unknown or large amount of data to still be read
        -- from the request body, simple drop this connection instead of
        -- reading it all in to satisfy a keep-alive request.
            case settingsMaximumBodyFlush settings of
                Nothing -> do
                    flushEntireBody nextBodyFlush
                    enter cs ReadingHeaders
                    return ReuseConnection
                Just maxToRead -> do
                    let tryKeepAlive = do
                            -- flush the rest of the request body
                            isComplete <- flushBody nextBodyFlush maxToRead
                            if isComplete
                                then do
                                    enter cs ReadingHeaders
                                    return ReuseConnection
                                else return CloseConnection
                    case mremainingRef of
                        Just ref -> do
                            remaining <- readIORef ref
                            if remaining <= maxToRead
                                then tryKeepAlive
                                else return CloseConnection
                        Nothing -> tryKeepAlive
        else return CloseConnection

sendErrorResponse
    :: Settings
    -> InternalInfo
    -> Connection
    -> ConnState
    -> IORef Bool
    -> Request
    -> SomeException
    -> IO Bool
sendErrorResponse settings ii conn cs istatus req e = do
    status <- readIORef istatus
    if shouldSendErrorResponse e && status
        then do
            -- We reach here from inside the application, so the connection is
            -- still in 'RunningApp' and therefore unlimited. Writing the error
            -- response is Warp's own work and belongs on the clock like any
            -- other response. (The old code sent it while the timer was
            -- paused, so this write was unprotected.)
            enter cs SendingResponse
            sendResponse
                settings
                conn
                ii
                cs
                req
                defaultIndexRequestHeader
                (return BS.empty)
                errorResponse
        else return False
  where
    shouldSendErrorResponse se
        | Just ConnectionClosedByPeer <- fromException se = False
        | otherwise = True
    errorResponse = settingsOnExceptionResponse settings e

flushEntireBody :: IO ByteString -> IO ()
flushEntireBody src =
    loop
  where
    loop = do
        bs <- src
        unless (BS.null bs) loop

flushBody
    :: IO ByteString
    -- ^ get next chunk
    -> Int
    -- ^ maximum to flush
    -> IO Bool
    -- ^ True == flushed the entire body, False == we didn't
flushBody src = loop
  where
    loop toRead = do
        bs <- src
        let toRead' = toRead - BS.length bs
        case () of
            ()
                | BS.null bs -> return True
                | toRead' >= 0 -> loop toRead'
                | otherwise -> return False
