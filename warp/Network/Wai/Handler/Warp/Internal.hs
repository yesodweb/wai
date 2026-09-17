{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- |
-- __IMPORTANT NOTICE__
--
-- This module exports internals mainly to provide the @warp-tls@ package
-- with tools to implement what it needs to. This module\/API should /NOT/ be
-- expected to remain stable at all, even between minor releases.
--
-- If you see a use case for these functions or types for other purposes,
-- please create an issue in the repository so that we might add it to the
-- main 'Network.Wai.Handler.Warp' API.
module Network.Wai.Handler.Warp.Internal (
    -- * Settings
    Settings (..),
    ProxyProtocol (..),
    makeSettingsAndCounter,
    makeSettingsAndServerState,

    -- ** Connection counter
    Counter,
    getCount,

    -- ** Server state
    ServerState,
    makeServerState,

    -- * Low level run functions
    runSettingsConnection,
    runSettingsConnectionMaker,
    runSettingsConnectionMakerSecure,
    Transport (..),

    -- * Connection
    Connection (..),
    socketConnection,

    -- ** Receive
    Recv,
    makeGracefulRecv,
    RecvBuf,

    -- ** Buffer
    Buffer,
    BufSize,
    WriteBuffer (..),
    createWriteBuffer,
    allocateBuffer,
    freeBuffer,
    copy,

    -- ** Sendfile
    FileId (..),
    SendFile,
    sendFile,
    readSendFile,

    -- * Version
    warpVersion,

    -- * Data types

    -- |
    --
    -- The internals of 'IndexedHeader' have changed since @3.4.15@, so we
    -- keep exporting it as a type synonym, but it is now a record instead of
    -- an array.
    -- As such there's no more 'requestMaxIndex', but we provide a blank
    -- 'defaultIndexRequestHeader'.
    InternalInfo (..),
    HeaderValue,
    IndexedHeader,
    -- I assume 'requestMaxIndex' was used in case anyone wanted to create
    -- an empty array, so we replace it with 'defaultIndexRequestHeader'.
    defaultIndexRequestHeader,

    -- * Time out manager

    -- |
    --
    -- In order to provide slowloris protection, every connection is supervised
    -- by a watchdog: a thread that watches a record of what the connection is
    -- currently doing and kills it if it stays in one 'Phase' for longer than
    -- that phase is allowed to take.
    --
    -- The rules are the 'phaseBudget' table plus the 'enter' calls that move
    -- between phases. In summary:
    --
    -- * Reading the request line and headers, reading the request body, and
    --   writing a response are each on the clock.
    --
    -- * Header reading gets a 'Total' budget: it must finish within the
    --   timeout however the client chunks it. That is what replaces the
    --   'settingsSlowlorisSize' heuristic on this path.
    --
    -- * User code is not. The connection moves to 'RunningApp' before the
    --   'Network.Wai.Application' is called, and again between the fragments
    --   of a streaming response body, so an application may take as long as
    --   it likes.
    --
    -- * Data successfully sent or received during a timed phase pushes the
    --   deadline out, via 'tick'.
    --
    -- HTTP\/2 connections are still handled by "System.TimeManager" directly,
    -- both here and per-stream inside the @http2@ library; the watchdog parks
    -- in the 'Delegated' phase for those.
    module Network.Wai.Handler.Warp.Watchdog,
    module System.TimeManager,

    -- * File descriptor cache
    module Network.Wai.Handler.Warp.FdCache,

    -- * File information cache
    module Network.Wai.Handler.Warp.FileInfoCache,

    -- * Date
    module Network.Wai.Handler.Warp.Date,

    -- * Request and response
    Source,
    FirstRequest (..),
    recvRequest,
    sendResponse,

    -- * Platform dependent helper functions
    setSocketCloseOnExec,
    windowsThreadBlockHack,

    -- * Misc
    http2server,
    withII,
    serveConnection,
    pReadMaker,
) where

import Network.Socket.BufferPool
import System.TimeManager

import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Counter (Counter, getCount)
import Network.Wai.Handler.Warp.Date
import Network.Wai.Handler.Warp.FdCache
import Network.Wai.Handler.Warp.FileInfoCache
import Network.Wai.Handler.Warp.HTTP2
import Network.Wai.Handler.Warp.HTTP2.File
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.Request
import Network.Wai.Handler.Warp.Response
import Network.Wai.Handler.Warp.Run
import Network.Wai.Handler.Warp.SendFile
import Network.Wai.Handler.Warp.Settings
import Network.Wai.Handler.Warp.Types
import Network.Wai.Handler.Warp.Watchdog
import Network.Wai.Handler.Warp.Windows

type IndexedHeader = IndexedRequestHeader
