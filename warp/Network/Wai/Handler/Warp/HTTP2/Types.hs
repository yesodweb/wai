{-# LANGUAGE OverloadedStrings, CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.Wai.Handler.Warp.HTTP2.Types where

import Data.ByteString.Builder (Builder)
#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>),(<*>))
#endif
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM
import Control.Exception (SomeException)
import Control.Monad (void)
import Control.Reaper
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap, IntMap)
import qualified Data.IntMap.Strict as M
import qualified Network.HTTP.Types as H
import Network.Wai (Request, FilePart)
import Network.Wai.HTTP2 (PushPromise, Trailers)
import Network.Wai.Handler.Warp.IORef
import Network.Wai.Handler.Warp.Types

import Network.HTTP2
import Network.HTTP2.Priority
import Network.HPACK

----------------------------------------------------------------

http2ver :: H.HttpVersion
http2ver = H.HttpVersion 2 0

isHTTP2 :: Transport -> Bool
isHTTP2 TCP = False
isHTTP2 tls = useHTTP2
  where
    useHTTP2 = case tlsNegotiatedProtocol tls of
        Nothing    -> False
        Just proto -> "h2-" `BS.isPrefixOf` proto || proto == "h2"

----------------------------------------------------------------

data Input = Input Stream Request

----------------------------------------------------------------

data Control a = CFinish
               | CNext a
               | CNone

instance Show (Control a) where
    show CFinish   = "CFinish"
    show (CNext _) = "CNext"
    show CNone     = "CNone"

type DynaNext = WindowSize -> IO Next

type BytesFilled = Int

data Next = Next BytesFilled (Control DynaNext)

data Output = OFinish
            -- ^ Terminate the connection.
            | OGoaway ByteString
            -- ^ Send a goaway frame and terminate the connection.
            | OFrame  ByteString
            -- ^ Send an entire pre-encoded frame.
            | OResponse Stream H.Status H.ResponseHeaders Aux
            -- ^ Send the headers and as much of the response as is immediately
            -- available.
            | OPush Stream PushPromise (MVar Bool) Stream H.Status H.ResponseHeaders Aux
            -- ^ Send a PUSH_PROMISE frame, then act like OResponse; signal the
            -- MVar whether the promise has been sent.
            | ONext Stream DynaNext
            -- ^ Send a chunk of the response.
            | OTrailers Stream Trailers
            -- ^ Send a series of trailers in header and continuation frames.

outputStream :: Output -> Stream
outputStream (OResponse strm _ _ _)   = strm
outputStream (ONext strm _)           = strm
outputStream (OPush strm _ _ _ _ _ _) = strm
outputStream (OTrailers strm _)       = strm
outputStream _                        = error "outputStream"

----------------------------------------------------------------

-- | An element on the queue between a running stream and the sender; the order
-- should consist of any number of 'SFile', 'SBuilder', and 'SFlush', followed
-- by a single 'SFinish'.
data Sequence = SFinish Trailers
              -- ^ The stream is over; its trailers are provided.
              | SFlush
              -- ^ Any buffered data should be sent immediately.
              | SBuilder Builder
              -- ^ Append a chunk of data to the stream.
              | SFile FilePath (Maybe FilePart)
              -- ^ Append a chunk of a file's contents to the stream.

-- | A message from the sender to a stream's dedicated waiter thread.
data Sync = SyncNone
          -- ^ Nothing interesting has happened.  Go back to sleep.
          | SyncFinish Trailers
          -- ^ The stream has ended; enqueue 'OTrailers' and quit.
          | SyncNext Output
          -- ^ The stream's queue has been drained; wait for more to be
          -- available and re-enqueue the given 'Output'.

-- | Auxiliary information needed to communicate with a running stream: a queue
-- of stream elements ('Sequence') and a 'TVar' connected to its waiter thread.
data Aux = Persist (TBQueue Sequence) (TVar Sync)

----------------------------------------------------------------

-- | The context for HTTP/2 connection.
data Context = Context {
    http2settings      :: IORef Settings
  , streamTable        :: StreamTable
  -- | Number of active streams initiated by the client; for enforcing our own
  -- max concurrency setting.
  , concurrency        :: IORef Int
  -- | Number of active streams initiated by the server; for respecting the
  -- client's max concurrency setting.
  , pushConcurrency    :: IORef Int
  -- | RFC 7540 says "Other frames (from any stream) MUST NOT
  --   occur between the HEADERS frame and any CONTINUATION
  --   frames that might follow". This field is used to implement
  --   this requirement.
  , continued          :: IORef (Maybe StreamId)
  , currentStreamId    :: IORef StreamId
  -- ^ Last client-initiated stream ID we've handled.
  , nextPushStreamId   :: IORef StreamId
  -- ^ Next available server-initiated stream ID.
  , inputQ             :: TQueue Input
  , outputQ            :: PriorityTree Output
  , encodeDynamicTable :: IORef DynamicTable
  , decodeDynamicTable :: IORef DynamicTable
  , connectionWindow   :: TVar WindowSize
  }

----------------------------------------------------------------

newContext :: IO Context
newContext = Context <$> newIORef defaultSettings
                     <*> initialize 10 -- fixme: hard coding: 10
                     <*> newIORef 0
                     <*> newIORef 0
                     <*> newIORef Nothing
                     <*> newIORef 0
                     <*> newIORef 2 -- first server push stream; 0 is reserved
                     <*> newTQueueIO
                     <*> newPriorityTree
                     <*> (newDynamicTableForEncoding defaultDynamicTableSize >>= newIORef)
                     <*> (newDynamicTableForDecoding defaultDynamicTableSize >>= newIORef)
                     <*> newTVarIO defaultInitialWindowSize

clearContext :: Context -> IO ()
clearContext ctx = void $ reaperStop $ streamTable ctx

----------------------------------------------------------------

data OpenState =
    JustOpened
  | Continued [HeaderBlockFragment]
              Int  -- Total size
              Int  -- The number of continuation frames
              Bool -- End of stream
              Priority
  | NoBody HeaderList Priority
  | HasBody HeaderList Priority
  | Body (TQueue ByteString)

data ClosedCode = Finished
                | Killed
                | Reset ErrorCodeId
                | ResetByMe SomeException
                deriving Show

data StreamState =
    Idle
  | Open OpenState
  | HalfClosed
  | Closed ClosedCode

isIdle :: StreamState -> Bool
isIdle Idle = True
isIdle _    = False

isOpen :: StreamState -> Bool
isOpen Open{} = True
isOpen _      = False

isHalfClosed :: StreamState -> Bool
isHalfClosed HalfClosed = True
isHalfClosed _          = False

isClosed :: StreamState -> Bool
isClosed Closed{} = True
isClosed _        = False

instance Show StreamState where
    show Idle        = "Idle"
    show Open{}      = "Open"
    show HalfClosed  = "HalfClosed"
    show (Closed e)  = "Closed: " ++ show e

----------------------------------------------------------------

data Stream = Stream {
    streamNumber        :: StreamId
  , streamState         :: IORef StreamState
  -- Next two fields are for error checking.
  , streamContentLength :: IORef (Maybe Int)
  , streamBodyLength    :: IORef Int
  , streamWindow        :: TVar WindowSize
  , streamPriority      :: IORef Priority
  -- | The concurrency IORef in which this stream has been counted.  The client
  -- and server each have separate concurrency values to respect, so pushed
  -- streams need to decrement a different count when they're closed.  This
  -- should be either @concurrency ctx@ or @pushConcurrency ctx@.
  , concurrencyRef      :: IORef Int
  }

instance Show Stream where
  show s = show (streamNumber s)

newStream :: IORef Int -> StreamId -> WindowSize -> IO Stream
newStream ref sid win =
    Stream sid <$> newIORef Idle
               <*> newIORef Nothing
               <*> newIORef 0
               <*> newTVarIO win
               <*> newIORef defaultPriority
               <*> pure ref

----------------------------------------------------------------

opened :: Stream -> IO ()
opened Stream{concurrencyRef,streamState} = do
    atomicModifyIORef' concurrencyRef (\x -> (x+1,()))
    writeIORef streamState (Open JustOpened)

closed :: Stream -> ClosedCode -> IO ()
closed Stream{concurrencyRef,streamState} cc = do
    atomicModifyIORef' concurrencyRef (\x -> (x-1,()))
    writeIORef streamState (Closed cc)

----------------------------------------------------------------

type StreamTable = Reaper (IntMap Stream) (M.Key, Stream)

initialize :: Int -> IO StreamTable
initialize duration = mkReaper settings
  where
    settings = defaultReaperSettings {
          reaperAction = clean
        , reaperDelay  = duration * 1000000
        , reaperCons   = uncurry M.insert
        , reaperNull   = M.null
        , reaperEmpty  = M.empty
        }

clean :: IntMap Stream -> IO (IntMap Stream -> IntMap Stream)
clean old = do
    new <- M.fromAscList <$> prune oldlist []
    return $ M.union new
  where
    oldlist = M.toDescList old
    prune []     lst = return lst
    prune (x@(_,s):xs) lst = do
        st <- readIORef (streamState s)
        if isClosed st then
            prune xs lst
          else
            prune xs (x:lst)

insert :: StreamTable -> M.Key -> Stream -> IO ()
insert strmtbl k v = reaperAdd strmtbl (k,v)

search :: StreamTable -> M.Key -> IO (Maybe Stream)
search strmtbl k = M.lookup k <$> reaperRead strmtbl


-- INVARIANT: streams in the output queue have non-zero window size.
enqueueWhenWindowIsOpen :: PriorityTree Output -> Output -> IO ()
enqueueWhenWindowIsOpen outQ out = do
    let strm = outputStream out
    atomically $ do
        x <- readTVar $ streamWindow strm
        check (x > 0)
    pri <- readIORef $ streamPriority strm
    enqueue outQ out pri

enqueueOrSpawnTemporaryWaiter :: Stream -> PriorityTree Output -> Output -> IO ()
enqueueOrSpawnTemporaryWaiter strm outQ out = do
    sw <- atomically $ readTVar $ streamWindow strm
    if sw == 0 then
        -- This waiter waits only for the stream window.
        void $ forkIO $ enqueueWhenWindowIsOpen outQ out
      else do
        pri <- readIORef $ streamPriority strm
        enqueue outQ out pri
