{-# LANGUAGE OverloadedStrings, CPP #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Network.Wai.Handler.Warp.HTTP2.Types where

import Data.ByteString.Builder (Builder)
#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>),(<*>))
#endif
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException)
import Control.Monad (void)
import Control.Reaper
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap, IntMap)
import qualified Data.IntMap.Strict as M
import qualified Network.HTTP.Types as H
import Network.Wai (Request, Response)
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
        Just proto -> "h2-" `BS.isPrefixOf` proto

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
            | OGoaway ByteString
            | OFrame  ByteString
            | OResponse Stream Response Aux
            | ONext Stream DynaNext

outputStream :: Output -> Stream
outputStream (OResponse strm _ _) = strm
outputStream (ONext strm _)       = strm
outputStream _                    = error "outputStream"

----------------------------------------------------------------

data Sequence = SFinish
              | SFlush
              | SBuilder Builder

data Sync = SyncNone
          | SyncFinish
          | SyncNext Output

data Aux = Oneshot Bool
         | Persist (TBQueue Sequence) (TVar Sync)

----------------------------------------------------------------

-- | The context for HTTP/2 connection.
data Context = Context {
    http2settings      :: IORef Settings
  , streamTable        :: StreamTable
  , concurrency        :: IORef Int
  , priorityTreeSize   :: IORef Int
  -- | RFC 7540 says "Other frames (from any stream) MUST NOT
  --   occur between the HEADERS frame and any CONTINUATION
  --   frames that might follow". This field is used to implement
  --   this requirement.
  , continued          :: IORef (Maybe StreamId)
  , currentStreamId    :: IORef StreamId
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
  , streamPrecedence    :: IORef Precedence
  , streamLogger        :: IORef (IO ())
  }

instance Show Stream where
  show s = show (streamNumber s)

newStream :: StreamId -> WindowSize -> IO Stream
newStream sid win = Stream sid <$> newIORef Idle
                               <*> newIORef Nothing
                               <*> newIORef 0
                               <*> newTVarIO win
                               <*> newIORef defaultPrecedence
                               <*> newIORef (return ())

----------------------------------------------------------------

opened :: Context -> Stream -> IO ()
opened Context{concurrency} Stream{streamState} = do
    atomicModifyIORef' concurrency (\x -> (x+1,()))
    writeIORef streamState (Open JustOpened)

closed :: Context -> Stream -> ClosedCode -> IO ()
closed Context{concurrency} Stream{streamState} cc = do
    atomicModifyIORef' concurrency (\x -> (x-1,()))
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
    let Stream{..} = outputStream out
    atomically $ do
        x <- readTVar streamWindow
        check (x > 0)
    pre <- readIORef streamPrecedence
    enqueue outQ streamNumber pre out

enqueueOrSpawnTemporaryWaiter :: Stream -> PriorityTree Output -> Output -> IO ()
enqueueOrSpawnTemporaryWaiter Stream{..} outQ out = do
    sw <- atomically $ readTVar streamWindow
    if sw == 0 then
        -- This waiter waits only for the stream window.
        void $ forkIO $ enqueueWhenWindowIsOpen outQ out
      else do
        pre <- readIORef streamPrecedence
        enqueue outQ streamNumber pre out
