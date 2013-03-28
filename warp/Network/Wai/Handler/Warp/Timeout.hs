{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnboxedTuples, MagicHash #-}
module Network.Wai.Handler.Warp.Timeout (
    Manager
  , Handle
  , initialize
  , stopManager
  , register
  , registerKillThread
  , tickle
  , pause
  , resume
  , cancel
  , withManager
  , dummyHandle
  ) where

import System.Mem.Weak (deRefWeak)
#if MIN_VERSION_base(4,6,0)
import Control.Concurrent (mkWeakThreadId)
#else
import GHC.Weak (Weak (..))
import GHC.Conc.Sync (ThreadId (..))
import GHC.IO (IO (IO))
import GHC.Exts (mkWeak#)
#endif
import Control.Concurrent (forkIO, threadDelay, myThreadId, killThread)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import qualified Data.IORef as I
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable (Typeable)

-- | A timeout manager
newtype Manager = Manager (I.IORef [Handle])

-- | A handle used by 'Manager'
--
-- First field is action to be performed on timeout.
data Handle = Handle (IO ()) (I.IORef State)

-- | A dummy @Handle@.
dummyHandle :: Handle
dummyHandle = Handle (return ()) (unsafePerformIO $ I.newIORef Active)

data State = Active | Inactive | Paused | Canceled

initialize :: Int -> IO Manager
initialize timeout = do
    ref <- I.newIORef []
    void . forkIO $ E.handle ignoreStop $ forever $ do
        threadDelay timeout
        ms <- I.atomicModifyIORef ref (\x -> ([], x))
        ms' <- go ms id
        I.atomicModifyIORef ref (\x -> (ms' x, ()))
    return $ Manager ref
  where
    ignoreStop TimeoutManagerStopped = return ()

    go [] front = return front
    go (m@(Handle onTimeout iactive):rest) front = do
        state <- I.atomicModifyIORef iactive (\x -> (go' x, x))
        case state of
            Inactive -> do
                onTimeout `E.catch` ignoreAll
                go rest front
            Canceled -> go rest front
            _ -> go rest (front . (:) m)
    go' Active = Inactive
    go' x = x

data TimeoutManagerStopped = TimeoutManagerStopped
    deriving (Show, Typeable)
instance E.Exception TimeoutManagerStopped

stopManager :: Manager -> IO ()
stopManager (Manager ihandles) = E.mask_ $ do
    -- Put an undefined value in the IORef to kill the worker thread (yes, it's
    -- a bit of a hack)
    !handles <- I.atomicModifyIORef ihandles $ \h -> (E.throw TimeoutManagerStopped, h)
    mapM_ go handles
  where
    go (Handle onTimeout _) = onTimeout `E.catch` ignoreAll

ignoreAll :: E.SomeException -> IO ()
ignoreAll _ = return ()

register :: Manager -> IO () -> IO Handle
register (Manager ref) onTimeout = do
    iactive <- I.newIORef Active
    let h = Handle onTimeout iactive
    I.atomicModifyIORef ref (\x -> (h : x, ()))
    return h

registerKillThread :: Manager -> IO Handle
registerKillThread m = do
    wtid <- myThreadId >>= mkWeakThreadId
    register m $ deRefWeak wtid >>= maybe (return ()) killThread

#if !MIN_VERSION_base(4,6,0)
mkWeakThreadId :: ThreadId -> IO (Weak ThreadId)
mkWeakThreadId t@(ThreadId t#) = IO $ \s ->
   case mkWeak# t# t Nothing s of
      (# s1, w #) -> (# s1, Weak w #)
#endif

tickle, pause, resume, cancel :: Handle -> IO ()
tickle (Handle _ iactive) = I.writeIORef iactive Active
pause (Handle _ iactive) = I.writeIORef iactive Paused
resume = tickle
cancel (Handle _ iactive) = I.writeIORef iactive Canceled

-- | Call the inner function with a timeout manager.
withManager :: Int -- ^ timeout in microseconds
            -> (Manager -> IO a)
            -> IO a
withManager timeout f = do
    -- FIXME when stopManager is available, use it
    man <- initialize timeout
    f man
