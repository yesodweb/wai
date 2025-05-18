{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module System.TimeManager (
    -- ** Types
    Manager,
    defaultManager,
    TimeoutAction,
    Handle,

    -- ** Manager
    initialize,
    stopManager,
    killManager,
    withManager,
    withManager',

    -- ** Registering a timeout action
    withHandle,
    withHandleKillThread,

    -- ** Control timeout
    tickle,
    pause,
    resume,

    -- ** Low level
    register,
    registerKillThread,
    cancel,

    -- ** Exceptions
    TimeoutThread (..),
) where

import Control.Concurrent (mkWeakThreadId, myThreadId)
import qualified Control.Exception as E
import Control.Monad (void)
import Control.Reaper
import Data.IORef (IORef)
import qualified Data.IORef as I
import Data.Typeable (Typeable)
import System.IO.Unsafe
import System.Mem.Weak (deRefWeak)

----------------------------------------------------------------

-- | A timeout manager
data Manager = Manager (Reaper [Handle] Handle) | NoManager

-- | No manager.
defaultManager :: Manager
defaultManager = NoManager

-- | An action to be performed on timeout.
type TimeoutAction = IO ()

-- | A handle used by a timeout manager.
data Handle = Handle
    { handleManager :: Manager
    , handleActionRef :: IORef TimeoutAction
    , handleStateRef :: IORef State
    }

emptyAction :: IORef TimeoutAction
emptyAction = unsafePerformIO $ I.newIORef (return ())

emptyState :: IORef State
emptyState = unsafePerformIO $ I.newIORef Inactive

emptyHandle :: Handle
emptyHandle =
    Handle
        { handleManager = NoManager
        , handleActionRef = emptyAction
        , handleStateRef = emptyState
        }

data State
    = Active -- Manager turns it to Inactive.
    | Inactive -- Manager removes it with timeout action.
    | Paused -- Manager does not change it.

----------------------------------------------------------------

-- | Creating timeout manager which works every N micro seconds
--   where N is the first argument.
initialize :: Int -> IO Manager
initialize timeout
    | timeout <= 0 = return NoManager
initialize timeout =
    Manager
        <$> mkReaper
            defaultReaperSettings
                { -- Data.Set cannot be used since 'partition' cannot be used
                  -- with 'readIORef`. So, let's just use a list.
                  reaperAction = mkListAction prune
                , reaperDelay = timeout
                , reaperThreadName = "WAI timeout manager (Reaper)"
                }
  where
    prune m@Handle{..} = do
        state <- I.atomicModifyIORef' handleStateRef (\x -> (inactivate x, x))
        case state of
            Inactive -> do
                onTimeout <- I.readIORef handleActionRef
                onTimeout `E.catch` ignoreSync
                return Nothing
            _ -> return $ Just m

    inactivate Active = Inactive
    inactivate x = x

----------------------------------------------------------------

-- | Stopping timeout manager with onTimeout fired.
stopManager :: Manager -> IO ()
stopManager NoManager = return ()
stopManager (Manager mgr) = E.mask_ (reaperStop mgr >>= mapM_ fire)
  where
    fire Handle{..} = do
        onTimeout <- I.readIORef handleActionRef
        onTimeout `E.catch` ignoreSync

-- | Killing timeout manager immediately without firing onTimeout.
killManager :: Manager -> IO ()
killManager NoManager = return ()
killManager (Manager mgr) = reaperKill mgr

----------------------------------------------------------------

-- | Registering a timeout action and unregister its handle
--   when the body action is finished.
--   'Nothing' is returned on timeout.
withHandle :: Manager -> TimeoutAction -> (Handle -> IO a) -> IO (Maybe a)
withHandle mgr onTimeout action =
    E.handle ignore $ E.bracket (register mgr onTimeout) cancel $ \th ->
        Just <$> action th
  where
    ignore TimeoutThread = return Nothing

-- | Registering a timeout action of killing this thread and
--   unregister its handle when the body action is killed or finished.
withHandleKillThread :: Manager -> TimeoutAction -> (Handle -> IO ()) -> IO ()
withHandleKillThread mgr onTimeout action =
    E.handle ignore $ E.bracket (registerKillThread mgr onTimeout) cancel action
  where
    ignore TimeoutThread = return ()

----------------------------------------------------------------

-- | Registering a timeout action.
register :: Manager -> TimeoutAction -> IO Handle
register NoManager _ = return emptyHandle
register m@(Manager mgr) !onTimeout = do
    actionRef <- I.newIORef onTimeout
    stateRef <- I.newIORef Active
    let h =
            Handle
                { handleManager = m
                , handleActionRef = actionRef
                , handleStateRef = stateRef
                }
    reaperAdd mgr h
    return h

-- | Removing the 'Handle' from the 'Manager' immediately.
cancel :: Handle -> IO ()
cancel Handle{..} = case handleManager of
    NoManager -> return ()
    Manager mgr -> void $ reaperModify mgr filt
  where
    -- It's very important that this function forces the whole workload so we
    -- don't retain old handles, otherwise disasterous leaks occur.
    filt [] = []
    filt (h@(Handle _ _ ref) : hs)
        | handleStateRef == ref = hs
        | otherwise =
            let !hs' = filt hs
             in h : hs'

----------------------------------------------------------------

-- | The asynchronous exception thrown if a thread is registered via
-- 'registerKillThread'.
data TimeoutThread = TimeoutThread
    deriving (Typeable)

instance E.Exception TimeoutThread where
    toException = E.asyncExceptionToException
    fromException = E.asyncExceptionFromException
instance Show TimeoutThread where
    show TimeoutThread = "Thread killed by timeout manager"

-- | Registering a timeout action of killing this thread.
--   'TimeoutThread' is thrown to the thread which called this
--   function on timeout. Catch 'TimeoutThread' if you don't
--   want to leak the asynchronous exception to GHC RTS.
registerKillThread :: Manager -> TimeoutAction -> IO Handle
registerKillThread m onTimeout = do
    tid <- myThreadId
    wtid <- mkWeakThreadId tid
    -- First run the timeout action in case the child thread is masked.
    register m $
        onTimeout `E.finally` do
            mtid <- deRefWeak wtid
            case mtid of
                Nothing -> return ()
                Just tid' -> E.throwTo tid' TimeoutThread

----------------------------------------------------------------

-- | Setting the state to active.
--   'Manager' turns active to inactive repeatedly.
tickle :: Handle -> IO ()
tickle Handle{..} = I.writeIORef handleStateRef Active

-- | Setting the state to paused.
--   'Manager' does not change the value.
pause :: Handle -> IO ()
pause Handle{..} = I.writeIORef handleStateRef Paused

-- | Setting the paused state to active.
--   This is an alias to 'tickle'.
resume :: Handle -> IO ()
resume = tickle

----------------------------------------------------------------

-- | Call the inner function with a timeout manager.
--   'stopManager' is used after that.
withManager
    :: Int
    -- ^ timeout in microseconds
    -> (Manager -> IO a)
    -> IO a
withManager timeout f =
    E.bracket
        (initialize timeout)
        stopManager
        f

-- | Call the inner function with a timeout manager.
--   'killManager' is used after that.
withManager'
    :: Int
    -- ^ timeout in microseconds
    -> (Manager -> IO a)
    -> IO a
withManager' timeout f =
    E.bracket
        (initialize timeout)
        killManager
        f

----------------------------------------------------------------

isAsyncException :: E.Exception e => e -> Bool
isAsyncException e =
    case E.fromException (E.toException e) of
        Just (E.SomeAsyncException _) -> True
        Nothing -> False

ignoreSync :: E.SomeException -> IO ()
ignoreSync se
    | isAsyncException se = E.throwIO se
    | otherwise = return ()
