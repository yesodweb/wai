{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module System.TimeManager (
    -- ** Types
    Manager,
    defaultManager,
    TimeoutAction,
    Handle,
    emptyHandle,

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
import Data.IORef (IORef)
import qualified Data.IORef as I
import Data.Typeable (Typeable)
import System.Mem.Weak (deRefWeak)

#if defined(mingw32_HOST_OS)
import qualified GHC.Event.Windows as EV
#else
import qualified GHC.Event as EV
#endif

----------------------------------------------------------------

-- | A timeout manager
data Manager = NoManager | Manager Int

defaultManager :: Manager
defaultManager = NoManager

----------------------------------------------------------------

-- | An action to be performed on timeout.
type TimeoutAction = IO ()

----------------------------------------------------------------

-- | A handle used by a timeout manager.
data Handle = Handle
    { handleTimeout :: Int
    , handleAction :: TimeoutAction
    , handleKeyRef :: IORef EV.TimeoutKey
    }

----------------------------------------------------------------

-- | Creating timeout manager which works every N microseconds
--   where N is the first argument.
initialize :: Int -> IO Manager
initialize timeout
    | timeout <= 0 = error "initialize"
initialize timeout = return $ Manager timeout

----------------------------------------------------------------

-- | Obsoleted.
stopManager :: Manager -> IO ()
stopManager _ = return ()

-- | Obsoleted.
killManager :: Manager -> IO ()
killManager _ = return ()

----------------------------------------------------------------

-- | Registering a timeout action and unregister its handle
--   when the body action is finished.
--   'Nothing' is returned on timeout.
withHandle :: Manager -> TimeoutAction -> (Handle -> IO a) -> IO (Maybe a)
withHandle NoManager _ _ = error "withHandle: NoManager"
withHandle mgr onTimeout action = do
    E.handle ignore $ E.bracket (register mgr onTimeout) cancel $ \th ->
        Just <$> action th
  where
    ignore TimeoutThread = return Nothing

-- | Registering a timeout action of killing this thread and
--   unregister its handle when the body action is killed or finished.
withHandleKillThread :: Manager -> TimeoutAction -> (Handle -> IO ()) -> IO ()
withHandleKillThread NoManager _ _ = error "withHandleKillThread: NoManager"
withHandleKillThread mgr onTimeout action =
    E.handle ignore $ E.bracket (registerKillThread mgr onTimeout) cancel action
  where
    ignore TimeoutThread = return ()

----------------------------------------------------------------

-- | Registering a timeout action.
register :: Manager -> TimeoutAction -> IO Handle
register NoManager _ = error "register: NoManager"
register (Manager timeout) onTimeout = do
    mgr <- getTimerManager
    key <- EV.registerTimeout mgr timeout onTimeout
    keyref <- I.newIORef key
    let h =
            Handle
                { handleTimeout = timeout
                , handleAction = onTimeout
                , handleKeyRef = keyref
                }
    return h

-- | Unregistering the timeout.
cancel :: Handle -> IO ()
cancel Handle{..} = do
    mgr <- getTimerManager
    key <- I.readIORef handleKeyRef
    EV.unregisterTimeout mgr key

-- | Extending the timeout.
tickle :: Handle -> IO ()
tickle Handle{..} = do
    mgr <- getTimerManager
    key <- I.readIORef handleKeyRef
#if defined(mingw32_HOST_OS)
    EV.updateTimeout mgr key $ fromIntegral (handleTimeout `div` 1000000)
#else
    EV.updateTimeout mgr key handleTimeout
#endif

-- | This is identical to 'cancel'.
--   To resume timeout with the same 'Handle', 'resume' MUST be called.
--   Don't call 'tickle' for resumption.
pause :: Handle -> IO ()
pause = cancel

-- | Resuming the timeout.
resume :: Handle -> IO ()
resume Handle{..} = do
    mgr <- getTimerManager
    key <- EV.registerTimeout mgr handleTimeout handleAction
    I.writeIORef handleKeyRef key

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
                -- FIXME: forkIO to prevent blocking TimerManger
                Just tid' -> E.throwTo tid' TimeoutThread

----------------------------------------------------------------

-- | Call the inner function with a timeout manager.
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
--   This is identical to 'withManager'.
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

#if defined(mingw32_HOST_OS)
getTimerManager :: IO EV.Manager
getTimerManager = EV.getSystemManager
#else
getTimerManager :: IO EV.TimerManager
getTimerManager = EV.getSystemTimerManager
#endif
