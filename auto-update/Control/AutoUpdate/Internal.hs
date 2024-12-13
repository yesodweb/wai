{-# LANGUAGE RecordWildCards #-}

module Control.AutoUpdate.Internal (
    -- * Type
    UpdateSettings (..),
    defaultUpdateSettings,

    -- * Creation
    mkAutoUpdate,
    mkAutoUpdateWithModify,

    -- * Debugging
    mkClosableAutoUpdate,
    mkClosableAutoUpdate',
    UpdateState (..),
)
where

-- GHC packages

import Control.Concurrent.STM
import Control.Monad
import Data.IORef
import GHC.Event (getSystemTimerManager, registerTimeout, unregisterTimeout)

-- | Default value for creating an 'UpdateSettings'.
--
-- @since 0.1.0
defaultUpdateSettings :: UpdateSettings ()
defaultUpdateSettings =
    UpdateSettings
        { updateFreq = 1000000
        , updateSpawnThreshold = 3
        , updateAction = return ()
        , updateThreadName = "AutoUpdate"
        }

-- | Settings to control how values are updated.
--
-- This should be constructed using 'defaultUpdateSettings' and record
-- update syntax, e.g.:
--
-- @
-- let settings = 'defaultUpdateSettings' { 'updateAction' = 'Data.Time.Clock.getCurrentTime' }
-- @
--
-- @since 0.1.0
data UpdateSettings a = UpdateSettings
    { updateFreq :: Int
    -- ^ Microseconds between update calls. Same considerations as
    -- 'threadDelay' apply.
    --
    -- Default: 1000000 microseconds (1 second)
    --
    -- @since 0.1.0
    , updateSpawnThreshold :: Int
    -- ^ Obsoleted field.
    --
    -- @since 0.1.0
    , updateAction :: IO a
    -- ^ Action to be performed to get the current value.
    --
    -- Default: does nothing.
    --
    -- @since 0.1.0
    , updateThreadName :: String
    -- ^ Label of the thread being forked.
    --
    -- Default: @"AutoUpdate"@
    --
    -- @since 0.2.2
    }

-- | Generate an action which will either read from an automatically
-- updated value, or run the update action in the current thread.
--
-- @since 0.1.0
mkAutoUpdate :: UpdateSettings a -> IO (IO a)
mkAutoUpdate = mkAutoUpdateThings $ \g _ _ -> g

-- | Generate an action which will either read from an automatically
-- updated value, or run the update action in the current thread if
-- the first time or the provided modify action after that.
--
-- @since 0.1.4
mkAutoUpdateWithModify :: UpdateSettings a -> (a -> IO a) -> IO (IO a)
mkAutoUpdateWithModify us f = undefined

-- $setup
-- >>> :set -XNumericUnderscores
-- >>> import Control.Concurrent

-- |
-- >>> iref <- newIORef (0 :: Int)
-- >>> action = modifyIORef iref (+ 1) >> readIORef iref
-- >>> (getValue, closeState) <- mkClosableAutoUpdate $ defaultUpdateSettings { updateFreq = 200_000, updateAction = action }
-- >>> getValue
-- 1
-- >>> threadDelay 100_000 >> getValue
-- 1
-- >>> threadDelay 200_000 >> getValue
-- 2
-- >>> closeState
mkClosableAutoUpdate :: UpdateSettings a -> IO (IO a, IO ())
mkClosableAutoUpdate = mkAutoUpdateThings $ \g c _ -> (g, c)

-- | provide `UpdateState` for debugging
mkClosableAutoUpdate' :: UpdateSettings a -> IO (IO a, IO (), UpdateState a)
mkClosableAutoUpdate' = mkAutoUpdateThings (,,)

mkAutoUpdateThings
    :: (IO a -> IO () -> UpdateState a -> b) -> UpdateSettings a -> IO b
mkAutoUpdateThings mk settings = do
    us <- openUpdateState settings
    pure $ mk (getUpdateResult us) (closeUpdateState us) us

--------------------------------------------------------------------------------

{- FOURMOLU_DISABLE -}
data UpdateState a =
    UpdateState
    { usUpdateAction_   :: a -> IO a
    , usLastResult_     :: IORef a
    , usIntervalMicro_  :: Int
    , usTimeHasCome_    :: TVar Bool
    , usDeleteTimeout_  :: IORef (IO ())
    }
{- FOURMOLU_ENABLE -}

mkDeleteTimeout :: TVar Bool -> Int -> IO (IO ())
mkDeleteTimeout thc micro = do
    mgr <- getSystemTimerManager
    key <- registerTimeout mgr micro (atomically $ writeTVar thc True)
    pure $ unregisterTimeout mgr key

openUpdateState :: UpdateSettings a -> IO (UpdateState a)
openUpdateState UpdateSettings{..} = do
    thc <- newTVarIO False
    UpdateState (const updateAction)
        <$> (newIORef =<< updateAction)
        <*> pure updateFreq
        <*> pure thc
        <*> (newIORef =<< mkDeleteTimeout thc updateFreq)

closeUpdateState :: UpdateState a -> IO ()
closeUpdateState UpdateState{..} = do
    delete <- readIORef usDeleteTimeout_
    delete

onceOnTimeHasCome :: UpdateState a -> IO () -> IO ()
onceOnTimeHasCome UpdateState{..} action = do
    action' <- atomically $ do
        timeHasCome <- readTVar usTimeHasCome_
        when timeHasCome $ writeTVar usTimeHasCome_ False
        pure $ when timeHasCome action
    action'

getUpdateResult :: UpdateState a -> IO a
getUpdateResult us@UpdateState{..} = do
    onceOnTimeHasCome us $ do
        writeIORef usLastResult_ =<< usUpdateAction_ =<< readIORef usLastResult_
        writeIORef usDeleteTimeout_ =<< mkDeleteTimeout usTimeHasCome_ usIntervalMicro_
    readIORef usLastResult_
