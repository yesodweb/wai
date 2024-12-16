{-# LANGUAGE RecordWildCards #-}

module Control.AutoUpdate.Event (
    -- * Creation
    mkAutoUpdate,
    mkAutoUpdateWithModify,
)
where

import Control.Concurrent.STM
import Control.Monad
import Data.IORef
import GHC.Event (getSystemTimerManager, registerTimeout, unregisterTimeout)

import Control.AutoUpdate.Internal
import Control.AutoUpdate.Types

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
mkAutoUpdateWithModify us f = mkAutoUpdateThingsWithModify (\g _ _ -> g) us f

mkAutoUpdateThings
    :: (IO a -> IO () -> UpdateState a -> b) -> UpdateSettings a -> IO b
mkAutoUpdateThings mk settings@UpdateSettings{..} =
    mkAutoUpdateThingsWithModify mk settings (const updateAction)

mkAutoUpdateThingsWithModify
    :: (IO a -> IO () -> UpdateState a -> b) -> UpdateSettings a -> (a -> IO a) -> IO b
mkAutoUpdateThingsWithModify mk settings update1 = do
    us <- openUpdateState settings update1
    pure $ mk (getUpdateResult us) (closeUpdateState us) us

--------------------------------------------------------------------------------

mkDeleteTimeout :: TVar Bool -> Int -> IO (IO ())
mkDeleteTimeout thc micro = do
    mgr <- getSystemTimerManager
    key <- registerTimeout mgr micro (atomically $ writeTVar thc True)
    pure $ unregisterTimeout mgr key

openUpdateState :: UpdateSettings a -> (a -> IO a) -> IO (UpdateState a)
openUpdateState UpdateSettings{..} update1 = do
    thc <- newTVarIO False
    UpdateState update1
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
