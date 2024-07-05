{-# LANGUAGE ScopedTypeVariables #-}

-- | Unstable API which exposes internals for testing.
module Control.Debounce.Internal (
    DebounceSettings (..),
    DebounceEdge (..),
    leadingEdge,
    trailingEdge,
    mkDebounceInternal,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (
    MVar,
    takeMVar,
    tryPutMVar,
    tryTakeMVar,
 )
import Control.Exception (SomeException, handle, mask_)
import Control.Monad (forever, void)
import GHC.Conc.Sync (labelThread)

-- | Settings to control how debouncing should work.
--
-- This should be constructed using 'defaultDebounceSettings' and record
-- update syntax, e.g.:
--
-- @
-- let settings = 'defaultDebounceSettings' { 'debounceAction' = flushLog }
-- @
--
-- @since 0.1.2
data DebounceSettings = DebounceSettings
    { debounceFreq :: Int
    -- ^ Length of the debounce timeout period in microseconds.
    --
    -- Default: 1 second (1000000)
    --
    -- @since 0.1.2
    , debounceAction :: IO ()
    -- ^ Action to be performed.
    --
    -- Note: all exceptions thrown by this action will be silently discarded.
    --
    -- Default: does nothing.
    --
    -- @since 0.1.2
    , debounceEdge :: DebounceEdge
    -- ^ Whether to perform the action on the leading edge or trailing edge of
    -- the timeout.
    --
    -- Default: 'trailingEdge'.
    --
    -- @since 0.1.6
    }

-- | Setting to control whether the action happens at the leading and/or trailing
-- edge of the timeout.
--
-- @since 0.1.6
data DebounceEdge
    = -- | Perform the action immediately, and then begin a cooldown period.
      -- If the trigger happens again during the cooldown, wait until the end of the cooldown
      -- and then perform the action again, then enter a new cooldown period.
      Leading
    | -- | Start a cooldown period and perform the action when the period ends. If another trigger
      -- happens during the cooldown, it has no effect.
      Trailing
    deriving (Show, Eq)

-- | Perform the action immediately, and then begin a cooldown period.
-- If the trigger happens again during the cooldown, wait until the end of the cooldown
-- and then perform the action again, then enter a new cooldown period.
--
-- @since 0.1.6
leadingEdge :: DebounceEdge
leadingEdge = Leading

-- | Start a cooldown period and perform the action when the period ends. If another trigger
-- happens during the cooldown, it has no effect.
--
-- @since 0.1.6
trailingEdge :: DebounceEdge
trailingEdge = Trailing

mkDebounceInternal
    :: MVar () -> (Int -> IO ()) -> DebounceSettings -> IO (IO ())
mkDebounceInternal baton delayFn (DebounceSettings freq action edge) = do
    tid <- mask_ $ forkIO $ forever $ do
        takeMVar baton
        case edge of
            Leading -> do
                ignoreExc action
                delayFn freq
            Trailing -> do
                delayFn freq
                -- Empty the baton of any other activations during the interval
                void $ tryTakeMVar baton
                ignoreExc action
    labelThread tid "Denounce"
    return $ void $ tryPutMVar baton ()

ignoreExc :: IO () -> IO ()
ignoreExc = handle $ \(_ :: SomeException) -> return ()
