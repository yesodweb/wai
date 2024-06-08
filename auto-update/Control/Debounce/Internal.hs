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
    newEmptyMVar,
    tryPutMVar,
    tryTakeMVar,
 )
import Control.Exception (SomeException, handle, mask_)
import Control.Monad (void)

import Debug.Trace (trace)

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
-- Example of how this style debounce works:
--
-- > ! = function execution
-- > . = cooldown period
-- > X = debounced code execution
-- >
-- > !    !    !  !
-- >  ....... ....... .......
-- > X       X       X
--
-- @since 0.1.6
leadingEdge :: DebounceEdge
leadingEdge = Leading

-- | Start a cooldown period and perform the action when the period ends. If another trigger
-- happens during the cooldown, it has no effect.
--
-- Example of how this style debounce works:
--
-- @
-- ! = function execution
-- . = cooldown period
-- X = debounced code execution
--
-- !     !     !  !
--  .......     .......
--         X           X
-- @
--
-- @since 0.1.6
trailingEdge :: DebounceEdge
trailingEdge = Trailing

mkDebounceInternal
    :: MVar () -> (Int -> IO ()) -> DebounceSettings -> IO (IO ())
-- \* LEADING
--
--   1) try put baton to start
--   2) succes -> start worker, failed -> try put trigger
--   3) worker try take trigger
--   4) do action
--   5) delay
--   7) try take trigger
--   8) success -> repeat action, failed -> void $ try take baton
mkDebounceInternal baton delayFn (DebounceSettings freq action Leading) = do
    trigger <- newEmptyMVar
    pure $ do
        success <- tryTakeMVar baton
        case success of
            -- Why the F does this fail if I remove the 'trace'?!
            Nothing -> trace "" $ void $ tryPutMVar trigger ()
            Just () -> startWorker trigger
  where
    startWorker trigger =
        let loop = do
                ignoreExc action
                delayFn freq
                isTriggered <- tryTakeMVar trigger
                case isTriggered of
                    Nothing -> void $ tryPutMVar baton ()
                    Just () -> loop
         in mask_ $ void $ forkIO $ do
                void $ tryTakeMVar trigger
                loop
-- \* TRAILING
--
--   1) try put baton to start
--   2) success -> start worker, failed -> die
--   3) worker delay
--   4) do action
--   5) void $ try take baton
mkDebounceInternal baton delayFn (DebounceSettings freq action Trailing) =
    pure $ do
        success <- tryTakeMVar baton
        case success of
            Nothing -> pure ()
            Just () ->
                mask_ $ void $ forkIO $ do
                    delayFn freq
                    ignoreExc action
                    void $ tryPutMVar baton ()

ignoreExc :: IO () -> IO ()
ignoreExc = handle $ \(_ :: SomeException) -> return ()
