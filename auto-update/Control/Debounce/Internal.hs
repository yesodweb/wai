{-# LANGUAGE ScopedTypeVariables #-}

-- | Unstable API which exposes internals for testing.
module Control.Debounce.Internal (
    DebounceSettings (..),
    DebounceEdge (..),
    leadingEdge,
    leadingMuteEdge,
    trailingEdge,
    trailingDelayEdge,
    mkDebounceInternal,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (
    MVar,
    newEmptyMVar,
    putMVar,
    tryPutMVar,
    tryTakeMVar,
 )
import Control.Exception (SomeException, handle, mask_)
import Control.Monad (void, when)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
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
    -- Default: 'leadingEdge'.
    --
    -- @since 0.1.6
    , debounceThreadName :: String
    -- ^ Label of the thread spawned when debouncing.
    --
    -- Default: @"Debounce"@.
    --
    -- @since 0.2.2
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
    | -- | Perform the action immediately, and then begin a cooldown period.
      -- If the trigger happens again during the cooldown, it is ignored.
      LeadingMute
    | -- | Start a cooldown period and perform the action when the period ends. If another trigger
      -- happens during the cooldown, it has no effect.
      Trailing
    | -- | Start a cooldown period and perform the action when the period ends. If another trigger
      -- happens during the cooldown, it restarts the cooldown again.
      TrailingDelay
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
-- > !   !         !            !
-- >  ....... ....... .......    .......
-- > X       X       X          X
--
-- @since 0.1.6
leadingEdge :: DebounceEdge
leadingEdge = Leading

-- | Perform the action immediately, and then begin a cooldown period.
-- If the trigger happens again during the cooldown, it is ignored.
--
-- Example of how this style debounce works:
--
-- > ! = function execution
-- > . = cooldown period
-- > X = debounced code execution
-- >
-- > !   !      !     !
-- >  .......    .......
-- > X          X
--
-- @since 0.1.6
leadingMuteEdge :: DebounceEdge
leadingMuteEdge = LeadingMute

-- | Start a cooldown period and perform the action when the period ends.
-- If another trigger happens during the cooldown, it has no effect.
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

-- | Start a cooldown period and perform the action when the period ends.
-- If another trigger happens during the cooldown, it restarts the cooldown again.
--
-- /N.B. If a trigger happens DURING the 'debounceAction' it starts a new cooldown./
-- /So if the 'debounceAction' takes longer than the 'debounceFreq', it might run/
-- /again before the previous action has ended./
--
-- Example of how this style debounce works:
--
-- @
-- ! = function execution
-- . = cooldown period
-- X = debounced code execution
--
-- !           !  !    !
--  .......     ...............
--         X                   X
-- @
--
-- @since 0.1.6
trailingDelayEdge :: DebounceEdge
trailingDelayEdge = TrailingDelay

mkDebounceInternal
    :: MVar () -> (Int -> IO ()) -> DebounceSettings -> IO (IO ())
mkDebounceInternal baton delayFn (DebounceSettings freq action edge name) =
    case edge of
        Leading -> leadingDebounce <$> newEmptyMVar
        LeadingMute -> pure leadingMuteDebounce
        Trailing -> pure trailingDebounce
        TrailingDelay -> trailingDelayDebounce <$> newTVarIO minBound
  where
    -- LEADING
    --
    --   1) try take baton to start
    --   2) succes -> empty trigger & start worker, failed -> fill trigger
    --   3) worker do action
    --   4) delay
    --   5) try take trigger
    --   6) success -> repeat action, failed -> put baton back
    leadingDebounce trigger = do
        -- 1)
        success <- tryTakeMVar baton
        case success of
            -- 2)
            Nothing -> void $ tryPutMVar trigger ()
            Just () -> do
                void $ tryTakeMVar trigger
                forkAndLabel loop
      where
        loop = do
            -- 3)
            ignoreExc action
            -- 4)
            delayFn freq
            -- 5)
            isTriggered <- tryTakeMVar trigger
            case isTriggered of
                -- 6)
                Nothing -> putMVar baton ()
                Just () -> loop
    -- LEADING MUTE
    --
    --   1) try take baton to start
    --   2) success -> start worker, failed -> die
    --   3) worker delay
    --   4) do action
    --   5) put baton back
    leadingMuteDebounce = do
        -- 1)
        success <- tryTakeMVar baton
        case success of
            -- 2)
            Nothing -> pure ()
            Just () ->
                forkAndLabel $ do
                    -- 3)
                    ignoreExc action
                    -- 4)
                    delayFn freq
                    -- 5)
                    putMVar baton ()
    -- TRAILING
    --
    --   1) try take baton to start
    --   2) success -> start worker, failed -> die
    --   3) worker delay
    --   4) do action
    --   5) put baton back
    trailingDebounce = do
        -- 1)
        success <- tryTakeMVar baton
        case success of
            -- 2)
            Nothing -> pure ()
            Just () ->
                forkAndLabel $ do
                    -- 3)
                    delayFn freq
                    -- 4)
                    ignoreExc action
                    -- 5)
                    putMVar baton ()
        -- TRAILING DELAY
        --
        --   1) get current time -> /now/
        --   2) try take baton to start
        --   3) success -> set time var to /now/ & start worker, failed -> update time var to /now/
        --   4) worker waits minimum delay
        --   5) check diff of time var with /now/
        --   6) less -> wait the difference, same/more -> do action
        --   7) after action, recheck if there was any trigger
        --   8) put baton back
    trailingDelayDebounce timeTVar = do
        -- 1)
        now <- getMonotonicTimeNSec
        -- 2)
        success <- tryTakeMVar baton
        case success of
            -- 3)
            Nothing -> atomically $ do
                oldTime <- readTVar timeTVar
                when (oldTime < now) $ writeTVar timeTVar now
            Just () -> do
                atomically $ writeTVar timeTVar now
                forkAndLabel $ loop freq
      where
        loop delay = do
            -- 4)
            delayFn delay
            lastTrigger <- readTVarIO timeTVar
            now <- getMonotonicTimeNSec
            -- 5)
            let diff = fromIntegral (now - lastTrigger) `div` 1000
                shouldWait = diff < freq
            if shouldWait
                -- 6)
                then loop $ freq - diff
                else do
                    ignoreExc action
                    timeAfterAction <- readTVarIO timeTVar
                    -- 7)
                    let wasTriggered = timeAfterAction > now
                    if wasTriggered
                        then do
                            updatedNow <- getMonotonicTimeNSec
                            let newDiff = fromIntegral (updatedNow - timeAfterAction) `div` 1000
                            loop $ freq - newDiff
                        -- 8)
                        else putMVar baton ()
    forkAndLabel act = do
        tid <- mask_ $ forkIO act
        labelThread tid name

ignoreExc :: IO () -> IO ()
ignoreExc = handle $ \(_ :: SomeException) -> return ()
