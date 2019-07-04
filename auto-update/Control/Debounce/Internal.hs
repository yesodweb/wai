{-# LANGUAGE ScopedTypeVariables #-}

-- | Unstable API which exposes internals for testing.
module Control.Debounce.Internal (
  DebounceSettings(..)
  , DebounceEdge(..)
  , mkDebounceInternal
  ) where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (takeMVar, tryPutMVar, tryTakeMVar, MVar)
import           Control.Exception       (SomeException, handle, mask_)
import           Control.Monad           (forever, void, when)
import           Data.Maybe              (isJust)

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
    { debounceFreq   :: Int
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
    -- Default: Leading.
    --
    -- @since 0.1.6
    }

-- | Setting to control whether the action happens at the leading and/or trailing
-- edge of the timeout.
--
-- @since 0.1.6
data DebounceEdge =
  Leading
  -- ^ Perform the action immediately, and then begin a cooldown period.
  -- If trigger happens again during the cooldown, wait until the end of the cooldown and then
  -- perform the action again /plus/ enter a new cooldown period.
  | LeadingAndTrailing
  -- ^ Perform the action immediately, and then begin a cooldown period.
  -- If trigger happens again during the cooldown, wait until the end of the cooldown and then
  -- perform the action again but /do not/ enter a new cooldown period.
  | Trailing
  -- ^ Start a cooldown period and perform the action when the period ends. If another trigger
  -- happens during the cooldown, it has no effect.
  deriving (Show, Eq)

mkDebounceInternal :: MVar () -> (Int -> IO ()) -> DebounceSettings -> IO (IO ())
mkDebounceInternal baton delayFn (DebounceSettings freq action edge) = do
    mask_ $ void $ forkIO $ forever $ do
        takeMVar baton
        case edge of
          Leading -> ignoreExc action >> runDelay
          LeadingAndTrailing -> ignoreExc action >> runDelay
          Trailing -> runDelay

    return $ void $ tryPutMVar baton ()

  where runDelay = do
            delayFn freq

            case edge of
                Trailing -> do
                    -- Empty the baton of any other activations during the interval
                    void $ tryTakeMVar baton
                    ignoreExc action
                Leading ->
                    -- We already fired at the beginning of the interval so do nothing
                    return ()
                LeadingAndTrailing -> do
                    firedDuringInterval <- isJust <$> tryTakeMVar baton
                    when firedDuringInterval $ ignoreExc action

ignoreExc :: IO () -> IO ()
ignoreExc = handle $ \(_ :: SomeException) -> return ()
