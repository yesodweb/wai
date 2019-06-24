{-# LANGUAGE ScopedTypeVariables #-}

module Control.Debounce.Internal (
  DebounceSettings(..)
  , DebounceEdge(..)
  , mkDebounce
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
    -- ^ Microseconds lag required between subsequence calls to the debounced
    -- action.
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
    -- @since 0.2.0
    }

data DebounceEdge =
  Leading
  -- ^ The action will either be performed immediately, or after the current cooldown period has expired.
  | Trailing
  -- ^ The action will be performed at the end of the current cooldown period.
  | LeadingAndTrailing
  -- ^ The action is performed on the leading edge. It's also performed on the trailing edge if
  -- the debounced function was invoked again during the cooldown.
  deriving (Show, Eq)

mkDebounce :: MVar () -> (Int -> IO ()) -> DebounceSettings -> IO (IO ())
mkDebounce baton delayFn (DebounceSettings freq action edge) = do
    mask_ $ void $ forkIO $ forever $ do
        takeMVar baton
        if edge `elem` [Leading, LeadingAndTrailing] then ignoreExc action >> runDelay False baton
        else runDelay True baton

    return $ void $ tryPutMVar baton ()

  where runDelay hasDelayedInitialActivation baton = do
            delayFn freq

            case edge of
                Trailing -> do
                    firedDuringInterval <- isJust <$> tryTakeMVar baton
                    when (firedDuringInterval || hasDelayedInitialActivation) $ ignoreExc action
                Leading ->
                    -- We already fired at the beginning of the interval so do nothing
                    return ()
                LeadingAndTrailing -> do
                    firedDuringInterval <- isJust <$> tryTakeMVar baton
                    when firedDuringInterval $ ignoreExc action

ignoreExc :: IO () -> IO ()
ignoreExc = handle $ \(_ :: SomeException) -> return ()
