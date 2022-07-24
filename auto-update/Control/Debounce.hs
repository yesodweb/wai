-- | Debounce an action, ensuring it doesn't occur more than once for a given
-- period of time.
--
-- This is useful as an optimization, for example to ensure that logs are only
-- flushed to disk at most once per second.
--
-- Example usage:
--
-- @
-- printString <- 'mkDebounce' 'defaultDebounceSettings'
--                  { 'debounceAction' = putStrLn "Running action"
--                  , 'debounceFreq' = 5000000 -- 5 seconds
--                  , 'debounceEdge' = 'DI.trailingEdge' -- Trigger on the trailing edge
--                  }
-- @
--
-- >>> printString
-- Running action
-- >>> printString
-- <Wait five seconds>
-- Running action
--
-- See the fast-logger package ("System.Log.FastLogger") for real-world usage.
--
-- @since 0.1.2
module Control.Debounce
    ( -- * Type
      DI.DebounceSettings
    , defaultDebounceSettings
      -- * Accessors
    , DI.debounceFreq
    , DI.debounceAction
    , DI.debounceEdge
    , DI.leadingEdge
    , DI.trailingEdge
      -- * Creation
    , mkDebounce
    ) where

import           Control.Concurrent      (newEmptyMVar, threadDelay)
import qualified Control.Debounce.Internal as DI

-- | Default value for creating a 'DebounceSettings'.
--
-- @since 0.1.2
defaultDebounceSettings :: DI.DebounceSettings
defaultDebounceSettings = DI.DebounceSettings
    { DI.debounceFreq = 1000000
    , DI.debounceAction = return ()
    , DI.debounceEdge = DI.leadingEdge
    }

-- | Generate an action which will trigger the debounced action to be performed.
--
-- @since 0.1.2
mkDebounce :: DI.DebounceSettings -> IO (IO ())
mkDebounce settings = do
  baton <- newEmptyMVar
  DI.mkDebounceInternal baton threadDelay settings
