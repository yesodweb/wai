-- | Debounce an action, ensuring it doesn't occur more than once for a given
-- period of time.
--
-- This is useful as an optimization, for example to ensure that logs are only
-- flushed to disk at most once per second.
--
-- Example usage:
--
-- @
-- > printString <- 'mkDebounce' 'defaultDebounceSettings'
--                  { 'debounceAction' = putStrLn "Running action"
--                  , 'debounceFreq' = 5000000 -- 5 seconds
--                  , 'debounceEdge' = 'DI.trailingEdge' -- Trigger on the trailing edge
--                  }
-- > printString
-- Running action
-- > printString
-- \<Wait five seconds>
-- Running action
-- @
--
-- See the fast-logger package ("System.Log.FastLogger") for real-world usage.
--
-- @since 0.1.2
module Control.Debounce (
    -- * Creation
    mkDebounce,

    -- * Settings
    DI.DebounceSettings,
    defaultDebounceSettings,

    -- ** Accessors
    DI.debounceFreq,
    DI.debounceAction,
    DI.debounceEdge,
    DI.debounceThreadName,

    -- ** Edge types
    DI.leadingEdge,
    DI.leadingMuteEdge,
    DI.trailingEdge,
    DI.trailingDelayEdge,
) where

import Control.Concurrent (newMVar, threadDelay)
import qualified Control.Debounce.Internal as DI

-- | Default value for creating a 'DebounceSettings'.
--
-- @since 0.1.2
defaultDebounceSettings :: DI.DebounceSettings
defaultDebounceSettings =
    DI.DebounceSettings
        { DI.debounceFreq = 1000000
        , DI.debounceAction = return ()
        , DI.debounceEdge = DI.leadingEdge
        , DI.debounceThreadName = "Debounce"
        }

-- | Generate an action which will trigger the debounced action to be performed.
--
-- /N.B. The generated action will always immediately return, regardless of the 'debounceFreq',/
-- /as the debounced action (and the delay\/cooldown) is always performed in a separate thread./
--
-- @since 0.1.2
mkDebounce :: DI.DebounceSettings -> IO (IO ())
mkDebounce settings = do
    baton <- newMVar ()
    DI.mkDebounceInternal baton threadDelay settings
