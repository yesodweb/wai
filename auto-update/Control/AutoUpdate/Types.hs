module Control.AutoUpdate.Types where

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
