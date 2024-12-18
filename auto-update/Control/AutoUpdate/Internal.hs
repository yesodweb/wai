{-# LANGUAGE RecordWildCards #-}

module Control.AutoUpdate.Internal (
    -- * Debugging
    UpdateState (..),
    mkClosableAutoUpdate,
    mkClosableAutoUpdate',
)
where

import Control.AutoUpdate.Event
