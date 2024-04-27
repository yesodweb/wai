module Control.Reaper.Internal (Reaper (..)) where

-- | A data structure to hold reaper APIs.
data Reaper workload item = Reaper
    { reaperAdd :: item -> IO ()
    -- ^ Adding an item to the workload
    , reaperRead :: IO workload
    -- ^ Reading workload.
    , reaperModify :: (workload -> workload) -> IO workload
    -- ^ Modify the workload. The resulting workload is returned.
    --
    --   If there is no reaper thread, the modifier will not be applied and
    --   'reaperEmpty' will be returned.
    --
    --   If the reaper is currently executing jobs, those jobs will not be in
    --   the given workload and the workload might appear empty.
    --
    --   If all jobs are removed by the modifier, the reaper thread will not be
    --   killed. The reaper thread will only terminate if 'reaperKill' is called
    --   or the result of 'reaperAction' satisfies 'reaperNull'.
    --
    --  @since 0.2.0
    , reaperStop :: IO workload
    -- ^ Stopping the reaper thread if exists.
    --   The current workload is returned.
    , reaperKill :: IO ()
    -- ^ Killing the reaper thread immediately if exists.
    }
