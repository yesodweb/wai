{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.HTTP2.File where

import Network.HTTP2.Server

import Network.Wai.Handler.Warp.Types

#ifdef WINDOWS
pReadMaker :: InternalInfo -> PositionReadMaker
pReadMaker _ = defaultPositionReadMaker
#else
import Network.Wai.Handler.Warp.FdCache
import Network.Wai.Handler.Warp.SendFile (positionRead)

-- | 'PositionReadMaker' based on file descriptor cache.
--
-- Since 3.3.13
pReadMaker :: InternalInfo -> PositionReadMaker
pReadMaker ii path = do
    (mfd, refresh) <- getFd ii path
    case mfd of
      Just fd -> return (pread fd, Refresher refresh)
      Nothing -> do
          fd <- openFile path
          return (pread fd, Closer $ closeFile fd)
  where
    pread :: Fd -> PositionRead
    pread fd off bytes buf = fromIntegral <$> positionRead fd buf bytes' off'
      where
        bytes' = fromIntegral bytes
        off' = fromIntegral off
#endif
