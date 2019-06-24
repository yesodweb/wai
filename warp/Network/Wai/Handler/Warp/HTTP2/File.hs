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

pReadMaker :: InternalInfo -> PositionReadMaker
pReadMaker ii path = do
    (Just fd, refresh) <- getFd ii path
    return (pread fd, Refresher refresh)
  where
    pread :: Fd -> PositionRead
    pread fd off bytes buf = fromIntegral <$> positionRead fd buf bytes' off'
      where
        bytes' = fromIntegral bytes
        off' = fromIntegral off
#endif
