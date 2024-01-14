{-# LANGUAGE CPP #-}

-- | Only allow local connections.
module Network.Wai.Middleware.Local (
    local,
) where

import Network.Socket (SockAddr (..))
import Network.Wai (Middleware, Response, remoteHost)

-- | This middleware rejects non-local connections with a specific response.
--   It is useful when supporting web-based local applications, which would
--   typically want to reject external connections.
local :: Response -> Middleware
local resp f r k = case remoteHost r of
    SockAddrInet _ h
        | h == fromIntegral home ->
            f r k
#if !defined(mingw32_HOST_OS) && !defined(_WIN32)
    SockAddrUnix _ -> f r k
#endif
    _ -> k resp
  where
    home :: Integer
    home = 127 + (256 * 256 * 256)
