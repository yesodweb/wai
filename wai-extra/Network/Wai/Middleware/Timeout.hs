-- | Timeout requests
module Network.Wai.Middleware.Timeout
    ( timeout
    , timeoutStatus
    , timeoutAs
    ) where

import Network.HTTP.Types (Status, status503)
import Network.Wai
import qualified System.Timeout as Timeout

-- | Time out the request after the given number of seconds
--
-- Timeouts respond with @'status503'@. See @'timeoutStatus'@ or @'timeoutAs'@
-- to customize the behavior of the timed-out case.
--
-- @since 3.0.24.0@
timeout :: Int -> Middleware
timeout = timeoutStatus status503

-- | Time out with the given @'Status'@
--
-- @since 3.0.24.0@
timeoutStatus :: Status -> Int -> Middleware
timeoutStatus status = timeoutAs $ responseLBS status [] ""

-- | Time out with the given @'Response'@
--
-- @since 3.0.24.0@
timeoutAs :: Response -> Int -> Middleware
timeoutAs timeoutReponse seconds app req respond =
    maybe (respond timeoutReponse) pure
        =<< Timeout.timeout (seconds * 1000000) (app req respond)
