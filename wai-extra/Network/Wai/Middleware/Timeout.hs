-- | Timeout requests
--
-- @since X.X.X@
--
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
timeout :: Int -> Middleware
timeout = timeoutStatus status503

-- | Time out with the given @'Status'@
timeoutStatus :: Status -> Int -> Middleware
timeoutStatus status = timeoutAs $ responseLBS status [] ""

-- | Time out with the given @'Response'@
timeoutAs :: Response -> Int -> Middleware
timeoutAs timeoutReponse seconds app req respond =
    maybe (respond timeoutReponse) pure
        =<< Timeout.timeout (seconds * 1000000) (app req respond)
