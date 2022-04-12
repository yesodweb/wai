-- | Infer the remote IP address using headers
module Network.Wai.Middleware.RealIp
    ( realIp
    , realIpHeader
    , realIpTrusted
    , defaultTrusted
    , ipInRange
    ) where

import qualified Data.ByteString.Char8 as B8 (split, unpack)
import qualified Data.IP as IP
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Network.HTTP.Types (HeaderName, RequestHeaders)
import Network.Wai (Middleware, remoteHost, requestHeaders)
import Text.Read (readMaybe)

-- | Infer the remote IP address from the @X-Forwarded-For@ header,
-- trusting requests from any private IP address. See 'realIpHeader' and
-- 'realIpTrusted' for more information and options.
--
-- @since 3.1.5
realIp :: Middleware
realIp = realIpHeader "X-Forwarded-For"

-- | Infer the remote IP address using the given header, trusting
-- requests from any private IP address. See 'realIpTrusted' for more
-- information and options.
--
-- @since 3.1.5
realIpHeader :: HeaderName -> Middleware
realIpHeader header =
    realIpTrusted header $ \ip -> any (ipInRange ip) defaultTrusted

-- | Infer the remote IP address using the given header, but only if the
-- request came from an IP that is trusted by the provided predicate.
--
-- The last non-trusted address is used to replace the 'remoteHost' in
-- the 'Request', unless all present IP addresses are trusted, in which
-- case the first address is used. Invalid IP addresses are ignored, and
-- the remoteHost value remains unaltered if no valid IP addresses are
-- found.
--
-- Examples:
--
-- @ realIpTrusted "X-Forwarded-For" $ flip ipInRange "10.0.0.0/8" @
--
-- @ realIpTrusted "X-Real-Ip" $ \\ip -> any (ipInRange ip) defaultTrusted @
--
-- @since 3.1.5
realIpTrusted :: HeaderName -> (IP.IP -> Bool) -> Middleware
realIpTrusted header isTrusted app req respond = app req' respond
  where
    req' = fromMaybe req $ do
             (ip, port) <- IP.fromSockAddr (remoteHost req)
             ip' <- if isTrusted ip
                      then findRealIp (requestHeaders req) header isTrusted
                      else Nothing
             Just $ req { remoteHost = IP.toSockAddr (ip', port) }

-- | Standard private IP ranges.
--
-- @since 3.1.5
defaultTrusted :: [IP.IPRange]
defaultTrusted = [ "127.0.0.0/8"
                 , "10.0.0.0/8"
                 , "172.16.0.0/12"
                 , "192.168.0.0/16"
                 , "::1/128"
                 , "fc00::/7"
                 ]

-- | Check if the given IP address is in the given range.
--
-- IPv4 addresses can be checked against IPv6 ranges, but testing an
-- IPv6 address against an IPv4 range is always 'False'.
--
-- @since 3.1.5
ipInRange :: IP.IP -> IP.IPRange -> Bool
ipInRange (IP.IPv4 ip) (IP.IPv4Range r) = ip `IP.isMatchedTo` r
ipInRange (IP.IPv6 ip) (IP.IPv6Range r) = ip `IP.isMatchedTo` r
ipInRange (IP.IPv4 ip) (IP.IPv6Range r) = IP.ipv4ToIPv6 ip `IP.isMatchedTo` r
ipInRange _ _ = False


findRealIp :: RequestHeaders -> HeaderName -> (IP.IP -> Bool) -> Maybe IP.IP
findRealIp reqHeaders header isTrusted =
    case (nonTrusted, ips) of
      ([], xs) -> listToMaybe xs
      (xs, _)  -> listToMaybe $ reverse xs
  where
    -- account for repeated headers
    headerVals = [ v | (k, v) <- reqHeaders, k == header ]
    ips = mapMaybe (readMaybe . B8.unpack) $ concatMap (B8.split ',') headerVals
    nonTrusted = filter (not . isTrusted) ips
