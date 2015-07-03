-- |
--
-- Since 3.0.3
module Network.Wai.Middleware.AddHeaders
    ( addHeaders
    , mapResponseHeader
    ) where

import Network.HTTP.Types   (ResponseHeaders, Header)
import Network.Wai          (Middleware)
import Network.Wai.Internal (Response(..))
import Data.ByteString      (ByteString)

import qualified Data.CaseInsensitive as CI
import Control.Arrow (first)

addHeaders :: [(ByteString, ByteString)] -> Middleware
-- ^ Prepend a list of headers without any checks
--
-- Since 3.0.3

addHeaders h app req respond = app req $ respond . addHeaders' (map (first CI.mk) h)

addHeaders' :: [Header] -> Response -> Response
addHeaders' h = mapResponseHeader (\hs -> h ++ hs)


-- | Apply the provided function to the response header list of the Response.
mapResponseHeader :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapResponseHeader f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapResponseHeader f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
mapResponseHeader f (ResponseStream s h b) = ResponseStream s (f h) b
mapResponseHeader _ r@(ResponseRaw _ _) = r

