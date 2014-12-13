-- |
--
-- Since 3.0.3
module Network.Wai.Middleware.AddHeaders
    ( addHeaders
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

mapHeader :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapHeader f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapHeader f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
mapHeader f (ResponseStream s h b) = ResponseStream s (f h) b
mapHeader _ r@(ResponseRaw _ _) = r

addHeaders' :: [Header] -> Response -> Response
addHeaders' h = mapHeader (\hs -> h ++ hs)
