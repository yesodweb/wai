-- |
--
-- Since 3.0.3
module Network.Wai.Middleware.AddHeaders
    ( addHeaders
    ) where

import Network.HTTP.Types   (Header)
import Network.Wai          (Middleware, modifyResponse, mapResponseHeaders)
import Network.Wai.Internal (Response(..))
import Data.ByteString      (ByteString)

import qualified Data.CaseInsensitive as CI
import Control.Arrow (first)

addHeaders :: [(ByteString, ByteString)] -> Middleware
-- ^ Prepend a list of headers without any checks
--
-- Since 3.0.3

addHeaders h = modifyResponse $ addHeaders' (map (first CI.mk) h)

addHeaders' :: [Header] -> Response -> Response
addHeaders' h = mapResponseHeaders (\hs -> h ++ hs)
