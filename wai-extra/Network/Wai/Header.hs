-- | Some helpers for dealing with WAI 'Header's.

module Network.Wai.Header
    ( contentLength
    ) where

import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Types as H

-- | More useful for a response. A Wai Request already has a requestBodyLength
contentLength :: [(HeaderName, S8.ByteString)] -> Maybe Integer
contentLength hdrs = lookup H.hContentLength hdrs >>= readInt

readInt :: S8.ByteString -> Maybe Integer
readInt bs =
    case S8.readInteger bs of
        Just (i, rest) | S8.null rest -> Just i
        _ -> Nothing
