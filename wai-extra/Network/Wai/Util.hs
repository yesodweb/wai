{-# LANGUAGE CPP #-}
-- | Some helpers functions.
module Network.Wai.Util
    ( dropWhileEnd
    , splitCommas
    , trimWS
    ) where

import qualified Data.ByteString as S
import Data.Word8 (Word8, _comma, _space)

-- | Used to split a header value which is a comma separated list
splitCommas :: S.ByteString -> [S.ByteString]
splitCommas = map trimWS . S.split _comma

-- Trim whitespace
trimWS :: S.ByteString -> S.ByteString
trimWS = dropWhileEnd (== _space) . S.dropWhile (== _space)

-- | Dropping all 'Word8's from the end that satisfy the predicate.
dropWhileEnd :: (Word8 -> Bool) -> S.ByteString -> S.ByteString
#if MIN_VERSION_bytestring(0,10,12)
dropWhileEnd = S.dropWhileEnd
#else
dropWhileEnd p = fst . S.spanEnd p
#endif
