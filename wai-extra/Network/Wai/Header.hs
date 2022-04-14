-- | Some helpers for dealing with WAI 'Header's.

module Network.Wai.Header
    ( contentLength
    , parseQValueList
    , replaceHeader
    , splitCommas
    ) where

import Control.Monad (guard)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as S
import Data.Word8 (_comma, _period, _semicolon, _space)
import Network.HTTP.Types as H
import Text.Read (readMaybe)

-- | More useful for a response. A Wai Request already has a requestBodyLength
contentLength :: [(HeaderName, S8.ByteString)] -> Maybe Integer
contentLength hdrs = lookup H.hContentLength hdrs >>= readInt

readInt :: S8.ByteString -> Maybe Integer
readInt bs =
    case S8.readInteger bs of
        Just (i, rest) | S8.null rest -> Just i
        _ -> Nothing

replaceHeader :: H.HeaderName -> S.ByteString -> [H.Header] -> [H.Header]
replaceHeader name val old =
    (name, val) : filter ((/= name) . fst) old

-- | Used to split a header value which is a comma separated list
splitCommas :: S.ByteString -> [S.ByteString]
splitCommas = map (S.dropWhile (== _space)) . S.split _comma

-- | Only to be used on header's values which support quality value syntax
--
-- A few things to keep in mind when using this function:
-- * The resulting 'Int' will be anywhere from 1000 to 0 ("1" = 1000, "0.6" = 600, "0.025" = 25)
-- * The absence of a Q value will result in 'Just 1000'
-- * A bad parse of the Q value will result in a 'Nothing', e.g.
--   * Q value has more than 3 digits behind the dot
--   * Q value is missing
--   * Q value is higher than 1
--   * Q value is not a number
parseQValueList :: S8.ByteString -> [(S8.ByteString, Maybe Int)]
parseQValueList = fmap go . splitCommas
  where
    go =
        fmap checkQ
        . S.break (== _semicolon)
        . S.dropWhile (== _space)
    checkQ :: S.ByteString -> Maybe Int
    checkQ "" = Just 1000
    checkQ bs = do
        q <- S8.stripPrefix ";q=" bs
        (i, rest) <- S8.uncons q
        guard $
            i `elem` ['0', '1']
                && S.length rest <= 4
        case S.uncons rest of
            -- q = "0" or "1"
            Nothing -> (1000 *) <$> readMaybe (S8.unpack q)
            Just (dot, trail)
                | dot == _period && not (i == '1' && S8.any (/= '0') trail) -> do
                    let len = S.length trail
                        extraZeroes = S8.unpack $ S8.replicate (3 - len) '0'
                    guard $ len > 0
                    readMaybe $ (i : S8.unpack trail) ++ extraZeroes
                | otherwise -> Nothing
