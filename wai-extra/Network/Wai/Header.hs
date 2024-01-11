-- | Some helpers for dealing with WAI 'Header's.
module Network.Wai.Header (
    contentLength,
    parseQValueList,
    replaceHeader,
) where

import Control.Monad (guard)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Internal (w2c)
import Data.Word8 (_0, _1, _period, _semicolon, _space)
import Network.HTTP.Types as H
import Text.Read (readMaybe)

import Network.Wai.Util (dropWhileEnd, splitCommas)

-- | More useful for a response. A Wai Request already has a requestBodyLength
contentLength :: [(HeaderName, S8.ByteString)] -> Maybe Integer
contentLength hdrs = lookup H.hContentLength hdrs >>= readInt

readInt :: S8.ByteString -> Maybe Integer
readInt bs =
    case S8.readInteger bs of
        -- 'S.all' is also 'True' for an empty string
        Just (i, rest) | S.all (== _space) rest -> Just i
        _ -> Nothing

replaceHeader :: H.HeaderName -> S.ByteString -> [H.Header] -> [H.Header]
replaceHeader name val old =
    (name, val) : filter ((/= name) . fst) old

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
    go = checkQ . S.break (== _semicolon)
    checkQ :: (S.ByteString, S.ByteString) -> (S.ByteString, Maybe Int)
    checkQ (val, "") = (val, Just 1000)
    checkQ (val, bs) =
        -- RFC 7231 says optional whitespace can be around the semicolon.
        -- So drop any before it       ,           . and any behind it       $ and drop the semicolon
        ( dropWhileEnd (== _space) val
        , parseQval . S.dropWhile (== _space) $ S.drop 1 bs
        )
      where
        parseQval qVal = do
            q <- S.stripPrefix "q=" qVal
            (i, rest) <- S.uncons q
            guard $
                i `elem` [_0, _1]
                    && S.length rest <= 4
            case S.uncons rest of
                Nothing
                    -- q = "0" or "1"
                    | i == _0 -> Just 0
                    | i == _1 -> Just 1000
                    | otherwise -> Nothing
                Just (dot, trail)
                    | dot == _period && not (i == _1 && S.any (/= _0) trail) -> do
                        let len = S.length trail
                            extraZeroes = replicate (3 - len) '0'
                        guard $ len > 0
                        readMaybe $ w2c i : S8.unpack trail ++ extraZeroes
                    | otherwise -> Nothing
