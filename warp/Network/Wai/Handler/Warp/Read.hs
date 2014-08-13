{-# LANGUAGE CPP #-}
module Network.Wai.Handler.Warp.Read (readMaybe) where

import Text.Read

#if !MIN_VERSION_base(4,6,0)
import qualified Text.ParserCombinators.ReadP as P

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
-- A 'Left' value indicates a parse error.
--
-- /Since: 4.6.0.0/
readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift P.skipSpaces
       return x

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
--
-- /Since: 4.6.0.0/
readMaybe :: Read a => String -> Maybe a
readMaybe s = case readEither s of
                Left _  -> Nothing
                Right a -> Just a
#endif
