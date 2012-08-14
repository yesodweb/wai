{-# LANGUAGE OverloadedStrings, MagicHash, BangPatterns  #-}

-- Copyright     : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License       : BSD3

module Network.Wai.Handler.Warp.ReadInt (readInt) where

-- This function lives in its own file because the MagicHash pragma interacts
-- poorly with the CPP pragma.

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as C
import Data.Int (Int64)
import GHC.Prim
import GHC.Types

{-# INLINE readInt #-}
readInt :: Integral a => ByteString -> a
readInt bs = fromIntegral $ readInt64 bs

-- This function is used to parse the Content-Length field of HTTP headers and
-- is a performance hot spot. It should only be replaced with something
-- significantly and provably faster.
--
-- It needs to be able work correctly on 32 bit CPUs for file sizes > 2G so we
-- use Int64 here and then make a generic 'readInt' that allows conversion to
-- Int and Integer.

{- NOINLINE readInt64MH #-}
readInt64 :: ByteString -> Int64
readInt64 bs =
        B.foldl' (\i c -> i * 10 + fromIntegral (mhDigitToInt c)) 0
             $ B.takeWhile C.isDigit bs

data Table = Table !Addr#

{- NOINLINE mhDigitToInt #-}
mhDigitToInt :: Char -> Int
mhDigitToInt (C# i) = I# (word2Int# (indexWord8OffAddr# addr (ord# i)))
  where
    !(Table addr) = table
    table :: Table
    table = Table
        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

