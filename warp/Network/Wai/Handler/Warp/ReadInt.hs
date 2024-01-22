{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Copyright     : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License       : BSD3

module Network.Wai.Handler.Warp.ReadInt (
    readInt,
    readInt64,
) where

import qualified Data.ByteString as S
import Data.Word8 (isDigit, _0)

import Network.Wai.Handler.Warp.Imports hiding (readInt)

{-# INLINE readInt #-}

-- | Will 'takeWhile isDigit' and return the parsed 'Integral'.
readInt :: Integral a => ByteString -> a
readInt bs = fromIntegral $ readInt64 bs

-- This function is used to parse the Content-Length field of HTTP headers and
-- is a performance hot spot. It should only be replaced with something
-- significantly and provably faster.
--
-- It needs to be able work correctly on 32 bit CPUs for file sizes > 2G so we
-- use Int64 here and then make a generic 'readInt' that allows conversion to
-- Int and Integer.

{-# NOINLINE readInt64 #-}
readInt64 :: ByteString -> Int64
readInt64 bs =
    S.foldl' (\ !i !c -> i * 10 + fromIntegral (c - _0)) 0 $
        S.takeWhile isDigit bs
