{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

-- |
-- Module      : Network.Wai.Search
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com, rtomharper@googlemail.com,
--               duncan@haskell.org
-- Stability   : experimental
-- Portability : GHC
--
-- Port of Data.Text.Search to ByteStrings, applied to the Wai framework
--
-- References:
--
-- * R. S. Boyer, J. S. Moore: A Fast String Searching Algorithm.
--   Communications of the ACM, 20, 10, 762-772 (1977)
--
-- * R. N. Horspool: Practical Fast Searching in Strings.  Software -
--   Practice and Experience 10, 501-506 (1980)
--
-- * D. M. Sunday: A Very Fast Substring Search Algorithm.
--   Communications of the ACM, 33, 8, 132-142 (1990)
--
-- * F. Lundh: The Fast Search Algorithm.
--   <http://effbot.org/zone/stringlib.htm> (2006)

module Network.Wai.Search
    (
      index
    , checkNeg
    , checkFound
    ) where

import qualified Data.ByteString.Unsafe as SU
import qualified Data.ByteString as S
import Data.ByteString ( ByteString )
import Data.Word (Word64)
import Data.Bits ((.|.), (.&.), shiftL)

data PairS a b = !a :*: !b

-- | /O(n+m)/ Find the offset of @needle@ within @haystack@.  The offset
-- returned represents the location within the haystack ByteString
--
-- In (unlikely) bad cases, this algorithm's complexity degrades
-- towards /O(n*m)/.
index :: ByteString                -- ^ Substring to search for (@needle@)
      -> ByteString                -- ^ Text to search in (@haystack@)
      -> Maybe Int
index needle haystack
    | nlen == 1              = scanOne (nindex 0)
    | nlen <= 0 || ldiff < 0 = Nothing
    | otherwise              = scan 0
  where
    nlen     = S.length needle
    hlen     = S.length haystack
    ldiff    = hlen - nlen
    nlast    = nlen - 1
    z        = nindex nlast
    nindex k = SU.unsafeIndex needle k
    hindex k = SU.unsafeIndex haystack k
    hindex' k | k == hlen  = 0
              | otherwise = SU.unsafeIndex haystack k
    (mask :: Word64) :*: skip  = buildTable 0 0 (nlen-2)
    buildTable !i !msk !skp
        | i >= nlast           = (msk .|. swizzle z) :*: skp
        | otherwise            = buildTable (i+1) (msk .|. swizzle c) skp'
        where c                = nindex i
              skp' | c == z    = nlen - i - 2
                   | otherwise = skp
    swizzle k = 1 `shiftL` (fromIntegral k .&. 0x3f)
    scan !i
        | i > ldiff                  = Nothing
        | c == z && candidateMatch 0 = Just i
        | otherwise                  = scan (i + delta)
        where c = hindex (i + nlast)
              candidateMatch !j
                    | j >= nlast               = True
                    | hindex (i+j) /= nindex j = False
                    | otherwise                = candidateMatch (j+1)
              delta | nextInPattern = nlen + 1
                    | c == z        = skip + 1
                    | otherwise     = 1
              nextInPattern         = mask .&. swizzle (hindex' (i+nlen)) == 0
    scanOne c = loop 0
        where loop !i | i >= hlen     = Nothing
                      | hindex i == c = Just i
                      | otherwise     = loop (i+1)
{-# INLINE index #-}

-- It's not likely that one arbitrary string will ever be a substring of some
-- other string, so this is pretty much a negative test
{-
checkNeg :: ByteString -> ByteString -> Bool
checkNeg n h
  | S.null n  = True
  | otherwise = (S.findSubstring n h) == (index n h)
  -}

-- Make sure that index works the same as ByteString.findSubstring when we
-- force matches
{-
checkFound :: ByteString -> Int -> Int -> Bool
checkFound bs offset len
  | S.null n = True -- cop out
  | otherwise = case index n bs of
      Nothing -> False
      _       -> checkNeg n bs
  where
  n = S.take len $ S.drop offset bs
  -}


