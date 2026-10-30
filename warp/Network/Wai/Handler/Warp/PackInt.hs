{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.Warp.PackInt where

import Data.ByteString.Internal (unsafeCreate)
import Data.Word8 (_0)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)
import qualified Network.HTTP.Types as H

import Network.Wai.Handler.Warp.Imports

packIntegral :: Integral a => a -> ByteString
packIntegral 0 = "0"
packIntegral n | n < 0 = error "packIntegral"
packIntegral n = unsafeCreate len go0
  where
    -- Counted by division: @ceiling (logBase 10 ...)@ in a 'Double' is
    -- one short from 10^15 up, and 'go' then writes its last digit
    -- before the buffer it was given.
    !len = numDigits n
    go0 p = go n $ p `plusPtr` (len - 1)
    go :: Integral a => a -> Ptr Word8 -> IO ()
    go i p = do
        let (d, r) = i `divMod` 10
        poke p (_0 + fromIntegral r)
        when (d /= 0) $ go d (p `plusPtr` (-1))
{-# SPECIALIZE packIntegral :: Int -> ByteString #-}
{-# SPECIALIZE packIntegral :: Integer -> ByteString #-}

-- | How many digits a non-negative number is written with.
numDigits :: Integral a => a -> Int
numDigits = go 1
  where
    go !acc i = case i `quot` 10 of
        0 -> acc
        q -> go (acc + 1) q
{-# SPECIALIZE numDigits :: Int -> Int #-}
{-# SPECIALIZE numDigits :: Integer -> Int #-}

-- |
--
-- >>> packStatus H.status200
-- "200"
-- >>> packStatus H.preconditionFailed412
-- "412"
packStatus :: H.Status -> ByteString
packStatus status = unsafeCreate 3 $ \p -> do
    poke p (toW8 r2)
    poke (p `plusPtr` 1) (toW8 r1)
    poke (p `plusPtr` 2) (toW8 r0)
  where
    toW8 :: Int -> Word8
    toW8 n = _0 + fromIntegral n
    !s = fromIntegral $ H.statusCode status
    (!q0, !r0) = s `divMod` 10
    (!q1, !r1) = q0 `divMod` 10
    !r2 = q1 `mod` 10
