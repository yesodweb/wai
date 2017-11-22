{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Network.Wai.Handler.Warp.PackInt where

import Data.ByteString.Internal (unsafeCreate)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)
import qualified Network.HTTP.Types as H

import Network.Wai.Handler.Warp.Imports

-- $setup
-- >>> import Data.ByteString.Char8 as C8
-- >>> import Test.QuickCheck (Large(..))

-- |
--
-- prop> packIntegral (abs n) == C8.pack (show (abs n))
-- prop> \(Large n) -> let n' = fromIntegral (abs n :: Int) in packIntegral n' == C8.pack (show n')

packIntegral :: Integral a => a -> ByteString
packIntegral 0 = "0"
packIntegral n | n < 0 = error "packIntegral"
packIntegral n = unsafeCreate len go0
  where
    n' = fromIntegral n + 1 :: Double
    len = ceiling $ logBase 10 n'
    go0 p = go n $ p `plusPtr` (len - 1)
    go :: Integral a => a -> Ptr Word8 -> IO ()
    go i p = do
        let (d,r) = i `divMod` 10
        poke p (48 + fromIntegral r)
        when (d /= 0) $ go d (p `plusPtr` (-1))

{-# SPECIALIZE packIntegral :: Int -> ByteString #-}
{-# SPECIALIZE packIntegral :: Integer -> ByteString #-}

-- |
--
-- >>> packStatus H.status200
-- "200"
-- >>> packStatus H.preconditionFailed412
-- "412"

packStatus :: H.Status -> ByteString
packStatus status = unsafeCreate 3 $ \p -> do
    poke p               (toW8 r2)
    poke (p `plusPtr` 1) (toW8 r1)
    poke (p `plusPtr` 2) (toW8 r0)
  where
    toW8 :: Int -> Word8
    toW8 n = 48 + fromIntegral n
    !s = fromIntegral $ H.statusCode status
    (!q0,!r0) = s `divMod` 10
    (!q1,!r1) = q0 `divMod` 10
    !r2 = q1 `mod` 10
