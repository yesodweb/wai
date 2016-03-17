{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.Warp.PackInt where

import Control.Monad (when)
import Data.ByteString.Internal (ByteString(..), unsafeCreate)
import Data.Word8 (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)

-- |
--
-- prop> packIntegral (abs n) == B.pack (show (abs n))
-- prop> \(Large n) -> let n' = fromIntegral (abs n :: Int) in packIntegral n' == B.pack (show n')

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
