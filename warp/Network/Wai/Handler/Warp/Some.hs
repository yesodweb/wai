{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.Some (
    Some
  , singleton
  , top
  , lookupWith
  , union
  , toList
  , prune
  ) where

----------------------------------------------------------------

-- | One ore more list to implement multimap.
data Some a = One !a
            | Tom !a !(Some a) -- Two or more
            deriving (Eq,Show)

{-# INLINE singleton #-}
singleton :: a -> Some a
singleton x = One x

{-# INLINE top #-}
top :: Some a -> a
top (One x)   = x
top (Tom x _) = x

{-# INLINE lookupWith #-}
lookupWith :: (a -> Bool) -> Some a -> Maybe a
lookupWith f s = go s
  where
    go (One x)
      | f x       = Just x
      | otherwise = Nothing
    go (Tom x xs)
      | f x       = Just x
      | otherwise = go xs

{-# INLINE union #-}
union :: Some a -> Some a -> Some a
union s t = go s t
  where
    go (One x)    u = Tom x u
    go (Tom x xs) u = go xs (Tom x u)

{-# INLINE toList #-}
toList :: Some a -> [a]
toList s = go s []
  where
    go (One x)    !acc = x : acc
    go (Tom x xs) !acc = go xs (x : acc)

{-# INLINE prune #-}
prune :: (a -> IO Bool) -> Some a -> IO (Maybe (Some a))
prune act s = go s
  where
    go (One x) = do
        keep <- act x
        return $ if keep then
                     Just (One x)
                 else
                     Nothing
    go (Tom x xs) = do
        keep <- act x
        mys <- go xs
        return $ if keep then
                     case mys of
                         Nothing -> Just (One x)
                         Just ys -> Just (Tom x ys)
                 else
                     mys

