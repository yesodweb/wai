module Network.Wai.Handler.Warp.MultiMap (
    MMap
  , Some(..)
  , empty
  , singleton
  , insert
  , search
  , searchWith
  , isEmpty
  , valid
  , pruneWith
  , fromList
  , toList
  , fromSortedList
  , toSortedList
  , merge
  ) where

import Control.Applicative ((<$>))
import Data.List (foldl')

----------------------------------------------------------------

-- | One ore more list to implement multimap.
data Some a = One !a
            | Tom !a !(Some a) -- Two or more
            deriving (Eq,Show)

-- This is slow but assuming rarely used.
snoc :: Some a -> a -> Some a
snoc (One x) y    = Tom x (One y)
snoc (Tom x xs) y = Tom x (snoc xs y)

top :: Some a -> a
top (One x)   = x
top (Tom x _) = x

----------------------------------------------------------------

-- | Red black tree as multimap.
data MMap k v = Leaf -- color is Black
              | Node Color !(MMap k v) !k !(Some v) !(MMap k v)
              deriving (Show)

data Color = B -- ^ Black
           | R -- ^ Red
           deriving (Eq, Show)

----------------------------------------------------------------

instance (Eq k, Eq v) => Eq (MMap k v) where
    t1 == t2 = toSortedList t1 == toSortedList t2

----------------------------------------------------------------

-- | O(log N)
search :: Ord k => k -> MMap k v -> Maybe v
search _ Leaf = Nothing
search xk (Node _ l k v r) = case compare xk k of
    LT -> search xk l
    GT -> search xk r
    EQ -> Just $ top v

-- | O(log N)
searchWith :: Ord k => k -> (Some v -> Maybe v) -> MMap k v -> Maybe v
searchWith _ _ Leaf = Nothing
searchWith xk f (Node _ l k v r) = case compare xk k of
    LT -> searchWith xk f l
    GT -> searchWith xk f r
    EQ -> f v

----------------------------------------------------------------

-- | O(1)
isEmpty :: MMap k v -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | O(1)
empty :: MMap k v
empty = Leaf

----------------------------------------------------------------

-- | O(1)
singleton :: Ord k => k -> v -> MMap k v
singleton k v = Node B Leaf k (One v) Leaf

----------------------------------------------------------------

-- | O(log N)
insert :: Ord k => k -> v -> MMap k v -> MMap k v
insert kx kv t = turnB (insert' kx kv t)

insert' :: Ord k => k -> v -> MMap k v -> MMap k v
insert' xk xv Leaf = Node R Leaf xk (One xv) Leaf
insert' xk xv (Node B l k v r) = case compare xk k of
    LT -> balanceL' (insert' xk xv l) k v r
    GT -> balanceR' l k v (insert' xk xv r)
    EQ -> Node B l k (snoc v xv) r
insert' xk xv (Node R l k v r) = case compare xk k of
    LT -> Node R (insert' xk xv l) k v r
    GT -> Node R l k v (insert' xk xv r)
    EQ -> Node R l k (snoc v xv) r

balanceL' :: MMap k v -> k -> Some v -> MMap k v -> MMap k v
balanceL' (Node R (Node R a xk xv b) yk yv c) zk zv d =
    Node R (Node B a xk xv b) yk yv (Node B c zk zv d)
balanceL' (Node R a xk xv (Node R b yk yv c)) zk zv d =
    Node R (Node B a xk xv b) yk yv (Node B c zk zv d)
balanceL' l k v r = Node B l k v r

balanceR' :: MMap k v -> k -> Some v -> MMap k v -> MMap k v
balanceR' a xk xv (Node R b yk yv (Node R c zk zv d)) =
    Node R (Node B a xk xv b) yk yv (Node B c zk zv d)
balanceR' a xk xv (Node R (Node R b yk yv c) zk zv d) =
    Node R (Node B a xk xv b) yk yv (Node B c zk zv d)
balanceR' l xk xv r = Node B l xk xv r

turnB :: MMap k v -> MMap k v
turnB Leaf             = error "turnB"
turnB (Node _ l k v r) = Node B l k v r

----------------------------------------------------------------

-- | O(N log N)
fromList :: Ord k => [(k,v)] -> MMap k v
fromList = foldl' (\t (k,v) -> insert k v t) empty

-- | O(N)
toList :: MMap k v -> [(k,v)]
toList t = inorder t []
  where
    inorder Leaf xs = xs
    inorder (Node _ l k v r) xs = inorder l (pairs k v ++ inorder r xs)
    pairs k (One v) = [(k,v)]
    pairs k (Tom v vs) = (k,v) : pairs k vs

----------------------------------------------------------------

-- | O(N)
-- "Constructing Red-Black Trees" by Ralf Hinze
fromSortedList :: Ord k => [(k,Some v)] -> MMap k v
fromSortedList = linkAll . foldr add []

data Digit k v = Uno k (Some v) (MMap k v)
               | Due k (Some v) (MMap k v) k (Some v) (MMap k v)
               deriving (Eq,Show)

incr :: Digit k v -> [Digit k v] -> [Digit k v]
incr (Uno k v t) [] = [Uno k v t]
incr (Uno k1 v1 t1) (Uno k2 v2 t2 : ps) = Due k1 v1 t1 k2 v2 t2 : ps
incr (Uno k1 v1 t1) (Due k2 v2 t2 k3 v3 t3 : ps) = Uno k1 v1 t1 : incr (Uno k2 v2 (Node B t2 k3 v3 t3)) ps
incr _ _ = error "incr"

add :: (k,Some v) -> [Digit k v] -> [Digit k v]
add (k,v) ps = incr (Uno k v Leaf) ps

linkAll :: [Digit k v] -> MMap k v
linkAll = foldl' link Leaf

link :: MMap k v -> Digit k v -> MMap k v
link l (Uno k v t) = Node B l k v t
--link l (Due k1 v1 t1 k2 v2 t2) = Node B (Node R l k1 v1 t1) k2 v2 t2
link l (Due k1 v1 t1 k2 v2 t2) = Node B l k1 v1 (Node R t1 k2 v2 t2)

----------------------------------------------------------------

-- | O(N)
toSortedList :: MMap k v -> [(k,Some v)]
toSortedList t = inorder t []
  where
    inorder Leaf xs = xs
    inorder (Node _ l k v r) xs = inorder l ((k,v) : inorder r xs)

----------------------------------------------------------------

-- | O(N)
pruneWith :: Ord k =>
             MMap k v
          -> (k -> Some v -> IO [(k, Some v)])
          -> IO (MMap k v)
pruneWith t run = fromSortedList <$> inorder t []
  where
    inorder Leaf xs = return xs
    inorder (Node _ l k v r) xs = do
        ys <- run k v
        zs <- inorder r xs
        inorder l (ys ++ zs)

----------------------------------------------------------------

-- O(N log N) where N is the size of the second argument
merge :: Ord k => MMap k v -> MMap k v -> MMap k v
merge base m = foldl' ins base xs
  where
    ins t (k,v) = insert k v t
    xs = toList m

----------------------------------------------------------------
-- for testing

valid :: Ord k => MMap k v -> Bool
valid t = isBalanced t && isOrdered t

isBalanced :: MMap k v -> Bool
isBalanced t = isBlackSame t && isRedSeparate t

isBlackSame :: MMap k v -> Bool
isBlackSame t = all (n==) ns
  where
    n:ns = blacks t

blacks :: MMap k v -> [Int]
blacks = blacks' 0
  where
    blacks' n Leaf = [n+1]
    blacks' n (Node R l _ _ r) = blacks' n  l ++ blacks' n  r
    blacks' n (Node B l _ _ r) = blacks' n' l ++ blacks' n' r
      where
        n' = n + 1

isRedSeparate :: MMap k v -> Bool
isRedSeparate = reds B

reds :: Color -> MMap k v -> Bool
reds _ Leaf = True
reds R (Node R _ _ _ _) = False
reds _ (Node c l _ _ r) = reds c l && reds c r

isOrdered :: Ord k => MMap k v -> Bool
isOrdered t = ordered $ toSortedList t
  where
    ordered [] = True
    ordered [_] = True
    ordered (x:y:xys) = fst x <= fst y && ordered (y:xys)
