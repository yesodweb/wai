module MultiMapSpec where

import Network.Wai.Handler.Warp.MultiMap
import Test.Hspec
import Test.QuickCheck (property)

type Alist = [(Int,Char)]

spec :: Spec
spec = do
    describe "fromList" $ do
        it "generates a valid tree" $ property $ \xs ->
            valid $ fromList (xs :: Alist)
    describe "toSortedList" $ do
        it "generated a sorted list" $ property $ \xs ->
            ordered $ toSortedList $ fromList (xs :: Alist)
    describe "search" $ do
        it "acts as the list model" $ property $ \x xs ->
            search x (fromList xs) == lookup x (xs :: Alist)
    describe "fromSortedList" $ do
        it "generates a valid tree" $ property $ \xs ->
            valid . fromSortedList . toSortedList . fromList $ (xs :: Alist)
        it "maintains the tree with toSortedList" $ property $ \xs ->
            let t1 = fromList (xs :: Alist)
                t2 = fromSortedList $ toSortedList t1
            in t1 == t2

ordered :: Ord a => [(a, b)] -> Bool
ordered (x:y:xys) = fst x <= fst y && ordered (y:xys)
ordered _         = True
