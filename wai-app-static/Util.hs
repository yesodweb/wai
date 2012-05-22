{-# LANGUAGE OverloadedStrings #-}
module Util
    ( relativeDirFromPieces
    , defaultMkRedirect
    , replace
    , remove
    ) where

import WaiAppStatic.Types
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text.Encoding as TE

-- alist helper functions
replace :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
replace k v [] = [(k,v)]
replace k v (x:xs) | fst x == k = (k,v):xs
                   | otherwise  = x:replace k v xs

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove _ [] = []
remove k (x:xs) | fst x == k = xs
                  | otherwise  = x:remove k xs

-- | Turn a list of pieces into a relative path to the root folder.
relativeDirFromPieces :: Pieces -> T.Text
relativeDirFromPieces pieces = T.concat $ map (const "../") (drop 1 pieces) -- last piece is not a dir

-- | Construct redirects with relative paths.
defaultMkRedirect :: Pieces -> ByteString -> S8.ByteString
defaultMkRedirect pieces newPath
    | S8.null newPath || S8.null relDir ||
      S8.last relDir /= '/' || S8.head newPath /= '/' =
        relDir `S8.append` newPath
    | otherwise = relDir `S8.append` S8.tail newPath
  where
    relDir = TE.encodeUtf8 (relativeDirFromPieces pieces)
