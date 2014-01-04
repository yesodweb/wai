{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Network.Wai.UrlMap (
    UrlMap,
    mount,
    mount',
    mountRoot,
    mapUrls
) where

import Control.Applicative
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Network.HTTP.Types
import Network.Wai

type Path = [Text]
newtype UrlMap' a = UrlMap' { unUrlMap :: [(Path, a)] }

instance Functor UrlMap' where
    fmap f (UrlMap' xs) = UrlMap' (fmap (\(p, a) -> (p, f a)) xs)

instance Applicative UrlMap' where
    pure x                        = UrlMap' [([], x)]
    (UrlMap' xs) <*> (UrlMap' ys) = UrlMap' [ (p, f y) |
                                              (p, y) <- ys,
                                              f <- map snd xs ]

instance Alternative UrlMap' where
    empty                         = UrlMap' empty
    (UrlMap' xs) <|> (UrlMap' ys) = UrlMap' (xs <|> ys)

type UrlMap = UrlMap' Application

-- | Mount an application under a given path.
mount' :: ToApplication a => Path -> a -> UrlMap
mount' prefix thing = UrlMap' [(prefix, toApplication thing)]

-- A little helper function, since most of the time, apps are mounted under
-- a single path segment.
mount :: ToApplication a => Text -> a -> UrlMap
mount prefix thing = mount' [prefix] thing

-- Another little helper function. Use this for the last mounted
-- application in the block, to avoid 500 errors from none matching.
mountRoot :: ToApplication a => a -> UrlMap
mountRoot = mount' []

try :: Eq a
    => [a] -- ^ Path info of request
    -> [([a], b)] -- ^ List of applications to match
    -> Maybe ([a], b)
try xs tuples = foldl go Nothing tuples
    where
        go (Just x) _ = Just x
        go _ (prefix, y) = stripPrefix prefix xs >>= \xs' -> return (xs', y)

class ToApplication a where
    toApplication :: a -> Application

instance ToApplication Application where
    toApplication = id

instance ToApplication UrlMap where
    toApplication urlMap = \req ->
        case try (pathInfo req) (unUrlMap urlMap) of
            Just (newPath, app) ->
                app $ req { pathInfo = newPath
                          , rawPathInfo = makeRaw newPath
                          }
            Nothing ->
                return $ responseLBS
                    status500
                    [("content-type", "text/plain")]
                    ("WaiUrlMapper: no routes matched. Consider using \
                     \mountRoot for the last mapping in the 'do' block.\n")

        where
        makeRaw :: [Text] -> B.ByteString
        makeRaw = ("/" `B.append`) . T.encodeUtf8 . T.intercalate "/"

mapUrls :: UrlMap -> Application
mapUrls = toApplication
