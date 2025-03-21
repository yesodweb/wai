{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module gives you a way to mount applications under sub-URIs.
-- For example:
--
-- > bugsApp, helpdeskApp, apiV1, apiV2, mainApp :: Application
-- >
-- > myApp :: Application
-- > myApp = mapUrls $
-- >       mount "bugs"     bugsApp
-- >   <|> mount "helpdesk" helpdeskApp
-- >   <|> mount "api"
-- >           (   mount "v1" apiV1
-- >           <|> mount "v2" apiV2
-- >           )
-- >   <|> mountRoot mainApp
module Network.Wai.UrlMap (
    UrlMap',
    UrlMap,
    mount',
    mount,
    mountRoot,
    mapUrls,
) where

import Control.Applicative
import qualified Data.ByteString as B
import Data.List (stripPrefix)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (hContentType, status404)
import Network.Wai (Application, Request (pathInfo, rawPathInfo), responseLBS)

type Path = [Text]
newtype UrlMap' a = UrlMap' {unUrlMap :: [(Path, a)]}

instance Functor UrlMap' where
    fmap f (UrlMap' xs) = UrlMap' (fmap (fmap f) xs)

instance Applicative UrlMap' where
    pure x = UrlMap' [([], x)]
    (UrlMap' xs) <*> (UrlMap' ys) =
        UrlMap'
            [ (p, f y)
            | (p, y) <- ys
            , f <- map snd xs
            ]

instance Alternative UrlMap' where
    empty = UrlMap' empty
    (UrlMap' xs) <|> (UrlMap' ys) = UrlMap' (xs <|> ys)

-- | @since 3.1.18
instance Foldable UrlMap' where
    foldr f z (UrlMap' xs) = foldr (f . snd) z xs

-- | @since 3.1.18
instance Traversable UrlMap' where
    traverse f (UrlMap' xs) = UrlMap' <$> traverse (traverse f) xs

type UrlMap = UrlMap' Application

-- | Mount an application under a given path. The ToApplication typeclass gives
-- you the option to pass either an 'Network.Wai.Application' or an 'UrlMap'
-- as the second argument.
mount' :: ToApplication a => Path -> a -> UrlMap
mount' prefix thing = UrlMap' [(prefix, toApplication thing)]

-- | A convenience function like mount', but for mounting things under a single
-- path segment.
mount :: ToApplication a => Text -> a -> UrlMap
mount prefix = mount' [prefix]

-- | Mount something at the root. Use this for the last application in the
-- block, to avoid 500 errors from none of the applications matching.
mountRoot :: ToApplication a => a -> UrlMap
mountRoot = mount' []

try
    :: Eq a
    => [a]
    -- ^ Path info of request
    -> [([a], b)]
    -- ^ List of applications to match
    -> Maybe ([a], b)
try xs = foldl go Nothing
  where
    go (Just x) _ = Just x
    go _ (prefix, y) = stripPrefix prefix xs >>= \xs' -> return (xs', y)

class ToApplication a where
    toApplication :: a -> Application

instance ToApplication Application where
    toApplication = id

instance ToApplication UrlMap where
    toApplication urlMap req sendResponse =
        case try (pathInfo req) (unUrlMap urlMap) of
            Just (newPath, app) ->
                app
                    ( req
                        { pathInfo = newPath
                        , rawPathInfo = makeRaw newPath
                        }
                    )
                    sendResponse
            Nothing ->
                sendResponse $
                    responseLBS
                        status404
                        [(hContentType, "text/plain")]
                        "Not found\n"
      where
        makeRaw :: [Text] -> B.ByteString
        makeRaw = ("/" `B.append`) . T.encodeUtf8 . T.intercalate "/"

mapUrls :: UrlMap -> Application
mapUrls = toApplication
