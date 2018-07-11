{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Middleware.Rewrite
    ( -- * How to use this module
      -- $howto

      -- ** A note on semantics

      -- $semantics

      -- ** Paths and Queries

      -- $pathsandqueries
      PathsAndQueries

      -- ** An example rewriting paths with queries

      -- $takeover

      -- ** Upgrading from wai-extra ≤ 3.0.16.1

      -- $upgrading

      -- * 'Middleware'

      -- ** Recommended functions
    , rewriteWithQueries
    , rewritePureWithQueries
    , rewriteRoot

      -- ** Deprecated
    , rewrite
    , rewritePure

      -- * Operating on 'Request's

    , rewriteRequest
    , rewriteRequestPure
    ) where

import Network.Wai
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Functor.Identity (Identity(..))
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Network.HTTP.Types as H

-- GHC ≤ 7.10 does not export Applicative functions from the prelude.
#if __GLASGOW_HASKELL__ <= 710
import Control.Applicative
#endif

-- $howto
-- This module provides 'Middleware' to rewrite URL paths. It also provides
-- functions that will convert a 'Request' to a modified 'Request'.
-- Both operations require a function that takes URL parameters and
-- headers, and returns new URL parameters. Parameters are pieces of URL
-- paths and query parameters.
--
-- If you are a new user of the library, use 'rewriteWithQueries' or
-- 'rewritePureWithQueries' for middleware. For modifying 'Request's
-- directly, use 'rewriteRequest' or 'rewriteRequestPure'.

-- $semantics
--
-- Versions of this library in wai-extra ≤ 3.0.16.1 exported only
-- 'rewrite' and 'rewritePure' and both modified 'rawPathInfo' of the
-- underlying requests. Such modification has been proscribed. The
-- semantics of these functions have not changed; instead the recommended
-- approach is to use 'rewriteWithQueries' and 'rewritePureWithQueries'.
-- The new functions are slightly different, as described in the section
-- on upgrading; code for previous library versions can be upgraded with
-- a single change, and as the type of the new function is different the
-- compiler will indicate where this change must be made.
--
-- The 'rewriteRequest' and 'rewriteRequestPure' functions use the new
-- semantics, too.

-- $pathsandqueries
--
-- This library defines the type synonym `PathsAndQueries` to make code
-- handling paths and queries easier to read.
--
-- /e.g./ /\/foo\/bar/ would look like
--
-- > ["foo", "bar"] :: Text
--
-- /?bar=baz/ would look like
--
-- > [("bar", Just "baz")] :: QueryText
--
-- Together,
--
-- /\/foo?bar=baz/ would look like
--
-- > (["foo"],[("bar", Just "baz")]) :: PathsAndQueries

-- $takeover
-- Let’s say we want to replace a website written in PHP with one written
-- using WAI. We’ll use the
-- <https://hackage.haskell.org/package/http-reverse-proxy http-reverse-proxy>
-- package to serve the old
-- site from the new site, but there’s a problem. The old site uses pages like
--
-- @
-- index.php?page=/page/
-- @
--
-- whereas the new site would look like
--
-- @
-- index\//page/
-- @
--
-- In doing this, we want to separate the migration code from our new
-- website. So we’d like to handle links internally using the path
-- formulation, but externally have the old links still work.
--
-- Therefore, we will use middleware ('rewritePureWithQueries') from this
-- module to rewrite incoming requests from the query formulation to the
-- paths formulation.
--
-- > {-# LANGUAGE ViewPatterns #-}
-- >
-- > rewritePathFromPhp :: Middleware
-- > rewritePathFromPhp = rewritePureWithQueries pathFromPhp
-- >
-- > pathFromPhp :: PathsAndQueries -> H.RequestHeaders -> PathsAndQueries
-- > pathFromPhp (pieces, queries) _ = piecesConvert pieces queries
-- >     where
-- >         piecesConvert :: [Text] -> H.Query -> PathsAndQueries
-- >         piecesConvert ["index.php"] qs@(join . lookup "page" -> Just page) =
-- >             ( ["index", TE.decodeUtf8With TE.lenientDecode page]
-- >             , delete ("page", pure page) qs
-- >             )
-- >         piecesConvert ps qs = (ps, qs)
--
-- On the other side, we will use 'rewriteRequestPure' to rewrite outgoing
-- requests to the original website from the reverse proxy code (using the
-- 'Network.HTTP.ReverseProxy.WPRModifiedRequest' or
-- 'Network.HTTP.ReverseProxy.WPRModifiedRequestSecure' constructors. Note,
-- these links will only work if the haddock documentation for
-- <https://hackage.haskell.org/package/http-reverse-proxy http-reverse-proxy>
-- is installed).
--
-- > rewritePhpFromPath :: Request -> Request
-- > rewritePhpFromPath = rewriteRequestPure phpFromPath
-- >
-- > phpFromPath :: PathsAndQueries -> H.RequestHeaders -> PathsAndQueries
-- > phpFromPath (pieces, queries) _ = piecesConvert pieces queries
-- >     where
-- >         piecesConvert :: [Text] -> H.Query -> PathsAndQueries
-- >         piecesConvert ["index", page] qs = ( ["index.php"], ("page", pure . TE.encodeUtf8 $ page) : qs )
-- >         piecesConvert ps qs = (ps, qs)
--
-- For the whole example, see
-- <https://gist.github.com/dbaynard/c844d0df124f68ec8b6da152c581ce6d>.

-- $upgrading
-- It is quite simple to upgrade from 'rewrite' and 'rewritePure', to
-- 'rewriteWithQueries' and 'rewritePureWithQueries'.
-- Insert 'Data.Bifunctor.first', which specialises to
--
-- @
-- 'Data.Bifunctor.first' :: (['Text'] -> ['Text']) -> 'PathsAndQueries' -> 'PathsAndQueries'
-- @
--
-- as the following example demonstrates.
--
-- Old versions of the libary could only handle path pieces, not queries.
-- This could have been supplied to 'rewritePure'.
--
-- @
-- staticConvert' :: [Text] -> H.RequestHeaders -> [Text]
-- staticConvert' pieces _ = piecesConvert pieces
--   where
--    piecesConvert [] = ["static", "html", "pages.html"]
--    piecesConvert route@("pages":_) = "static":"html":route
-- @
--
-- Instead, use this function, supplied to 'rewritePureWithQueries'.
--
-- @
-- staticConvert :: 'PathsAndQueries' -> H.RequestHeaders -> 'PathsAndQueries'
-- staticConvert pathsAndQueries _ = 'Data.Bifunctor.first' piecesConvert pathsAndQueries
--   where
--    piecesConvert [] = ["static", "html", "pages.html"]
--    piecesConvert route@("pages":_) = "static":"html":route
-- @
--
-- The former formulation is deprecated for two reasons:
--
--   1. The original formulation of 'rewrite' modified 'rawPathInfo', which
--   is deprecated behaviour.
--
--   2. The original formulation did not allow query parameters to
--   influence the path.
--
-- Concerning the first point, take care with semantics of your program when
-- upgrading as the upgraded functions no longer modify 'rawPathInfo'.

--------------------------------------------------
-- * Types
--------------------------------------------------

-- | A tuple of the path sections as ['Text'] and query parameters as
-- 'H.Query'. This makes writing type signatures for the conversion
-- function far more pleasant.
--
-- Note that this uses 'H.Query' not 'H.QueryText' to more accurately
-- reflect the paramaters that can be supplied in URLs. It may be safe to
-- treat parameters as text; use the 'H.queryToQueryText' and
-- 'H.queryTextToQuery' functions to interconvert.
type PathsAndQueries = ([Text], H.Query)

--------------------------------------------------
-- * Rewriting 'Middleware'
--------------------------------------------------

-- | Rewrite based on your own conversion function for paths only, to be
-- supplied by users of this library (with the conversion operating in 'IO').
--
-- For new code, use 'rewriteWithQueries' instead.
rewrite :: ([Text] -> H.RequestHeaders -> IO [Text]) -> Middleware
rewrite convert app req sendResponse = do
  let convertIO = pathsOnly . curry $ liftIO . uncurry convert
  newReq <- rewriteRequestRawM convertIO req
  app newReq sendResponse
{-# WARNING rewrite [
          "This modifies the 'rawPathInfo' field of a 'Request'."
        , " This is not recommended behaviour; it is however how"
        , " this function has worked in the past."
        , " Use 'rewriteWithQueries' instead"] #-}

-- | Rewrite based on pure conversion function for paths only, to be
-- supplied by users of this library.
--
-- For new code, use 'rewritePureWithQueries' instead.
rewritePure :: ([Text] -> H.RequestHeaders -> [Text]) -> Middleware
rewritePure convert app req =
  let convertPure = pathsOnly . curry $ Identity . uncurry convert
      newReq = runIdentity $ rewriteRequestRawM convertPure req
  in  app newReq
{-# WARNING rewritePure [
          "This modifies the 'rawPathInfo' field of a 'Request'."
        , " This is not recommended behaviour; it is however how"
        , " this function has worked in the past."
        , " Use 'rewritePureWithQueries' instead"] #-}

-- | Rewrite based on your own conversion function for paths and queries.
-- This function is to be supplied by users of this library, and operates
-- in 'IO'.
rewriteWithQueries :: (PathsAndQueries -> H.RequestHeaders -> IO PathsAndQueries)
                   -> Middleware
rewriteWithQueries convert app req sendResponse = do
  newReq <- rewriteRequestM convert req
  app newReq sendResponse

-- | Rewrite based on pure conversion function for paths and queries. This
-- function is to be supplied by users of this library.
rewritePureWithQueries :: (PathsAndQueries -> H.RequestHeaders -> PathsAndQueries)
                       -> Middleware
rewritePureWithQueries convert app req = app $ rewriteRequestPure convert req

-- | Rewrite root requests (/) to a specified path
--
-- Note that /index.html/ in example below should already be a valid route.
--
-- @
--     rewriteRoot "index.html" :: Middleware
-- @
--
-- @since 3.0.23.0
rewriteRoot :: Text -> Middleware
rewriteRoot root = rewritePureWithQueries onlyRoot
  where
    onlyRoot ([], q) _ = ([root], q)
    onlyRoot paths _ = paths

--------------------------------------------------
-- * Modifying 'Request's directly
--------------------------------------------------

-- | Modify a 'Request' using the supplied function in 'IO'. This is suitable for
-- the reverse proxy example.
rewriteRequest :: (PathsAndQueries -> H.RequestHeaders -> IO PathsAndQueries)
               -> Request -> IO Request
rewriteRequest convert req =
  let convertIO = curry $ liftIO . uncurry convert
  in  rewriteRequestRawM convertIO req

-- | Modify a 'Request' using the pure supplied function. This is suitable for
-- the reverse proxy example.
rewriteRequestPure :: (PathsAndQueries -> H.RequestHeaders -> PathsAndQueries)
                   -> Request -> Request
rewriteRequestPure convert req =
  let convertPure = curry $ Identity . uncurry convert
  in  runIdentity $ rewriteRequestRawM convertPure req

--------------------------------------------------
-- * Helper functions
--------------------------------------------------

-- | This helper function factors out the common behaviour of rewriting requests.
rewriteRequestM :: (Applicative m, Monad m)
                => (PathsAndQueries -> H.RequestHeaders -> m PathsAndQueries)
                -> Request -> m Request
rewriteRequestM convert req = do
  (pInfo, qByteStrings) <- curry convert (pathInfo req) (queryString req) (requestHeaders req)
  pure req {pathInfo = pInfo, queryString = qByteStrings}

-- | This helper function preserves the semantics of wai-extra ≤ 3.0, in
-- which the rewrite functions modify the 'rawPathInfo' parameter. Note
-- that this has not been extended to modify the 'rawQueryInfo' as
-- modifying either of these values has been deprecated.
rewriteRequestRawM :: (Applicative m, Monad m)
                    => (PathsAndQueries -> H.RequestHeaders -> m PathsAndQueries)
                    -> Request -> m Request
rewriteRequestRawM convert req = do
  newReq <- rewriteRequestM convert req
  let rawPInfo = TE.encodeUtf8 . T.intercalate "/" . pathInfo $ newReq
  pure newReq { rawPathInfo = rawPInfo }
{-# WARNING rewriteRequestRawM [
          "This modifies the 'rawPathInfo' field of a 'Request'."
        , " This is not recommended behaviour; it is however how"
        , " this function has worked in the past."
        , " Use 'rewriteRequestM' instead"] #-}

-- | Produce a function that works on 'PathsAndQueries' from one working
-- only on paths. This is not exported, as it is only needed to handle
-- code written for versions ≤ 3.0 of the library; see the
-- example above using 'Data.Bifunctor.first' to do something similar.
pathsOnly :: (Applicative m, Monad m)
          => ([Text] -> H.RequestHeaders -> m [Text])
          -> PathsAndQueries -> H.RequestHeaders -> m PathsAndQueries
pathsOnly convert psAndQs headers = (,[]) <$> convert (fst psAndQs) headers
{-# INLINE pathsOnly #-}
