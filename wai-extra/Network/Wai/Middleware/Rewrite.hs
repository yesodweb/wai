{-# LANGUAGE TupleSections #-}

module Network.Wai.Middleware.Rewrite
    ( rewrite
    , rewritePure
    , rewriteWithQueries
    , rewritePureWithQueries
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

-- | A tuple of the path sections as '[Text]' and query parameters as
-- 'H.QueryText'.
type PathsAndQueries = ([Text], H.QueryText)

-- | rewrite based on your own conversion rules
rewrite :: ([Text] -> H.RequestHeaders -> IO [Text]) -> Middleware
rewrite convert app req sendResponse = do
  let convertIO = pathsOnly . curry $ liftIO . uncurry convert
  newReq <- rewriteRequestRawM convertIO req
  app newReq sendResponse

-- | rewrite based on your own conversion rules
-- Example convert function:

-- staticConvert :: [Text] -> H.RequestHeaders -> [Text]
-- staticConvert pieces _ = piecesConvert pieces
--   where
--    piecesConvert [] = ["static", "html", "pages.html"]
--    piecesConvert route@("pages":_) = "static":"html":route
rewritePure :: ([Text] -> H.RequestHeaders -> [Text]) -> Middleware
rewritePure convert app req =
  let convertPure = pathsOnly . curry $ Identity . uncurry convert
      newReq = runIdentity $ rewriteRequestRawM convertPure req
  in  app newReq

-- | rewrite based on your own conversion rules
-- Example convert function:

-- staticConvert :: [Text] -> H.RequestHeaders -> [Text]
-- staticConvert pieces _ = piecesConvert pieces
--   where
--    piecesConvert [] = ["static", "html", "pages.html"]
--    piecesConvert route@("pages":_) = "static":"html":route
rewriteWithQueries :: (PathsAndQueries -> H.RequestHeaders -> IO PathsAndQueries)
                   -> Middleware
rewriteWithQueries convert app req sendResponse = do
  newReq <- rewriteRequestM convert req
  app newReq sendResponse

-- | rewrite based on your own conversion rules
-- Example convert function:

-- staticConvert :: [Text] -> H.RequestHeaders -> [Text]
-- staticConvert pieces _ = piecesConvert pieces
--   where
--    piecesConvert [] = ["static", "html", "pages.html"]
--    piecesConvert route@("pages":_) = "static":"html":route
rewritePureWithQueries :: (PathsAndQueries -> H.RequestHeaders -> PathsAndQueries)
                       -> Middleware
rewritePureWithQueries convert app req = app $ rewriteRequestPure convert req

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

-- | This helper function factors out the common behaviour of rewriting requests.
rewriteRequestM :: Monad m
                => (PathsAndQueries -> H.RequestHeaders -> m PathsAndQueries)
                -> Request -> m Request
rewriteRequestM convert req = do
  (pInfo, qText) <- curry convert (pathInfo req) (H.queryToQueryText . queryString $ req) (requestHeaders req)
  pure req {pathInfo = pInfo, queryString = H.queryTextToQuery qText}
        -- rawPInfo = toS . toLazyByteString $ encodePathSegmentsRelative pInfo
        -- rawQString = toS . toLazyByteString $ renderQueryText True qText

-- | This helper function preserves the semantics of wai-extra <= 3.0, in
-- which the rewrite functions modify the `rawPathInfo` parameter. Note
-- that this has not been extended to modify the `rawQueryInfo` as
-- modifying either of these values has been deprecated.
rewriteRequestRawM :: Monad m
                    => (PathsAndQueries -> H.RequestHeaders -> m PathsAndQueries)
                    -> Request -> m Request
rewriteRequestRawM convert req = do
  newReq <- rewriteRequestM convert req
  let rawPInfo = TE.encodeUtf8 . T.intercalate "/" . pathInfo $ newReq
  pure newReq { rawPathInfo = rawPInfo }

-- | Produce a function that works on 'PathsandQueries' from one working
-- only on paths.
pathsOnly :: Monad m
          => ([Text] -> H.RequestHeaders -> m [Text])
          -> PathsAndQueries -> H.RequestHeaders -> m PathsAndQueries
pathsOnly convert paths headers = (,[]) <$> convert (fst paths) headers
{-# INLINE pathsOnly #-}
