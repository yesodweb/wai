{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, CPP #-}
{-# LANGUAGE RecordWildCards #-}
-- | Static file serving for WAI.
module Network.Wai.Application.Static
    ( -- * WAI application
      staticApp
      -- ** Default Settings
    , defaultWebAppSettings
    , webAppSettingsWithLookup
    , defaultFileServerSettings
    , embeddedSettings
      -- ** Settings
    , StaticSettings
    , ssLookupFile
    , ssMkRedirect
    , ssGetMimeType
    , ssListing
    , ssIndices
    , ssMaxAge
    , ssRedirectToIndex
    ) where

import Prelude hiding (FilePath)
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Control.Monad.IO.Class (liftIO)

import Blaze.ByteString.Builder (toByteString)

import Data.FileEmbed (embedFile)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Either (rights)
import Data.Maybe (isJust, fromJust)
import Network.HTTP.Date (parseHTTPDate, epochTimeToHTTPDate, formatHTTPDate)
import Data.Monoid (First (First, getFirst), mconcat)

import WaiAppStatic.Types
import Util
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Storage.Embedded

data StaticResponse =
      -- | Just the etag hash or Nothing for no etag hash
      Redirect Pieces (Maybe ByteString)
    | NotFound
    | FileResponse File H.ResponseHeaders
    | NotModified
    -- TODO: add file size
    | SendContent MimeType L.ByteString
    | WaiResponse W.Response

safeInit  :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

filterButLast :: (a -> Bool) -> [a] -> [a]
filterButLast _ [] = []
filterButLast _ [x] = [x]
filterButLast f (x:xs)
    | f x = x : filterButLast f xs
    | otherwise = filterButLast f xs

-- | Serve an appropriate response for a folder request.
serveFolder :: StaticSettings -> Pieces -> W.Request -> Folder -> IO StaticResponse
serveFolder ss@StaticSettings {..} pieces req folder@Folder {..} =
    -- first check if there is an index file in this folder
    case getFirst $ mconcat $ map (findIndex $ rights folderContents) ssIndices of
        Just index -> do
            let pieces' = setLast pieces index
             in if ssRedirectToIndex
                    then return $ Redirect pieces' Nothing
                    -- start the checking process over, with a new set
                    else checkPieces ss pieces' req
        Nothing ->
            case ssListing of
                Just listing -> do
                    -- directory listings turned on, display it
                    builder <- listing pieces folder
                    return $ WaiResponse $ W.ResponseBuilder H.status200
                        [ ("Content-Type", "text/html; charset=utf-8")
                        ] builder
                Nothing -> return $ WaiResponse $ W.responseLBS H.status403
                    [ ("Content-Type", "text/plain")
                    ] "Directory listings disabled"
  where
    setLast :: Pieces -> Piece -> Pieces
    setLast [] x = [x]
    setLast [t] x
        | fromPiece t == "" = [x]
    setLast (a:b) x = a : setLast b x

    findIndex :: [File] -> Piece -> First Piece
    findIndex files index
        | index `elem` map fileName files = First $ Just index
        | otherwise = First Nothing

checkPieces :: StaticSettings
            -> Pieces                    -- ^ parsed request
            -> W.Request
            -> IO StaticResponse
-- If we have any empty pieces in the middle of the requested path, generate a
-- redirect to get rid of them.
checkPieces _ pieces _ | any (T.null . fromPiece) $ safeInit pieces =
    return $ Redirect (filterButLast (not . T.null . fromPiece) pieces) Nothing

checkPieces ss@StaticSettings {..} pieces req = do
    res <- ssLookupFile pieces
    case res of
        LRNotFound -> return NotFound
        LRFile file -> serveFile ss pieces req file
        LRFolder folder -> serveFolder ss pieces req folder

serveFile :: StaticSettings -> Pieces -> W.Request -> File -> IO StaticResponse
serveFile StaticSettings {..} pieces req file =
    handleCache
  where
    headers = W.requestHeaders req
    queryString = W.queryString req

    -- FIXME This whole thing seems like a mess.

    -- HTTP caching has a cache control header that you can set an expire time for a resource.
    --   Max-Age is easiest because it is a simple number
    --   a cache-control asset will only be downloaded once (if the browser maintains its cache)
    --   and the server will never be contacted for the resource again (until it expires)
    --
    -- A second caching mechanism is ETag and last-modified
    --   this form of caching is not as good as the static- the browser can avoid downloading the file, but it always need to send a request with the etag value or the last-modified value to the server to see if its copy is up to date
    --
    -- We should set a cache control and one of ETag or last-modifed whenever possible
    --
    -- In a Yesod web application we can append an etag parameter to static assets.
    -- This signals that both a max-age and ETag header should be set
    -- if there is no etag parameter
    -- * don't set the max-age
    -- * set ETag or last-modified
    --   * ETag must be calculated ahead of time.
    --   * last-modified is just the file mtime.
    handleCache =
      if not ssUseHash then lastModifiedCache
        else do
          let etagParam = lookup "etag" queryString

          case etagParam of
            Nothing -> do -- no query parameter. Set appropriate ETag headers
                mHash <- fileGetHash file
                case mHash of
                    Nothing -> lastModifiedCache
                    Just hash ->
                        case lookup "if-none-match" headers of
                            Just lastHash ->
                              if hash == lastHash
                                  then return NotModified
                                  else return $ FileResponse file $ [("ETag", hash)]
                            Nothing -> return $ FileResponse file $ [("ETag", hash)]

            Just mEtag -> do
                mHash <- fileGetHash file
                case mHash of
                  -- a file used to have an etag parameter, but no longer does
                  Nothing -> return $ Redirect pieces Nothing
                  Just hash ->
                    if isJust mEtag && hash == fromJust mEtag
                      then return $ FileResponse file $ ("ETag", hash):cacheControl
                      else return $ Redirect pieces (Just hash)


    lastModifiedCache =
      case (lookup "if-modified-since" headers >>= parseHTTPDate, fileGetModified file) of
          (mLastSent, Just modified) -> do
            let mdate = epochTimeToHTTPDate modified in
              case mLastSent of
                Just lastSent ->
                  if lastSent == mdate
                      then return NotModified
                      else return $ FileResponse file $ [("last-modified", formatHTTPDate mdate)]
                Nothing -> return $ FileResponse file $ [("last-modified", formatHTTPDate mdate)]
          _ -> return $ FileResponse file []

    cacheControl = headerCacheControl $ headerExpires []
      where
        ccInt =
            case ssMaxAge of
                NoMaxAge -> Nothing
                MaxAgeSeconds i -> Just i
                MaxAgeForever -> Just oneYear
        oneYear :: Int
        oneYear = 60 * 60 * 24 * 365

        headerCacheControl =
          case ccInt of
            Nothing -> id
            Just i  -> (:) ("Cache-Control", S8.append "public, max-age=" $ S8.pack $ show i)
        headerExpires =
          case ssMaxAge of
            NoMaxAge        -> id
            MaxAgeSeconds _ -> id -- FIXME
            MaxAgeForever   -> (:) ("Expires", "Thu, 31 Dec 2037 23:55:55 GMT")

-- | Turn a @StaticSettings@ into a WAI application.
staticApp :: StaticSettings -> W.Application
staticApp set req = staticAppPieces set (W.pathInfo req) req

staticAppPieces :: StaticSettings -> [Text] -> W.Application
staticAppPieces _ _ req
    | W.requestMethod req /= "GET" = return $ W.responseLBS
        H.status405
        [("Content-Type", "text/plain")]
        "Only GET is supported"
staticAppPieces _ [".hidden", "folder.png"] _  = return $ W.responseLBS H.status200 [("Content-Type", "image/png")] $ L.fromChunks [$(embedFile "images/folder.png")]
staticAppPieces _ [".hidden", "haskell.png"] _ = return $ W.responseLBS H.status200 [("Content-Type", "image/png")] $ L.fromChunks [$(embedFile "images/haskell.png")]
staticAppPieces ss rawPieces req = liftIO $ do
    case toPieces rawPieces of
        Just pieces -> checkPieces ss pieces req >>= response
        Nothing -> return $ W.responseLBS H.status403
            [ ("Content-Type", "text/plain")
            ] "Forbidden"
  where
    response :: StaticResponse -> IO W.Response
    response (FileResponse file ch) = do
        mimetype <- ssGetMimeType ss file
        let filesize = fileGetSize file
        let headers = ("Content-Type", mimetype)
                    : ("Content-Length", S8.pack $ show filesize)
                    : ch
        return $ fileToResponse file H.status200 headers

    response NotModified =
            return $ W.responseLBS H.status304
                        [ ("Content-Type", "text/plain")
                        ] "Not Modified"

    response (SendContent mt lbs) = do
            -- TODO: set caching headers
            return $ W.responseLBS H.status200
                [ ("Content-Type", mt)
                  -- TODO: set Content-Length
                ] lbs

    response (Redirect pieces' mHash) = do
            let loc = (ssMkRedirect ss) pieces' $ toByteString (H.encodePathSegments $ map fromPiece pieces')
            let qString = case mHash of
                  Just hash -> replace "etag" (Just hash) (W.queryString req)
                  Nothing   -> remove "etag" (W.queryString req)

            return $ W.responseLBS H.status301
                [ ("Content-Type", "text/plain")
                , ("Location", S8.append loc $ H.renderQuery True qString)
                ] "Redirect"

    response NotFound = return $ W.responseLBS H.status404
                        [ ("Content-Type", "text/plain")
                        ] "File not found"

    response (WaiResponse r) = return r
