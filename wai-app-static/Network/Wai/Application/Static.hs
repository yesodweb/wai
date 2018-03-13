{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
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
    , ssAddTrailingSlash
    , ss404Handler
    ) where

import Prelude hiding (FilePath)
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Control.Monad.IO.Class (liftIO)

import Data.ByteString.Builder (toLazyByteString)

import Data.FileEmbed (embedFile)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Network.HTTP.Date (parseHTTPDate, epochTimeToHTTPDate, formatHTTPDate)

import WaiAppStatic.Types
import Util
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Storage.Embedded
import Network.Mime (MimeType)

data StaticResponse =
      -- | Just the etag hash or Nothing for no etag hash
      Redirect Pieces (Maybe ByteString)
    | RawRedirect ByteString
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
serveFolder StaticSettings {..} pieces req folder@Folder {..} =
    case ssListing of
        Just _ | Just path <- addTrailingSlash req, ssAddTrailingSlash ->
            return $ RawRedirect path
        Just listing -> do
            -- directory listings turned on, display it
            builder <- listing pieces folder
            return $ WaiResponse $ W.responseBuilder H.status200
                [ ("Content-Type", "text/html; charset=utf-8")
                ] builder
        Nothing -> return $ WaiResponse $ W.responseLBS H.status403
            [ ("Content-Type", "text/plain")
            ] "Directory listings disabled"

addTrailingSlash :: W.Request -> Maybe ByteString
addTrailingSlash req
    | S8.null rp = Just "/"
    | S8.last rp == '/' = Nothing
    | otherwise = Just $ S8.snoc rp '/'
  where
    rp = W.rawPathInfo req

checkPieces :: StaticSettings
            -> Pieces                    -- ^ parsed request
            -> W.Request
            -> IO StaticResponse
-- If we have any empty pieces in the middle of the requested path, generate a
-- redirect to get rid of them.
checkPieces _ pieces _ | any (T.null . fromPiece) $ safeInit pieces =
    return $ Redirect (filterButLast (not . T.null . fromPiece) pieces) Nothing

checkPieces ss@StaticSettings {..} pieces req = do
    res <- lookupResult
    case res of
        Left location -> return $ RawRedirect location
        Right LRNotFound -> return NotFound
        Right (LRFile file) -> serveFile ss req file
        Right (LRFolder folder) -> serveFolder ss pieces req folder
  where
    lookupResult :: IO (Either ByteString LookupResult)
    lookupResult = do
      nonIndexResult <- ssLookupFile pieces
      case nonIndexResult of
          LRFile{} -> return $ Right nonIndexResult
          _ -> do
              eIndexResult <- lookupIndices (map (\ index -> dropLastIfNull pieces ++ [index]) ssIndices)
              return $ case eIndexResult of
                  Left redirect -> Left redirect
                  Right indexResult -> case indexResult of
                      LRNotFound -> Right nonIndexResult
                      LRFile file | ssRedirectToIndex ->
                          let relPath =
                                  case reverse pieces of
                                      -- Served at root
                                      [] -> fromPiece $ fileName file
                                      lastSegment:_ ->
                                          case fromPiece lastSegment of
                                              -- Ends with a trailing slash
                                              "" -> fromPiece $ fileName file
                                              -- Lacks a trailing slash
                                              lastSegment' -> T.concat
                                                  [ lastSegment'
                                                  , "/"
                                                  , fromPiece $ fileName file
                                                  ]
                           in Left $ TE.encodeUtf8 relPath
                      _ -> Right indexResult

    lookupIndices :: [Pieces] -> IO (Either ByteString LookupResult)
    lookupIndices (x : xs) = do
        res <- ssLookupFile x
        case res of
            LRNotFound -> lookupIndices xs
            _ -> return $ case (ssAddTrailingSlash, addTrailingSlash req) of
                (True, Just redirect) -> Left redirect
                _ -> Right res
    lookupIndices [] = return $ Right LRNotFound

serveFile :: StaticSettings -> W.Request -> File -> IO StaticResponse
serveFile StaticSettings {..} req file
    -- First check etag values, if turned on
    | ssUseHash = do
        mHash <- fileGetHash file
        case (mHash, lookup "if-none-match" $ W.requestHeaders req) of
            -- if-none-match matches the actual hash, return a 304
            (Just hash, Just lastHash) | hash == lastHash -> return NotModified

            -- Didn't match, but we have a hash value. Send the file contents
            -- with an ETag header.
            --
            -- Note: It would be arguably better to next check
            -- if-modified-since and return a 304 if that indicates a match as
            -- well. However, the circumstances under which such a situation
            -- could arise would be very anomolous, and should likely warrant a
            -- new file being sent anyway.
            (Just hash, _) -> respond [("ETag", hash)]

            -- No hash value available, fall back to last modified support.
            (Nothing, _) -> lastMod
    -- etag turned off, so jump straight to last modified
    | otherwise = lastMod
  where
    mLastSent = lookup "if-modified-since" (W.requestHeaders req) >>= parseHTTPDate
    lastMod =
        case (fmap epochTimeToHTTPDate $ fileGetModified file, mLastSent) of
            -- File modified time is equal to the if-modified-since header,
            -- return a 304.
            --
            -- Question: should the comparison be, date <= lastSent?
            (Just mdate, Just lastSent)
                | mdate == lastSent -> return NotModified

            -- Did not match, but we have a new last-modified header
            (Just mdate, _) -> respond [("last-modified", formatHTTPDate mdate)]

            -- No modification time available
            (Nothing, _) -> respond []

    -- Send a file response with the additional weak headers provided.
    respond headers = return $ FileResponse file $ cacheControl ssMaxAge headers

-- | Return a difference list of headers based on the specified MaxAge.
--
-- This function will return both Cache-Control and Expires headers, as
-- relevant.
cacheControl :: MaxAge -> (H.ResponseHeaders -> H.ResponseHeaders)
cacheControl maxage =
    headerCacheControl . headerExpires
  where
    ccInt =
        case maxage of
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
      case maxage of
        NoMaxAge        -> id
        MaxAgeSeconds _ -> id -- FIXME
        MaxAgeForever   -> (:) ("Expires", "Thu, 31 Dec 2037 23:55:55 GMT")

-- | Turn a @StaticSettings@ into a WAI application.
staticApp :: StaticSettings -> W.Application
staticApp set req = staticAppPieces set (W.pathInfo req) req

staticAppPieces :: StaticSettings -> [Text] -> W.Application
staticAppPieces _ _ req sendResponse
    | notElem (W.requestMethod req) ["GET", "HEAD"] = sendResponse $ W.responseLBS
        H.status405
        [("Content-Type", "text/plain")]
        "Only GET or HEAD is supported"
staticAppPieces _ [".hidden", "folder.png"] _ sendResponse = sendResponse $ W.responseLBS H.status200 [("Content-Type", "image/png")] $ L.fromChunks [$(embedFile "images/folder.png")]
staticAppPieces _ [".hidden", "haskell.png"] _ sendResponse = sendResponse $ W.responseLBS H.status200 [("Content-Type", "image/png")] $ L.fromChunks [$(embedFile "images/haskell.png")]
staticAppPieces ss rawPieces req sendResponse = liftIO $ do
    case toPieces rawPieces of
        Just pieces -> checkPieces ss pieces req >>= response
        Nothing -> sendResponse $ W.responseLBS H.status403
            [ ("Content-Type", "text/plain")
            ] "Forbidden"
  where
    response :: StaticResponse -> IO W.ResponseReceived
    response (FileResponse file ch) = do
        mimetype <- ssGetMimeType ss file
        let filesize = fileGetSize file
        let headers = ("Content-Type", mimetype)
                    -- Let Warp provide the content-length, since it takes
                    -- range requests into account
                    -- : ("Content-Length", S8.pack $ show filesize)
                    : ch
        sendResponse $ fileToResponse file H.status200 headers

    response NotModified =
            sendResponse $ W.responseLBS H.status304 [] ""

    response (SendContent mt lbs) = do
            -- TODO: set caching headers
            sendResponse $ W.responseLBS H.status200
                [ ("Content-Type", mt)
                  -- TODO: set Content-Length
                ] lbs

    response (Redirect pieces' mHash) = do
            let loc = ssMkRedirect ss pieces' $ L.toStrict $ toLazyByteString (H.encodePathSegments $ map fromPiece pieces')
            let qString = case mHash of
                  Just hash -> replace "etag" (Just hash) (W.queryString req)
                  Nothing   -> remove "etag" (W.queryString req)

            sendResponse $ W.responseLBS H.status301
                [ ("Content-Type", "text/plain")
                , ("Location", S8.append loc $ H.renderQuery True qString)
                ] "Redirect"

    response (RawRedirect path) =
            sendResponse $ W.responseLBS H.status301
                [ ("Content-Type", "text/plain")
                , ("Location", path)
                ] "Redirect"

    response NotFound = case (ss404Handler ss) of
        Just app -> app req sendResponse
        Nothing  -> sendResponse $ W.responseLBS H.status404
                        [ ("Content-Type", "text/plain")
                        ] "File not found"

    response (WaiResponse r) = sendResponse r
