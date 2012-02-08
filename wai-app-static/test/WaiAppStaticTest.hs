{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module WaiAppStaticTest (specs) where 

import Network.Wai.Application.Static

import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit ()
import Test.HUnit ((@?=))
import Data.List (isInfixOf)
import qualified Data.ByteString.Char8 as S8
-- import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.PosixCompat.Files (getFileStatus, modificationTime)

import Network.HTTP.Date
{-import System.Locale (defaultTimeLocale)-}
{-import Data.Time.Format (formatTime)-}

import Network.Wai
import Network.Wai.Test

import Network.Socket.Internal as Sock
import qualified Network.HTTP.Types as H
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mempty)

defRequest :: Request
defRequest = defaultRequest

specs :: Specs
specs = do
  let webApp = flip runSession $ staticApp defaultWebAppSettings  {ssFolder = fileSystemLookup "test"}
  let fileServerApp = flip runSession $ staticApp defaultFileServerSettings  {ssFolder = fileSystemLookup "test"}

  let etag = "1B2M2Y8AsgTpgAmY7PhCfg=="
  let file = "a/b"
  let statFile = setRawPathInfo defRequest file

  describe "Pieces: pathFromPieces" $ do
    it "converts to a file path" $
      (pathFromPieces "prefix" ["a", "bc"]) @?= "prefix/a/bc"

    prop "each piece is in file path" $ \piecesS ->
      let pieces = map (FilePath . T.pack) piecesS
      in  all (\p -> ("/" ++ p) `isInfixOf` (T.unpack $ unFilePath $ pathFromPieces "root" $ pieces)) piecesS

  describe "webApp" $ do
    it "403 for unsafe paths" $ webApp $
      flip mapM_ ["..", "."] $ \path ->
        assertStatus 403 =<<
          request (setRawPathInfo defRequest path)

    it "200 for hidden paths" $ webApp $
      flip mapM_ [".hidden/folder.png", ".hidden/haskell.png"] $ \path ->
        assertStatus 200 =<<
          request (setRawPathInfo defRequest path)

    it "404 for non-existant files" $ webApp $
      assertStatus 404 =<<
        request (setRawPathInfo defRequest "doesNotExist")

    it "301 redirect when multiple slashes" $ webApp $ do
      req <- request (setRawPathInfo defRequest "a//b/c")
      assertStatus 301 req
      assertHeader "Location" "../../a/b/c" req

    let absoluteApp = flip runSession $ staticApp $ defaultWebAppSettings {
          ssFolder = fileSystemLookup "test", ssMkRedirect = \_ u -> S8.append "http://www.example.com" u
        }
    it "301 redirect when multiple slashes" $ absoluteApp $
      flip mapM_ ["/a//b/c", "a//b/c"] $ \path -> do
        req <- request (setRawPathInfo defRequest path)
        assertStatus 301 req
        assertHeader "Location" "http://www.example.com/a/b/c" req

  describe "webApp when requesting a static asset" $ do
    it "200 and etag when no etag query parameters" $ webApp $ do
      req <- request statFile
      assertStatus 200 req
      assertNoHeader "Cache-Control" req
      assertHeader "ETag" etag req
      assertNoHeader "Last-Modified" req

    it "200 when no cache headers and bad cache query string" $ webApp $ do
      flip mapM_ [Just "cached", Nothing] $ \badETag -> do
        req <- request statFile { queryString = [("etag", badETag)] }
        assertStatus 301 req
        assertHeader "Location" "../a/b?etag=1B2M2Y8AsgTpgAmY7PhCfg%3D%3D" req
        assertNoHeader "Cache-Control" req
        assertNoHeader "Last-Modified" req

    it "Cache-Control set when etag parameter is correct" $ webApp $ do
      req <- request statFile { queryString = [("etag", Just etag)] }
      assertStatus 200 req
      assertHeader "Cache-Control" "max-age=31536000" req
      assertNoHeader "Last-Modified" req

    it "200 when invalid in-none-match sent" $ webApp $
      flip mapM_ ["cached", ""] $ \badETag -> do
        req <- request statFile { requestHeaders  = [("If-None-Match", badETag)] }
        assertStatus 200 req
        assertHeader "ETag" etag req
        assertNoHeader "Last-Modified" req

    it "304 when valid if-none-match sent" $ webApp $ do
      req <- request statFile { requestHeaders  = [("If-None-Match", etag)] }
      assertStatus 304 req
      assertNoHeader "Etag" req
      assertNoHeader "Last-Modified" req

  describe "fileServerApp" $ do
    let fileDate = do
          stat <- liftIO $ getFileStatus $ "test/" ++ file
          return $ formatHTTPDate . epochTimeToHTTPDate $ modificationTime stat

    it "directory listing for index" $ fileServerApp $ do
      resp <- request (setRawPathInfo defRequest "a/")
      assertStatus 200 resp
      -- note the unclosed img tags so both /> and > will pass
      assertBodyContains "<img src=\"../.hidden/haskell.png\"" resp
      assertBodyContains "<img src=\"../.hidden/folder.png\" alt=\"Folder\"" resp
      assertBodyContains "<a href=\"b\">b</a>" resp

    it "200 when invalid if-modified-since header" $ fileServerApp $ do
      flip mapM_ ["123", ""] $ \badDate -> do
        req <- request statFile {
          requestHeaders = [("If-Modified-Since", badDate)]
        }
        assertStatus 200 req
        assertNoHeader "Cache-Control" req
        fdate <- fileDate
        assertHeader "Last-Modified" fdate req

    it "304 when if-modified-since matches" $ fileServerApp $ do
      fdate <- fileDate
      req <- request statFile {
        requestHeaders = [("If-Modified-Since", fdate)]
      }
      assertStatus 304 req
      assertNoHeader "Cache-Control" req

