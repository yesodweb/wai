{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module WaiAppStaticTest (spec) where

import Network.Wai.Application.Static

import Test.Hspec
import qualified Data.ByteString.Char8 as S8
-- import qualified Data.ByteString.Lazy.Char8 as L8
import System.PosixCompat.Files (getFileStatus, modificationTime)

import Network.HTTP.Date
import Network.HTTP.Types (status500)
{-import System.Locale (defaultTimeLocale)-}
{-import Data.Time.Format (formatTime)-}

import Network.Wai
import Network.Wai.Test

import Control.Monad.IO.Class (liftIO)
import Network.Mime

defRequest :: Request
defRequest = defaultRequest

spec :: Spec
spec = do
  let webApp = flip runSession $ staticApp $ defaultWebAppSettings "test"
  let fileServerApp = flip runSession $ staticApp (defaultFileServerSettings "test")
        { ssAddTrailingSlash = True
        }

  let etag = "1B2M2Y8AsgTpgAmY7PhCfg=="
  let file = "a/b"
  let statFile = setRawPathInfo defRequest file

  describe "mime types" $ do
    it "fileNameExtensions" $
        fileNameExtensions "foo.tar.gz" `shouldBe` ["tar.gz", "gz"]
    it "handles multi-extensions" $
        defaultMimeLookup "foo.tar.gz" `shouldBe` "application/x-tgz"
    it "defaults correctly" $
        defaultMimeLookup "foo.unknown" `shouldBe` "application/octet-stream"

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

    let absoluteApp = flip runSession $ staticApp $ (defaultWebAppSettings "test") {
          ssMkRedirect = \_ u -> S8.append "http://www.example.com" u
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
      assertHeader "ETag" etag req
      assertNoHeader "Last-Modified" req

    it "Cache-Control set when etag parameter is correct" $ webApp $ do
      req <- request statFile { queryString = [("etag", Just etag)] }
      assertStatus 200 req
      assertHeader "Cache-Control" "public, max-age=31536000" req
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
        fdate <- fileDate
        assertHeader "Last-Modified" fdate req

    it "304 when if-modified-since matches" $ fileServerApp $ do
      fdate <- fileDate
      req <- request statFile {
        requestHeaders = [("If-Modified-Since", fdate)]
      }
      assertStatus 304 req
      assertNoHeader "Cache-Control" req

    context "301 redirect to add a trailing slash on directories if missing" $ do
      it "works at the root" $ fileServerApp $ do
        req <- request (setRawPathInfo defRequest "/a")
        assertStatus 301 req
        assertHeader "Location" "/a/" req

      let urlMapApp = flip runSession $ \req send ->
            case pathInfo req of
                "subPath":rest ->
                    let req' = req { pathInfo = rest }
                     in (staticApp (defaultFileServerSettings "test")
                            { ssAddTrailingSlash = True
                            }) req' send
                _ -> send $ responseLBS status500 []
                    "urlMapApp: only works at subPath"
      it "works with subpath at the root of the file server" $ urlMapApp $ do
        req <- request (setRawPathInfo defRequest "/subPath")
        assertStatus 301 req
        assertHeader "Location" "/subPath/" req
