{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.TestSpec (main, spec) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.IORef as IORef
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Network.HTTP.Types (status200)
import Network.Wai
import Test.Hspec
import qualified Web.Cookie as Cookie

import Network.Wai.Test

main :: IO ()
main = hspec spec

toByteString :: Builder -> ByteString
toByteString = L8.toStrict . toLazyByteString

spec :: Spec
spec = do
  describe "setPath" $ do

    let req = setPath defaultRequest "/foo/bar/baz?foo=23&bar=42&baz"

    it "sets pathInfo" $ do
      pathInfo req `shouldBe` ["foo", "bar", "baz"]

    it "utf8 path" $
      pathInfo (setPath defaultRequest "/foo/%D7%A9%D7%9C%D7%95%D7%9D/bar") `shouldBe`
        ["foo", "שלום", "bar"]

    it "sets rawPathInfo" $ do
      rawPathInfo req `shouldBe` "/foo/bar/baz"

    it "sets queryString" $ do
      queryString req `shouldBe` [("foo", Just "23"), ("bar", Just "42"), ("baz", Nothing)]

    it "sets rawQueryString" $ do
      rawQueryString req `shouldBe` "?foo=23&bar=42&baz"

    context "when path has no query string" $ do
      it "sets rawQueryString to empty string" $ do
        rawQueryString (setPath defaultRequest "/foo/bar/baz") `shouldBe` ""

  describe "srequest" $ do

    let echoApp req respond = do
          reqBody <- L8.fromStrict <$> getRequestBodyChunk req
          let reqHeaders = requestHeaders req
          respond $
            responseLBS
              status200
              reqHeaders
              reqBody

    it "returns the response body    of an echo app" $ do
      sresp <- flip runSession echoApp $
        srequest $ SRequest defaultRequest "request body"
      simpleBody sresp `shouldBe` "request body"

  describe "request" $ do

    let echoApp req respond = do
          reqBody <- L8.fromStrict <$> getRequestBodyChunk req
          let reqHeaders = requestHeaders req
          respond $
            responseLBS
              status200
              reqHeaders
              reqBody

    it "returns the status code      of an echo app on default request" $ do
      sresp <- runSession (request defaultRequest) echoApp
      simpleStatus sresp `shouldBe` status200

    it "returns the response body    of an echo app" $ do
      bodyRef <- IORef.newIORef "request body"
      let getBodyChunk = IORef.atomicModifyIORef bodyRef $ \leftover -> ("", leftover)
      sresp <- flip runSession echoApp $
        request $
          defaultRequest
            { requestBody = getBodyChunk
            }
      simpleBody sresp `shouldBe` "request body"

    it "returns the response headers of an echo app" $ do
      sresp <- flip runSession echoApp $
        request $
          defaultRequest
            { requestHeaders = [("foo", "bar")]
            }
      simpleHeaders sresp `shouldBe` [("foo", "bar")]

    let cookieApp req respond =
          case pathInfo req of
            ["set", name, val] ->
              respond $
                responseLBS
                  status200
                  [( "Set-Cookie"
                   , toByteString $ Cookie.renderSetCookie $
                      Cookie.def { Cookie.setCookieName  = TE.encodeUtf8 name
                                 , Cookie.setCookieValue = TE.encodeUtf8 val
                                 }
                   )
                  ]
                  "set_cookie_body"
            ["delete", name] ->
              respond $
                responseLBS
                  status200
                  [( "Set-Cookie"
                   , toByteString $ Cookie.renderSetCookie $
                      Cookie.def { Cookie.setCookieName  =
                                     TE.encodeUtf8 name
                                 , Cookie.setCookieExpires =
                                     Just $ UTCTime (fromGregorian 1970 1 1) 0
                                 }
                   )
                  ]
                  "set_cookie_body"
            _ ->
              respond $
                responseLBS
                  status200
                  []
                  ( L8.pack
                  $ show
                  $ map snd
                  $ filter ((=="Cookie") . fst)
                  $ requestHeaders req
                  )

    it "sends a Cookie header with correct value after receiving a Set-Cookie header" $ do
      sresp <- flip runSession cookieApp $ do
                 void $ request $
                   setPath defaultRequest "/set/cookie_name/cookie_value"
                 request $
                   setPath defaultRequest "/get"
      simpleBody sresp `shouldBe` "[\"cookie_name=cookie_value\"]"

    it "sends a Cookie header with updated value after receiving a Set-Cookie header update" $ do
      sresp <- flip runSession cookieApp $ do
                 void $ request $
                   setPath defaultRequest "/set/cookie_name/cookie_value"
                 void $ request $
                   setPath defaultRequest "/set/cookie_name/cookie_value2"
                 request $
                   setPath defaultRequest "/get"
      simpleBody sresp `shouldBe` "[\"cookie_name=cookie_value2\"]"

    it "handles multiple cookies" $ do
      sresp <- flip runSession cookieApp $ do
                 void $ request $
                   setPath defaultRequest "/set/cookie_name/cookie_value"
                 void $ request $
                   setPath defaultRequest "/set/cookie_name2/cookie_value2"
                 request $
                   setPath defaultRequest "/get"
      simpleBody sresp `shouldBe` "[\"cookie_name=cookie_value;cookie_name2=cookie_value2\"]"

    it "removes a deleted cookie" $ do
      sresp <- flip runSession cookieApp $ do
                 void $ request $
                   setPath defaultRequest "/set/cookie_name/cookie_value"
                 void $ request $
                   setPath defaultRequest "/set/cookie_name2/cookie_value2"
                 void $ request $
                   setPath defaultRequest "/delete/cookie_name2"
                 request $
                   setPath defaultRequest "/get"
      simpleBody sresp `shouldBe` "[\"cookie_name=cookie_value\"]"

    it "sends a cookie set with setClientCookie to server" $ do
      sresp <- flip runSession cookieApp $ do
                 setClientCookie
                   (Cookie.def { Cookie.setCookieName = "cookie_name"
                               , Cookie.setCookieValue = "cookie_value"
                               }
                   )
                 request $
                   setPath defaultRequest "/get"
      simpleBody sresp `shouldBe` "[\"cookie_name=cookie_value\"]"

    it "sends a cookie updated with setClientCookie to server" $ do
      sresp <- flip runSession cookieApp $ do
                 setClientCookie
                   (Cookie.def { Cookie.setCookieName = "cookie_name"
                               , Cookie.setCookieValue = "cookie_value"
                               }
                   )
                 setClientCookie
                   (Cookie.def { Cookie.setCookieName = "cookie_name"
                               , Cookie.setCookieValue = "cookie_value2"
                               }
                   )
                 request $
                   setPath defaultRequest "/get"
      simpleBody sresp `shouldBe` "[\"cookie_name=cookie_value2\"]"

    it "does not send a cookie deleted with deleteClientCookie to server" $ do
      sresp <- flip runSession cookieApp $ do
                 setClientCookie
                   (Cookie.def { Cookie.setCookieName = "cookie_name"
                               , Cookie.setCookieValue = "cookie_value"
                               }
                   )
                 deleteClientCookie "cookie_name"
                 request $
                   setPath defaultRequest "/get"
      simpleBody sresp `shouldBe` "[]"
