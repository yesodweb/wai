{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.CombineHeadersSpec
    ( main
    , spec
    ) where

import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header
import Network.Wai
import Test.Hspec

import Network.Wai.Middleware.CombineHeaders (CombineSettings, combineHeaders, defaultCombineSettings, setRequestHeaders, setResponseHeaders)
import Network.Wai.Test (SResponse (simpleHeaders), request, runSession)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    let test name settings reqHeaders expectedReqHeaders resHeaders expectedResHeaders = it name $ do
            (reqHdrs, resHdrs) <- runApp settings reqHeaders resHeaders
            reqHdrs `shouldBe` expectedReqHeaders
            resHdrs `shouldBe` expectedResHeaders
        testReqHdrs name a b =
            test name defaultCombineSettings a b [] []
        testResHdrs name a b =
            test name (setRequestHeaders False $ setResponseHeaders True defaultCombineSettings) [] [] a b

    -- Request Headers
    testReqHdrs
        "should reorder alphabetically (request)"
        [host      , userAgent, acceptHtml]
        [acceptHtml, host     , userAgent ]
    -- Response Headers
    testResHdrs
        "should reorder alphabetically (response)"
        [expires        , location, contentTypeHtml]
        [contentTypeHtml, expires , location       ]

    -- Request Headers
    testReqHdrs
        "combines Accept (in order)"
        [userAgent, acceptHtml, host, acceptJSON]
        [acceptHtml `combineHdrs` acceptJSON, host, userAgent]
    -- Response Headers
    testResHdrs
        -- Using the default header map, Cache-Control is a "combineable" header, "Set-Cookie" is not
        "combines Cache-Control (in order) and keeps Set-Cookie (in order)"
        [ cacheControlPublic, setCookie "2", date, cacheControlMax, setCookie "1"]
        [ cacheControlPublic `combineHdrs` cacheControlMax, date, setCookie "2", setCookie "1"]

    -- Request Headers
    testReqHdrs
        "KeepOnly works as expected (present | request)"
        -- "Alt-Svc" has (KeepOnly "clear")
        [ date, altSvc "wrong", altSvc "clear", altSvc "wrong again", host ]
        [ altSvc "clear", date, host ]
    testReqHdrs
        "KeepOnly works as expected ( absent | request)"
        -- "Alt-Svc" has (KeepOnly "clear"), but will combine when there's no "clear" (AND keeps order)
        [ date, altSvc "wrong", altSvc "not clear", altSvc "wrong again", host ]
        [ altSvc "wrong, not clear, wrong again", date, host ]

    -- Response Headers
    testResHdrs
        "KeepOnly works as expected (present | response)"
        -- "If-None-Match" has (KeepOnly "*")
        [ date, ifNoneMatch "wrong", ifNoneMatch "*", ifNoneMatch "wrong again", host ]
        [ date, host, ifNoneMatch "*" ]
    testResHdrs
        "KeepOnly works as expected ( absent | response)"
        -- "If-None-Match" has (KeepOnly "*"), but will combine when there's no "*" (AND keeps order)
        [ date, ifNoneMatch "wrong", ifNoneMatch "not *", ifNoneMatch "wrong again", host ]
        [ date, host, ifNoneMatch "wrong, not *, wrong again" ]

    -- Request Headers
    testReqHdrs
        "Technically acceptable headers get combined correctly (request)"
        [ ifNoneMatch "correct, ", ifNoneMatch "something else \t", ifNoneMatch "and more , "]
        [ ifNoneMatch "correct, something else, and more" ]
    -- Response Headers
    testResHdrs
        "Technically acceptable headers get combined correctly (response)"
        [ altSvc "correct\t, ", altSvc "something else", altSvc "and more, , "]
        [ altSvc "correct, something else, and more" ]

combineHdrs :: Header -> Header -> Header
combineHdrs (hname, h1) (_, h2) = (hname, h1 <> ", " <> h2)

acceptHtml,
    acceptJSON,
    cacheControlMax,
    cacheControlPublic,
    contentTypeHtml,
    date,
    expires,
    host,
    location,
    userAgent :: Header

acceptHtml = (hAccept, "text/html")
acceptJSON = (hAccept, "application/json")
altSvc :: ByteString -> Header
altSvc x = ("Alt-Svc", x)
cacheControlPublic = (hCacheControl, "public")
cacheControlMax = (hCacheControl, "public")
contentTypeHtml = (hContentType, "text/html")
date = (hDate, "Mon, 19 Aug 2022 18:18:31 GMT")
expires = (hExpires, "Mon, 19 Sep 2022 18:18:31 GMT")
host = (hHost, "google.com")
ifNoneMatch :: ByteString -> Header
ifNoneMatch x = (hIfNoneMatch, x)
location = (hLocation, "http://www.google.com/")
setCookie :: ByteString -> Header
setCookie val = (hSetCookie, val)
userAgent = (hUserAgent, "curl/7.68.0")

runApp :: CombineSettings -> RequestHeaders -> ResponseHeaders -> IO (RequestHeaders, ResponseHeaders)
runApp settings reqHeaders resHeaders = do
    reqHdrs <- newIORef $ error "IORef not set"
    sResponse <- runSession
        session
        $ combineHeaders settings $ app reqHdrs
    finalReqHeaders <- readIORef reqHdrs
    pure (finalReqHeaders, simpleHeaders sResponse)
  where
    session =
        request
            defaultRequest { requestHeaders = reqHeaders }
    app hdrRef req respond = do
        writeIORef hdrRef $ requestHeaders req
        respond $ responseLBS status200 resHeaders ""
