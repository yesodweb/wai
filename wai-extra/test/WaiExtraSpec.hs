{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module WaiExtraSpec (spec, toRequest) where

import Codec.Compression.GZip (decompress)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.IORef as I
import Data.Maybe (fromMaybe)
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid ((<>))
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mempty, mappend)
#endif
#endif
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as T
import Network.HTTP.Types (
    Header,
    RequestHeaders,
    ResponseHeaders,
    hContentEncoding,
    hContentLength,
    hContentType,
    partialContent206,
    status200,
 )
import Network.HTTP.Types.Header (hAcceptEncoding, hVary)
import Network.Wai
import System.Directory (listDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Log.FastLogger (fromLogStr)
import Test.HUnit (Assertion, assertBool, assertEqual, (@?=))
import Test.Hspec

import Network.Wai.Header (parseQValueList)
import Network.Wai.Middleware.AcceptOverride (acceptOverride)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Gzip (
    GzipFiles (..),
    GzipSettings (..),
    def,
    defaultCheckMime,
    gzip,
 )
import Network.Wai.Middleware.Jsonp (jsonp)
import Network.Wai.Middleware.MethodOverride (methodOverride)
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.StreamFile (streamFile)
import Network.Wai.Middleware.Vhost (vhost)
import Network.Wai.Test
import Network.Wai.UrlMap (mapUrls, mount, mountRoot)

spec :: Spec
spec = do
    describe "Network.Wai.UrlMap" $ do
        mapM_ (uncurry it) casesUrlMap

    describe "Network.Wai" $ do
        {-
        , it "findBound" caseFindBound
        , it "sinkTillBound" caseSinkTillBound
        , it "killCR" caseKillCR
        , it "killCRLF" caseKillCRLF
        , it "takeLine" caseTakeLine
        -}
        it "jsonp" caseJsonp
        describe "gzip" $ do
            it "gzip" caseGzip
            it "more gzip" caseGzip2
            it "gzip not on partial content" caseGzipPartial
            it "gzip removes length header" caseGzipLength
            it "gzip not for MSIE" caseGzipMSIE
            it "gzip bypass when precompressed" caseGzipBypassPre
            it "defaultCheckMime" caseDefaultCheckMime
            it "gzip checking of files" caseGzipFiles
        it "vhost" caseVhost
        it "autohead" caseAutohead
        it "method override" caseMethodOverride
        it "method override post" caseMethodOverridePost
        it "accept override" caseAcceptOverride
        it "debug request body" caseDebugRequestBody
        it "stream file" caseStreamFile
        it "stream LBS" caseStreamLBS
        it "can modify POST params before logging" caseModifyPostParamsInLogs
        it "can filter requests in logs" caseFilterRequestsInLogs
        it "can parse Q values" caseQValues

toRequest :: S8.ByteString -> S8.ByteString -> SRequest
toRequest ctype content =
    SRequest
        defaultRequest
            { requestHeaders = [("Content-Type", ctype)]
            , requestMethod = "POST"
            , rawPathInfo = "/"
            , rawQueryString = ""
            , queryString = []
            }
        (L.fromChunks [content])

{-
caseFindBound :: Assertion
caseFindBound = do
    findBound "def" "abcdefghi" @?=
        FoundBound "abc" "ghi"
    findBound "def" "ABC" @?= NoBound
    findBound "def" "abcd" @?= PartialBound
    findBound "def" "abcdE" @?= NoBound
    findBound "def" "abcdEdef" @?=
        FoundBound "abcdE" ""

caseSinkTillBound :: Assertion
caseSinkTillBound = do
    let iter () _ = return ()
    let src = "this is some text"
        bound1 = "some"
        bound2 = "some!"
    let enum = enumList 1 [src]
    let helper _ _ = return ()
    (_, res1) <- run_ $ enum $$ sinkTillBound bound1 helper ()
    res1 @?= True
    (_, res2) <- run_ $ enum $$ sinkTillBound bound2 helper ()
    res2 @?= False

caseKillCR :: Assertion
caseKillCR = do
    "foo" @=? killCR "foo"
    "foo" @=? killCR "foo\r"
    "foo\r\n" @=? killCR "foo\r\n"
    "foo\r'" @=? killCR "foo\r'"

caseKillCRLF :: Assertion
caseKillCRLF = do
    "foo" @=? killCRLF "foo"
    "foo\r" @=? killCRLF "foo\r"
    "foo" @=? killCRLF "foo\r\n"
    "foo\r'" @=? killCRLF "foo\r'"
    "foo" @=? killCRLF "foo\n"

caseTakeLine :: Assertion
caseTakeLine = do
    helper "foo\nbar\nbaz" "foo"
    helper "foo\r\nbar\nbaz" "foo"
    helper "foo\nbar\r\nbaz" "foo"
    helper "foo\rbar\r\nbaz" "foo\rbar"
  where
    helper haystack needle = do
        x <- run_ $ enumList 1 [haystack] $$ takeLine
        Just needle @=? x
-}

jsonpApp :: Application
jsonpApp = jsonp $ \_ f ->
    f $
        responseLBS
            status200
            [("Content-Type", "application/json")]
            "{\"foo\":\"bar\"}"

caseJsonp :: Assertion
caseJsonp = withSession jsonpApp $ do
    sres1 <-
        request
            defaultRequest
                { queryString = [("callback", Just "test")]
                , requestHeaders = [("Accept", "text/javascript")]
                }
    assertContentType "text/javascript" sres1
    assertBody "test({\"foo\":\"bar\"})" sres1

    sres2 <-
        request
            defaultRequest
                { queryString = [("call_back", Just "test")]
                , requestHeaders = [("Accept", "text/javascript")]
                }
    assertContentType "application/json" sres2
    assertBody "{\"foo\":\"bar\"}" sres2

    sres3 <-
        request
            defaultRequest
                { queryString = [("callback", Just "test")]
                , requestHeaders = [("Accept", "text/html")]
                }
    assertContentType "application/json" sres3
    assertBody "{\"foo\":\"bar\"}" sres3

gzipApp :: Application
gzipApp = gzipApp' id

gzipApp' :: (Response -> Response) -> Application
gzipApp' changeRes =
    gzip def $ \_ f ->
        f . changeRes $
            responseLBS
                status200
                [("Content-Type", "text/plain")]
                "test"

gzipAppWithHeaders :: ResponseHeaders -> Application
gzipAppWithHeaders hdrs = gzipApp' $ mapResponseHeaders (hdrs ++)

gzipFileApp :: GzipSettings -> Application
gzipFileApp = flip gzipFileApp' id

gzipJSONFile, gzipNoPreCompressFile :: FilePath
gzipJSONFile = "test/json"
gzipNoPreCompressFile = "test/noprecompress"

gzipJSONBody, gzipNocompressBody :: L.ByteString
#if WINDOWS
gzipJSONBody = "{\"data\":\"this is some data\"}\r\n"
gzipNocompressBody = "noprecompress\r\n"
#else
gzipJSONBody = "{\"data\":\"this is some data\"}\n"
gzipNocompressBody = "noprecompress\n"
#endif

-- | Use 'changeRes' to make r
gzipFileApp' :: GzipSettings -> (Response -> Response) -> Application
gzipFileApp' set changeRes =
    gzip set $ \_ f ->
        f . changeRes $
            responseFile status200 [(hContentType, "application/json")] gzipJSONFile Nothing

acceptGzip :: Header
acceptGzip = (hAcceptEncoding, "gzip")

doesEncodeGzip :: RequestHeaders -> Session SResponse
doesEncodeGzip = doesEncodeGzip' "test"

doesEncodeGzipJSON :: RequestHeaders -> Session SResponse
doesEncodeGzipJSON = doesEncodeGzip' gzipJSONBody

doesEncodeGzipNoPreCompress :: RequestHeaders -> Session SResponse
doesEncodeGzipNoPreCompress = doesEncodeGzip' gzipNocompressBody

doesEncodeGzip' :: L.ByteString -> RequestHeaders -> Session SResponse
doesEncodeGzip' body hdrs = do
    sres <-
        request
            defaultRequest
                { requestHeaders = hdrs
                }
    assertHeader hContentEncoding "gzip" sres
    assertHeader hVary "Accept-Encoding" sres
    liftIO $ decompress (simpleBody sres) @?= body
    pure sres

doesNotEncodeGzip :: RequestHeaders -> Session SResponse
doesNotEncodeGzip = doesNotEncodeGzip' "test"

doesNotEncodeGzipJSON :: RequestHeaders -> Session SResponse
doesNotEncodeGzipJSON = doesNotEncodeGzip' gzipJSONBody

doesNotEncodeGzipNoPreCompress :: RequestHeaders -> Session SResponse
doesNotEncodeGzipNoPreCompress = doesNotEncodeGzip' gzipNocompressBody

doesNotEncodeGzip' :: L.ByteString -> RequestHeaders -> Session SResponse
doesNotEncodeGzip' body hdrs = do
    sres <-
        request
            defaultRequest
                { requestHeaders = hdrs
                }
    assertNoHeader hContentEncoding sres
    assertHeader hVary "Accept-Encoding" sres
    assertBody body sres
    pure sres

caseGzip :: Assertion
caseGzip = do
    withSession gzipApp $ do
        _ <- doesEncodeGzip [acceptGzip]
        _ <- doesNotEncodeGzip []
        _ <- doesEncodeGzip [(hAcceptEncoding, "compress , gzip ; q=0.8")]
        pure ()

    withSession (gzipAppWithHeaders [(hContentLength, "200")]) $ do
        sres4 <- doesNotEncodeGzip [acceptGzip]
        assertHeader hContentLength "200" sres4

caseGzipLength :: Assertion
caseGzipLength = do
    withSession (gzipAppWithHeaders [(hContentLength, "4000")]) $ do
        sres <- doesEncodeGzip [acceptGzip]
        assertNoHeader hContentLength sres

caseGzipPartial :: Assertion
caseGzipPartial =
    withSession partialApp $ do
        _ <- doesNotEncodeGzip [acceptGzip]
        pure ()
  where
    partialApp = gzipApp' $ mapResponseStatus $ const partialContent206

-- | Checking that it doesn't compress when already compressed AND
-- doesn't replace already set "Vary" header.
caseGzip2 :: Assertion
caseGzip2 =
    withSession gzipVariantApp $ do
        sres1 <-
            request
                defaultRequest
                    { requestHeaders = [(hAcceptEncoding, "compress, gzip")]
                    }
        assertHeader hContentEncoding "compress" sres1
        assertHeader hVary "Accept-Encoding, foobar" sres1
  where
    gzipVariantApp =
        gzipAppWithHeaders
            [ ("Content-Encoding", "compress")
            , ("Vary", "foobar")
            ]

-- | Testing of the GzipSettings's 'GzipFiles' setting
-- with 'ResponseFile' responses.
caseGzipFiles :: Assertion
caseGzipFiles = do
    -- Default GzipSettings ignore compressing files
    withSession (gzipFileApp def) $ do
        _ <- doesNotEncodeGzipJSON [acceptGzip]
        _ <- doesNotEncodeGzipJSON []
        pure ()

    -- Just compresses the file
    withSession (gzipFileApp def{gzipFiles = GzipCompress}) $ do
        _ <- doesEncodeGzipJSON [acceptGzip]
        _ <- doesNotEncodeGzipJSON []
        pure ()

    -- Checks for a "filename.gz" file in the same folder
    withSession (gzipFileApp def{gzipFiles = GzipPreCompressed GzipIgnore}) $ do
        sres <-
            request
                defaultRequest
                    { requestHeaders = [acceptGzip]
                    }
        assertHeader hContentEncoding "gzip" sres
        assertHeader hVary "Accept-Encoding" sres
        -- json.gz has body "test\n"
        assertBody
#if WINDOWS
            "test\r\n"
#else
            "test\n"
#endif
            sres

        doesNotEncodeGzipJSON [] >> pure ()

    -- If no "filename.gz" file is in the same folder, just ignore
    withSession (noPreCompressApp $ GzipPreCompressed GzipIgnore) $ do
        _ <- doesNotEncodeGzipNoPreCompress [acceptGzip]
        _ <- doesNotEncodeGzipNoPreCompress []
        pure ()

    -- If no "filename.gz" file is in the same folder, just compress
    withSession (noPreCompressApp $ GzipPreCompressed GzipCompress) $ do
        _ <- doesEncodeGzipNoPreCompress [acceptGzip]
        _ <- doesNotEncodeGzipNoPreCompress []
        pure ()

    -- Using a caching directory
    withSystemTempDirectory "gziptest" $ \path -> do
        let checkTempDir n s = do
                fs <- listDirectory path
                assertBool s $ length fs == n
        checkTempDir 0 "temp directory should be empty"
        -- Respond with "test/json" file
        withSession (gzipFileApp def{gzipFiles = GzipCacheFolder path}) $ do
            _ <- doesEncodeGzipJSON [acceptGzip]
            liftIO $ checkTempDir 1 "should have one file"
            _ <- doesEncodeGzipJSON [acceptGzip]
            liftIO $ checkTempDir 1 "should still have only one file"
            _ <- doesNotEncodeGzipJSON []
            liftIO $ checkTempDir 1 "should not have done anything"

        -- Respond with "test/noprecompress" file
        withSession (noPreCompressApp $ GzipCacheFolder path) $ do
            _ <- doesEncodeGzipNoPreCompress [acceptGzip]
            liftIO $ checkTempDir 2 "should now have 2 files"
            _ <- doesEncodeGzipNoPreCompress [acceptGzip]
            liftIO $ checkTempDir 2 "should still only have 2 files"
            _ <- doesNotEncodeGzipNoPreCompress []
            liftIO $ checkTempDir 2 "again should not have done anything"

        -- try "test/json" again, just to make sure it isn't a weird Session bug
        withSession (gzipFileApp def{gzipFiles = GzipCacheFolder path}) $ do
            _ <- doesEncodeGzipJSON [acceptGzip]
            liftIO $ checkTempDir 2 "just to make sure it isn't a fluke"
  where
    noPreCompressApp set =
        gzipFileApp'
            def{gzipFiles = set}
            $ const
            $ responseFile
                status200
                [(hContentType, "text/plain")]
                gzipNoPreCompressFile
                Nothing

caseDefaultCheckMime :: Assertion
caseDefaultCheckMime = do
    let go x y = (x, defaultCheckMime x) `shouldBe` (x, y)
    go "application/json" True
    go "application/javascript" True
    go "application/something" False
    go "text/something" True
    go "foo/bar" False
    go "application/json; charset=utf-8" True

caseGzipMSIE :: Assertion
caseGzipMSIE = withSession gzipApp $ do
    sres1 <-
        doesNotEncodeGzip
            [ acceptGzip
            , ("User-Agent", "Mozilla/4.0 (Windows; MSIE 6.0; Windows NT 6.0)")
            ]
    assertHeader "Vary" "Accept-Encoding" sres1

caseGzipBypassPre :: Assertion
caseGzipBypassPre =
    -- Lie a little and don't compress the body.  This way we test
    -- that the compression is skipped based on the presence of
    -- the Content-Encoding header.
    withSession (gzipAppWithHeaders [(hContentEncoding, "gzip")]) $ do
        sres1 <- request defaultRequest{requestHeaders = [acceptGzip]}
        assertHeader "Content-Encoding" "gzip" sres1
        assertHeader "Vary" "Accept-Encoding" sres1
        assertBody "test" sres1 -- the body is not actually compressed

vhostApp1, vhostApp2, vhostApp :: Application
vhostApp1 _ f = f $ responseLBS status200 [] "app1"
vhostApp2 _ f = f $ responseLBS status200 [] "app2"
vhostApp =
    vhost
        [ ((== Just "foo.com") . lookup "host" . requestHeaders, vhostApp1)
        ]
        vhostApp2

caseVhost :: Assertion
caseVhost = withSession vhostApp $ do
    sres1 <-
        request
            defaultRequest
                { requestHeaders = [("Host", "foo.com")]
                }
    assertBody "app1" sres1

    sres2 <-
        request
            defaultRequest
                { requestHeaders = [("Host", "bar.com")]
                }
    assertBody "app2" sres2

autoheadApp :: Application
autoheadApp = autohead $ \_ f ->
    f $
        responseLBS
            status200
            [("Foo", "Bar")]
            "body"

caseAutohead :: Assertion
caseAutohead = withSession autoheadApp $ do
    sres1 <-
        request
            defaultRequest
                { requestMethod = "GET"
                }
    assertHeader "Foo" "Bar" sres1
    assertBody "body" sres1

    sres2 <-
        request
            defaultRequest
                { requestMethod = "HEAD"
                }
    assertHeader "Foo" "Bar" sres2
    assertBody "" sres2

moApp :: Application
moApp = methodOverride $ \req f ->
    f $
        responseLBS
            status200
            [("Method", requestMethod req)]
            ""

caseMethodOverride :: Assertion
caseMethodOverride = withSession moApp $ do
    sres1 <-
        request
            defaultRequest
                { requestMethod = "GET"
                , queryString = []
                }
    assertHeader "Method" "GET" sres1

    sres2 <-
        request
            defaultRequest
                { requestMethod = "POST"
                , queryString = []
                }
    assertHeader "Method" "POST" sres2

    sres3 <-
        request
            defaultRequest
                { requestMethod = "POST"
                , queryString = [("_method", Just "PUT")]
                }
    assertHeader "Method" "PUT" sres3

mopApp :: Application
mopApp = methodOverridePost $ \req f -> f $ responseLBS status200 [("Method", requestMethod req)] ""

caseMethodOverridePost :: Assertion
caseMethodOverridePost = withSession mopApp $ do
    -- Get Request are unmodified
    sres1 <-
        let r = toRequest "application/x-www-form-urlencoded" "_method=PUT&foo=bar&baz=bin"
            s = simpleRequest r
            m = s{requestMethod = "GET"}
            b = r{simpleRequest = m}
         in srequest b
    assertHeader "Method" "GET" sres1

    -- Post requests are modified if _method comes first
    sres2 <-
        srequest $
            toRequest "application/x-www-form-urlencoded" "_method=PUT&foo=bar&baz=bin"
    assertHeader "Method" "PUT" sres2

    -- Post requests are unmodified if _method doesn't come first
    sres3 <-
        srequest $
            toRequest "application/x-www-form-urlencoded" "foo=bar&_method=PUT&baz=bin"
    assertHeader "Method" "POST" sres3

    -- Post requests are unmodified if Content-Type header isn't set to "application/x-www-form-urlencoded"
    sres4 <-
        srequest $ toRequest "text/html; charset=utf-8" "foo=bar&_method=PUT&baz=bin"
    assertHeader "Method" "POST" sres4

aoApp :: Application
aoApp = acceptOverride $ \req f ->
    f $
        responseLBS
            status200
            [("Accept", fromMaybe "" $ lookup "Accept" $ requestHeaders req)]
            ""

caseAcceptOverride :: Assertion
caseAcceptOverride = withSession aoApp $ do
    sres1 <-
        request
            defaultRequest
                { queryString = []
                , requestHeaders = [("Accept", "foo")]
                }
    assertHeader "Accept" "foo" sres1

    sres2 <-
        request
            defaultRequest
                { queryString = []
                , requestHeaders = [("Accept", "bar")]
                }
    assertHeader "Accept" "bar" sres2

    sres3 <-
        request
            defaultRequest
                { queryString = [("_accept", Just "baz")]
                , requestHeaders = [("Accept", "bar")]
                }
    assertHeader "Accept" "baz" sres3

caseDebugRequestBody :: Assertion
caseDebugRequestBody = do
    withSession (debugApp postOutput) $ do
        let req = toRequest "application/x-www-form-urlencoded" "foo=bar&baz=bin"
        res <- srequest req
        assertStatus 200 res

    let qs = "?foo=bar&baz=bin"
    withSession (debugApp $ getOutput params) $ do
        assertStatus 200
            =<< request
                defaultRequest
                    { requestMethod = "GET"
                    , queryString = map (\(k, v) -> (k, Just v)) params
                    , rawQueryString = qs
                    , requestHeaders = []
                    , rawPathInfo = "/location"
                    }
  where
    params = [("foo", "bar"), ("baz", "bin")]
    -- the time cannot be known, so match around it
    postOutput = (T.pack $ "POST /\n  Params: " ++ show params, "s\n")
    getOutput params' =
        ( "GET /location\n  Params: "
            <> T.pack (show params')
            <> "\n  Accept: \n  Status: 200 OK 0"
        , "s\n"
        )

    debugApp (beginning, ending) req send = do
        iactual <- I.newIORef mempty
        middleware <-
            mkRequestLogger
                def
                    { destination = Callback $ \strs -> I.modifyIORef iactual (`mappend` strs)
                    , outputFormat = Detailed False
                    }
        res <- middleware (\_req f -> f $ responseLBS status200 [] "") req send
        actual <- logToBs <$> I.readIORef iactual
        actual `shouldSatisfy` S.isPrefixOf begin
        actual `shouldSatisfy` S.isSuffixOf end

        return res
      where
        begin = TE.encodeUtf8 $ T.toStrict beginning
        end = TE.encodeUtf8 $ T.toStrict ending

        logToBs = fromLogStr

{-debugApp = debug $ \req -> do-}
{-return $ responseLBS status200 [ ] ""-}

urlMapTestApp :: Application
urlMapTestApp =
    mapUrls $
        mount "bugs" bugsApp
            <|> mount "helpdesk" helpdeskApp
            <|> mount
                "api"
                ( mount "v1" apiV1
                    <|> mount "v2" apiV2
                )
            <|> mountRoot mainApp
  where
    trivialApp :: S.ByteString -> Application
    trivialApp name req f =
        f $
            responseLBS
                status200
                [ ("content-type", "text/plain")
                , ("X-pathInfo", S8.pack . show . pathInfo $ req)
                , ("X-rawPathInfo", rawPathInfo req)
                , ("X-appName", name)
                ]
                ""

    bugsApp = trivialApp "bugs"
    helpdeskApp = trivialApp "helpdesk"
    apiV1 = trivialApp "apiv1"
    apiV2 = trivialApp "apiv2"
    mainApp = trivialApp "main"

casesUrlMap :: [(String, Assertion)]
casesUrlMap = [pair1, pair2, pair3, pair4]
  where
    makePair name session = (name, runSession session urlMapTestApp)
    get reqPath = request $ setPath defaultRequest reqPath
    s = S8.pack . show :: [TS.Text] -> S.ByteString

    pair1 = makePair "should mount root" $ do
        res1 <- get "/"
        assertStatus 200 res1
        assertHeader "X-rawPathInfo" "/" res1
        assertHeader "X-pathInfo" (s []) res1
        assertHeader "X-appName" "main" res1

    pair2 = makePair "should mount apps" $ do
        res2 <- get "/bugs"
        assertStatus 200 res2
        assertHeader "X-rawPathInfo" "/" res2
        assertHeader "X-pathInfo" (s []) res2
        assertHeader "X-appName" "bugs" res2

    pair3 = makePair "should preserve extra path info" $ do
        res3 <- get "/helpdesk/issues/11"
        assertStatus 200 res3
        assertHeader "X-rawPathInfo" "/issues/11" res3
        assertHeader "X-pathInfo" (s ["issues", "11"]) res3

    pair4 = makePair "should 404 if none match" $ do
        res4 <- get "/api/v3"
        assertStatus 404 res4

testFile :: FilePath
testFile = "test/WaiExtraSpec.hs"

streamFileApp :: Application
streamFileApp = streamFile $ \_ f -> f $ responseFile status200 [] testFile Nothing

caseStreamFile :: Assertion
caseStreamFile = withSession streamFileApp $ do
    sres <- request defaultRequest
    assertStatus 200 sres
    assertBodyContains "caseStreamFile" sres
    assertNoHeader "Transfer-Encoding" sres

streamLBSApp :: Application
streamLBSApp = streamFile $ \_ f ->
    f $
        responseLBS
            status200
            [("Content-Type", "text/plain")]
            "test"

caseStreamLBS :: Assertion
caseStreamLBS = withSession streamLBSApp $ do
    sres <- request defaultRequest
    assertStatus 200 sres
    assertBody "test" sres

caseModifyPostParamsInLogs :: Assertion
caseModifyPostParamsInLogs = do
    let formatUnredacted = DetailedWithSettings $ DetailedSettings False Nothing Nothing False
        outputUnredacted = [("username", "some_user"), ("password", "dont_show_me")]
        formatRedacted =
            DetailedWithSettings $ DetailedSettings False (Just hidePasswords) Nothing False
        hidePasswords p@(k, _) = Just $ if k == "password" then (k, "***REDACTED***") else p
        outputRedacted = [("username", "some_user"), ("password", "***REDACTED***")]

    testLogs formatUnredacted outputUnredacted
    testLogs formatRedacted outputRedacted
  where
    testLogs :: OutputFormat -> [(String, String)] -> Assertion
    testLogs format output = withSession (debugApp format output) $ do
        let req =
                toRequest
                    "application/x-www-form-urlencoded"
                    "username=some_user&password=dont_show_me"
        res <- srequest req
        assertStatus 200 res

    postOutputStart params = TE.encodeUtf8 $ T.toStrict $ "POST /\n  Params: " <> (T.pack . show $ params)
    postOutputEnd = TE.encodeUtf8 $ T.toStrict "s\n"

    debugApp format output req send = do
        iactual <- I.newIORef mempty
        middleware <-
            mkRequestLogger
                def
                    { destination = Callback $ \strs -> I.modifyIORef iactual (`mappend` strs)
                    , outputFormat = format
                    }
        res <- middleware (\_req f -> f $ responseLBS status200 [] "") req send
        actual <- fromLogStr <$> I.readIORef iactual
        actual `shouldSatisfy` S.isPrefixOf (postOutputStart output)
        actual `shouldSatisfy` S.isSuffixOf postOutputEnd

        return res

caseFilterRequestsInLogs :: Assertion
caseFilterRequestsInLogs = do
    let formatUnfiltered = DetailedWithSettings $ DetailedSettings False Nothing Nothing False
        formatFiltered =
            DetailedWithSettings $
                DetailedSettings False Nothing (Just hideHealthCheck) False
        pathHidden = "/health-check"
        pathNotHidden = "/foobar"

    -- filter is off
    testLogs formatUnfiltered pathNotHidden True
    testLogs formatUnfiltered pathHidden True
    -- filter is on, path does not match
    testLogs formatFiltered pathNotHidden True
    -- filter is on, path matches
    testLogs formatFiltered pathHidden False
  where
    testLogs :: OutputFormat -> S8.ByteString -> Bool -> Assertion
    testLogs format rpath haslogs = withSession (debugApp format rpath haslogs) $ do
        let req = flip SRequest "" $ setPath defaultRequest rpath
        res <- srequest req
        assertStatus 200 res

    hideHealthCheck req _res = pathInfo req /= ["health-check"]

    debugApp format rpath haslogs req send = do
        iactual <- I.newIORef mempty
        middleware <-
            mkRequestLogger
                def
                    { destination = Callback $ \strs -> I.modifyIORef iactual (`mappend` strs)
                    , outputFormat = format
                    }
        res <- middleware (\_req f -> f $ responseLBS status200 [] "") req send
        actual <- fromLogStr <$> I.readIORef iactual
        if haslogs
            then do
                actual `shouldSatisfy` S.isPrefixOf ("GET " <> rpath <> "\n")
                actual `shouldSatisfy` S.isSuffixOf "s\n"
            else actual `shouldBe` ""

        return res

-- | Unit test to make sure 'parseQValueList' works correctly
caseQValues :: Assertion
caseQValues = do
    let encodings =
            mconcat
                -- This has weird white space on purpose, because this
                -- should be acceptable according to RFC 7231
                [ "deflate,   compress; q=0.813  ,gzip ;  q=0.9, * ;q=0, "
                , "notq;charset=bar, "
                , "noq;q=,   "
                , "toolong;q=0.0023, "
                , "toohigh ;q=1.1"
                ]
        qList = parseQValueList encodings
        expected =
            [ ("deflate", Just 1000)
            , ("compress", Just 813)
            , ("gzip", Just 900)
            , ("*", Just 0)
            , ("notq", Nothing)
            , ("noq", Nothing)
            , ("toolong", Nothing)
            , ("toohigh", Nothing)
            ]
    assertEqual "invalid Q values" expected qList
