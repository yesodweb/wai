{-# LANGUAGE OverloadedStrings #-}
module WaiExtraSpec (spec, toRequest) where

import Test.Hspec
import Test.HUnit hiding (Test)
import Data.Monoid (mappend, mempty, (<>))

import Network.Wai
import Network.Wai.Test
import Network.Wai.UrlMap
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import Control.Applicative

import Network.Wai.Middleware.Jsonp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Vhost
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Middleware.MethodOverridePost
import Network.Wai.Middleware.AcceptOverride
import Network.Wai.Middleware.RequestLogger
import Codec.Compression.GZip (decompress)
import Network.Wai.Middleware.StreamFile

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (status200)
import System.Log.FastLogger

import qualified Data.IORef as I

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
    it "gzip" caseGzip
    it "gzip not for MSIE" caseGzipMSIE
    it "gzip bypass when precompressed" caseGzipBypassPre
    it "defaultCheckMime" caseDefaultCheckMime
    it "vhost" caseVhost
    it "autohead" caseAutohead
    it "method override" caseMethodOverride
    it "method override post" caseMethodOverridePost
    it "accept override" caseAcceptOverride
    it "debug request body" caseDebugRequestBody
    it "stream file" caseStreamFile
    it "stream LBS" caseStreamLBS

toRequest :: S8.ByteString -> S8.ByteString -> SRequest
toRequest ctype content = SRequest defaultRequest
    { requestHeaders = [("Content-Type", ctype)]
    , requestMethod = "POST"
    , rawPathInfo = "/"
    , rawQueryString = ""
    , queryString = []
    } (L.fromChunks [content])

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
jsonpApp = jsonp $ \_ f -> f $ responseLBS
    status200
    [("Content-Type", "application/json")]
    "{\"foo\":\"bar\"}"

caseJsonp :: Assertion
caseJsonp = flip runSession jsonpApp $ do
    sres1 <- request defaultRequest
                { queryString = [("callback", Just "test")]
                , requestHeaders = [("Accept", "text/javascript")]
                }
    assertContentType "text/javascript" sres1
    assertBody "test({\"foo\":\"bar\"})" sres1

    sres2 <- request defaultRequest
                { queryString = [("call_back", Just "test")]
                , requestHeaders = [("Accept", "text/javascript")]
                }
    assertContentType "application/json" sres2
    assertBody "{\"foo\":\"bar\"}" sres2

    sres3 <- request defaultRequest
                { queryString = [("callback", Just "test")]
                , requestHeaders = [("Accept", "text/html")]
                }
    assertContentType "application/json" sres3
    assertBody "{\"foo\":\"bar\"}" sres3

gzipApp :: Application
gzipApp = gzip def $ \_ f -> f $ responseLBS status200
    [("Content-Type", "text/plain")]
    "test"

-- Lie a little and don't compress the body.  This way we test
-- that the compression is skipped based on the presence of
-- the Content-Encoding header.
gzipPrecompressedApp :: Application
gzipPrecompressedApp = gzip def $ \_ f -> f $ responseLBS status200
    [("Content-Type", "text/plain"), ("Content-Encoding", "gzip")]
    "test"

caseGzip :: Assertion
caseGzip = flip runSession gzipApp $ do
    sres1 <- request defaultRequest
                { requestHeaders = [("Accept-Encoding", "gzip")]
                }
    assertHeader "Content-Encoding" "gzip" sres1
    liftIO $ decompress (simpleBody sres1) @?= "test"

    sres2 <- request defaultRequest
                { requestHeaders = []
                }
    assertNoHeader "Content-Encoding" sres2
    assertBody "test" sres2

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
caseGzipMSIE = flip runSession gzipApp $ do
    sres1 <- request defaultRequest
                { requestHeaders =
                    [ ("Accept-Encoding", "gzip")
                    , ("User-Agent", "Mozilla/4.0 (Windows; MSIE 6.0; Windows NT 6.0)")
                    ]
                }
    assertNoHeader "Content-Encoding" sres1
    liftIO $ simpleBody sres1 @?= "test"

caseGzipBypassPre :: Assertion
caseGzipBypassPre = flip runSession gzipPrecompressedApp $ do
    sres1 <- request defaultRequest
                { requestHeaders = [("Accept-Encoding", "gzip")]
                }
    assertHeader "Content-Encoding" "gzip" sres1
    assertBody "test" sres1 -- the body is not actually compressed

vhostApp1, vhostApp2, vhostApp :: Application
vhostApp1 _ f = f $ responseLBS status200 [] "app1"
vhostApp2 _ f = f $ responseLBS status200 [] "app2"
vhostApp = vhost
    [ ((== Just "foo.com") . lookup "host" . requestHeaders, vhostApp1)
    ]
    vhostApp2

caseVhost :: Assertion
caseVhost = flip runSession vhostApp $ do
    sres1 <- request defaultRequest
                { requestHeaders = [("Host", "foo.com")]
                }
    assertBody "app1" sres1

    sres2 <- request defaultRequest
                { requestHeaders = [("Host", "bar.com")]
                }
    assertBody "app2" sres2

autoheadApp :: Application
autoheadApp = autohead $ \_ f -> f $ responseLBS status200
    [("Foo", "Bar")] "body"

caseAutohead :: Assertion
caseAutohead = flip runSession autoheadApp $ do
    sres1 <- request defaultRequest
                { requestMethod = "GET"
                }
    assertHeader "Foo" "Bar" sres1
    assertBody "body" sres1

    sres2 <- request defaultRequest
                { requestMethod = "HEAD"
                }
    assertHeader "Foo" "Bar" sres2
    assertBody "" sres2

moApp :: Application
moApp = methodOverride $ \req f -> f $ responseLBS status200
    [("Method", requestMethod req)] ""

caseMethodOverride :: Assertion
caseMethodOverride = flip runSession moApp $ do
    sres1 <- request defaultRequest
                { requestMethod = "GET"
                , queryString = []
                }
    assertHeader "Method" "GET" sres1

    sres2 <- request defaultRequest
                { requestMethod = "POST"
                , queryString = []
                }
    assertHeader "Method" "POST" sres2

    sres3 <- request defaultRequest
                { requestMethod = "POST"
                , queryString = [("_method", Just "PUT")]
                }
    assertHeader "Method" "PUT" sres3

mopApp :: Application
mopApp = methodOverridePost $ \req f -> f $ responseLBS status200 [("Method", requestMethod req)] ""

caseMethodOverridePost :: Assertion
caseMethodOverridePost = flip runSession mopApp $ do

    -- Get Request are unmodified
    sres1 <- let r = toRequest "application/x-www-form-urlencoded" "_method=PUT&foo=bar&baz=bin"
                 s = simpleRequest r
                 m = s { requestMethod = "GET" }
                 b = r { simpleRequest = m }
             in srequest b
    assertHeader "Method" "GET" sres1

    -- Post requests are modified if _method comes first
    sres2 <- srequest $ toRequest "application/x-www-form-urlencoded" "_method=PUT&foo=bar&baz=bin"
    assertHeader "Method" "PUT" sres2

    -- Post requests are unmodified if _method doesn't come first
    sres3 <- srequest $ toRequest "application/x-www-form-urlencoded" "foo=bar&_method=PUT&baz=bin"
    assertHeader "Method" "POST" sres3

    -- Post requests are unmodified if Content-Type header isn't set to "application/x-www-form-urlencoded"
    sres4 <- srequest $ toRequest "text/html; charset=utf-8" "foo=bar&_method=PUT&baz=bin"
    assertHeader "Method" "POST" sres4

aoApp :: Application
aoApp = acceptOverride $ \req f -> f $ responseLBS status200
    [("Accept", fromMaybe "" $ lookup "Accept" $ requestHeaders req)] ""

caseAcceptOverride :: Assertion
caseAcceptOverride = flip runSession aoApp $ do
    sres1 <- request defaultRequest
                { queryString = []
                , requestHeaders = [("Accept", "foo")]
                }
    assertHeader "Accept" "foo" sres1

    sres2 <- request defaultRequest
                { queryString = []
                , requestHeaders = [("Accept", "bar")]
                }
    assertHeader "Accept" "bar" sres2

    sres3 <- request defaultRequest
                { queryString = [("_accept", Just "baz")]
                , requestHeaders = [("Accept", "bar")]
                }
    assertHeader "Accept" "baz" sres3

caseDebugRequestBody :: Assertion
caseDebugRequestBody = do
    flip runSession (debugApp postOutput) $ do
        let req = toRequest "application/x-www-form-urlencoded" "foo=bar&baz=bin"
        res <- srequest req
        assertStatus 200 res

    let qs = "?foo=bar&baz=bin"
    flip runSession (debugApp $ getOutput params) $ do
        assertStatus 200 =<< request defaultRequest
                { requestMethod = "GET"
                , queryString = map (\(k,v) -> (k, Just v)) params
                , rawQueryString = qs
                , requestHeaders = []
                , rawPathInfo = "/location"
                }
  where
    params = [("foo", "bar"), ("baz", "bin")]
    -- FIXME change back once we include post parameter output in logging
    -- postOutput = T.pack $ "POST \nAccept: \n  Params: " ++ (show params)
    -- the time cannot be known, so match around it
    postOutput = ("POST /\n  Accept: \n  Status: 200 OK 0", "s\n")
    getOutput params' = ("GET /location\n  Params: " <> T.pack (show params') <> "\n  Accept: \n  Status: 200 OK 0", "s\n")

    debugApp (beginning, ending) req send = do
        iactual <- I.newIORef mempty
        middleware <- mkRequestLogger def
            { destination = Callback $ \strs -> I.modifyIORef iactual $ (`mappend` strs)
            , outputFormat = Detailed False
            }
        res <- middleware (\_req f -> f $ responseLBS status200 [ ] "") req send
        actual <- logToBs <$> I.readIORef iactual
        actual `shouldSatisfy` S.isPrefixOf begin
        actual `shouldSatisfy` S.isSuffixOf end

        return res
      where
        begin = TE.encodeUtf8 $ T.toStrict beginning
        end   = TE.encodeUtf8 $ T.toStrict ending

        logToBs = fromLogStr

    {-debugApp = debug $ \req -> do-}
        {-return $ responseLBS status200 [ ] ""-}

urlMapTestApp :: Application
urlMapTestApp = mapUrls $
        mount "bugs"     bugsApp
    <|> mount "helpdesk" helpdeskApp
    <|> mount "api"
            (   mount "v1" apiV1
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
        , ("X-pathInfo",    S8.pack . show . pathInfo $ req)
        , ("X-rawPathInfo", rawPathInfo req)
        , ("X-appName",     name)
        ]
        ""

  bugsApp     = trivialApp "bugs"
  helpdeskApp = trivialApp "helpdesk"
  apiV1       = trivialApp "apiv1"
  apiV2       = trivialApp "apiv2"
  mainApp     = trivialApp "main"

casesUrlMap :: [(String, Assertion)]
casesUrlMap = [pair1, pair2, pair3, pair4]
  where
  makePair name session = (name, runSession session urlMapTestApp)
  get reqPath = request $ setPath defaultRequest reqPath
  s = S8.pack . show :: [TS.Text] -> S.ByteString

  pair1 = makePair "should mount root" $ do
    res1 <- get "/"
    assertStatus 200 res1
    assertHeader "X-rawPathInfo" "/"    res1
    assertHeader "X-pathInfo"    (s []) res1
    assertHeader "X-appName"     "main" res1

  pair2 = makePair "should mount apps" $ do
    res2 <- get "/bugs"
    assertStatus 200 res2
    assertHeader "X-rawPathInfo" "/"    res2
    assertHeader "X-pathInfo"    (s []) res2
    assertHeader "X-appName"     "bugs" res2

  pair3 = makePair "should preserve extra path info" $ do
    res3 <- get "/helpdesk/issues/11"
    assertStatus 200 res3
    assertHeader "X-rawPathInfo" "/issues/11"         res3
    assertHeader "X-pathInfo"    (s ["issues", "11"]) res3

  pair4 = makePair "should 404 if none match" $ do
    res4 <- get "/api/v3"
    assertStatus 404 res4

testFile :: FilePath
testFile = "test/WaiExtraSpec.hs"

streamFileApp :: Application
streamFileApp = streamFile $ \_ f -> f $ responseFile status200 [] testFile Nothing

caseStreamFile :: Assertion
caseStreamFile = flip runSession streamFileApp $ do
    sres <- request defaultRequest
    assertStatus 200 sres
    assertBodyContains "caseStreamFile" sres
    assertNoHeader "Transfer-Encoding" sres

streamLBSApp :: Application
streamLBSApp = streamFile $ \_ f -> f $ responseLBS status200
    [("Content-Type", "text/plain")]
    "test"

caseStreamLBS :: Assertion
caseStreamLBS = flip runSession streamLBSApp $ do
    sres <- request defaultRequest
    assertStatus 200 sres
    assertBody "test" sres
