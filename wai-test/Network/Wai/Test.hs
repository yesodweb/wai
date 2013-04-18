{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Network.Wai.Test
    ( -- * Session
      Session
    , runSession
      -- * Requests
    , request
    , srequest
    , SRequest (..)
    , SResponse (..)
    , defaultRequest
    , setPath
    , setRawPathInfo
      -- * Assertions
    , assertStatus
    , assertContentType
    , assertBody
    , assertBodyContains
    , assertHeader
    , assertNoHeader
    ) where
import Network.Wai
import qualified Test.HUnit.Base as H
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.Conduit.Blaze (builderToByteString)
import Blaze.ByteString.Builder (flush)
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.HTTP.Types as H
import Data.CaseInsensitive (CI)
import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Monoid (mempty)
import Network.Socket.Internal (SockAddr (SockAddrInet))

type Session = ReaderT Application (StateT ClientState IO)

data ClientState = ClientState
    { _clientCookies :: Map ByteString ByteString
    }

initState :: ClientState
initState = ClientState Map.empty

runSession :: Session a -> Application -> IO a
runSession session app = evalStateT (runReaderT session app) initState

data SRequest = SRequest
    { simpleRequest :: Request
    , simpleRequestBody :: L.ByteString
    }
data SResponse = SResponse
    { simpleStatus :: H.Status
    , simpleHeaders :: H.ResponseHeaders
    , simpleBody :: L.ByteString
    }
    deriving (Show, Eq)
request :: Request -> Session SResponse
request = srequest . flip SRequest L.empty

defaultRequest :: Request
defaultRequest = Request
    { requestMethod = "GET"
    , httpVersion = H.http11
    , rawPathInfo = "/"
    , rawQueryString = ""
    , serverName = "localhost"
    , serverPort = 80
    , requestHeaders = []
    , isSecure = False
    , remoteHost = SockAddrInet 0 0
    , pathInfo = []
    , queryString = []
    , requestBody = mempty
    , vault = mempty
#if MIN_VERSION_wai(1, 4, 0)
    , requestBodyLength = KnownLength 0
#endif
    }

-- | Set whole path (request path + query string).
setPath :: Request -> S8.ByteString -> Request
setPath req path = req {
    pathInfo = segments
  , rawPathInfo = B.toByteString (H.encodePathSegments segments)
  , queryString = query
  , rawQueryString = (H.renderQuery True query)
  }
  where
    (segments, query) = H.decodePath path

setRawPathInfo :: Request -> S8.ByteString -> Request
setRawPathInfo r rawPinfo =
  let pInfo = T.split (== '/') $ TE.decodeUtf8 rawPinfo
  in  r { rawPathInfo = rawPinfo, pathInfo = pInfo }

srequest :: SRequest -> Session SResponse
srequest (SRequest req bod) = do
    app <- ask
    liftIO $ C.runResourceT $ do
        let req' = req { requestBody = CL.sourceList $ L.toChunks bod }
        res <- app req'
        sres <- runResponse res
        -- FIXME cookie processing
        return sres

runResponse :: Response -> C.ResourceT IO SResponse
runResponse res = do
    bss <- body C.$= CL.map toBuilder C.$= builderToByteString C.$$ CL.consume
    return $ SResponse s h $ L.fromChunks bss
  where
    (s, h, body) = responseSource res
    toBuilder (C.Chunk builder) = builder
    toBuilder C.Flush = flush

assertBool :: String -> Bool -> Session ()
assertBool s b = liftIO $ H.assertBool s b

assertString :: String -> Session ()
assertString s = liftIO $ H.assertString s

assertContentType :: ByteString -> SResponse -> Session ()
assertContentType ct SResponse{simpleHeaders = h} =
    case lookup "content-type" h of
        Nothing -> assertString $ concat
            [ "Expected content type "
            , show ct
            , ", but no content type provided"
            ]
        Just ct' -> assertBool (concat
            [ "Expected content type "
            , show ct
            , ", but received "
            , show ct'
            ]) (go ct == go ct')
  where
    go = S8.takeWhile (/= ';')

assertStatus :: Int -> SResponse -> Session ()
assertStatus i SResponse{simpleStatus = s} = assertBool (concat
    [ "Expected status code "
    , show i
    , ", but received "
    , show sc
    ]) $ i == sc
  where
    sc = H.statusCode s

assertBody :: L.ByteString -> SResponse -> Session ()
assertBody lbs SResponse{simpleBody = lbs'} = assertBool (concat
    [ "Expected response body "
    , show $ L8.unpack lbs
    , ", but received "
    , show $ L8.unpack lbs'
    ]) $ lbs == lbs'

assertBodyContains :: L.ByteString -> SResponse -> Session ()
assertBodyContains lbs SResponse{simpleBody = lbs'} = assertBool (concat
    [ "Expected response body to contain "
    , show $ L8.unpack lbs
    , ", but received "
    , show $ L8.unpack lbs'
    ]) $ strict lbs `S.isInfixOf` strict lbs'
  where
    strict = S.concat . L.toChunks

assertHeader :: CI ByteString -> ByteString -> SResponse -> Session ()
assertHeader header value SResponse{simpleHeaders = h} =
    case lookup header h of
        Nothing -> assertString $ concat
            [ "Expected header "
            , show header
            , " to be "
            , show value
            , ", but it was not present"
            ]
        Just value' -> assertBool (concat
            [ "Expected header "
            , show header
            , " to be "
            , show value
            , ", but received "
            , show value'
            ]) (value == value')

assertNoHeader :: CI ByteString -> SResponse -> Session ()
assertNoHeader header SResponse{simpleHeaders = h} =
    case lookup header h of
        Nothing -> return ()
        Just s -> assertString $ concat
            [ "Unexpected header "
            , show header
            , " containing "
            , show s
            ]
