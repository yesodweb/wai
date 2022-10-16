{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Network.Wai.Test
    ( -- * Session
      Session
    , runSession
    , withSession
      -- * Client Cookies
    , ClientCookies
    , getClientCookies
    , modifyClientCookies
    , setClientCookie
    , deleteClientCookie
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
    , assertClientCookieExists
    , assertNoClientCookieExists
    , assertClientCookieValue
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
import Data.Monoid (mempty, mappend)
#endif

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Control.Monad.Trans.State as ST
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.CallStack (HasCallStack)
import Data.CaseInsensitive (CI)
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import qualified Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import Network.Wai.Test.Internal
import qualified Test.HUnit as HUnit
import qualified Web.Cookie as Cookie

-- |
--
-- Since 3.0.6
getClientCookies :: Session ClientCookies
getClientCookies = clientCookies <$> lift ST.get

-- |
--
-- Since 3.0.6
modifyClientCookies :: (ClientCookies -> ClientCookies) -> Session ()
modifyClientCookies f =
  lift (ST.modify (\cs -> cs { clientCookies = f $ clientCookies cs }))

-- |
--
-- Since 3.0.6
setClientCookie :: Cookie.SetCookie -> Session ()
setClientCookie c =
  modifyClientCookies $
    Map.insert (Cookie.setCookieName c) c

-- |
--
-- Since 3.0.6
deleteClientCookie :: ByteString -> Session ()
deleteClientCookie =
  modifyClientCookies . Map.delete

-- | See also: 'runSessionWith'.
runSession :: Session a -> Application -> IO a
runSession session app = ST.evalStateT (runReaderT session app) initState

-- | Synonym for 'flip runSession'
withSession :: Application -> Session a -> IO a
withSession = flip runSession

data SRequest = SRequest
    { simpleRequest :: Request
    , simpleRequestBody :: L.ByteString
    -- ^ Request body that will override the one set in 'simpleRequest'.
    --
    -- This is usually simpler than setting the body as a stateful IO-action
    -- in 'simpleRequest'.
    }
data SResponse = SResponse
    { simpleStatus :: H.Status
    , simpleHeaders :: H.ResponseHeaders
    , simpleBody :: L.ByteString
    }
    deriving (Show, Eq)

request :: Request -> Session SResponse
request req = do
    app <- ask
    req' <- addCookiesToRequest req
    response <- liftIO $ do
        ref <- newIORef $ error "runResponse gave no result"
        ResponseReceived <- app req' (runResponse ref)
        readIORef ref
    extractSetCookieFromSResponse response

-- | Set whole path (request path + query string).
setPath :: Request -> S8.ByteString -> Request
setPath req path = req {
    pathInfo = segments
  , rawPathInfo = L8.toStrict . toLazyByteString $ H.encodePathSegments segments
  , queryString = query
  , rawQueryString = H.renderQuery True query
  }
  where
    (segments, query) = H.decodePath path

setRawPathInfo :: Request -> S8.ByteString -> Request
setRawPathInfo r rawPinfo =
    let pInfo = dropFrontSlash $ T.split (== '/') $ TE.decodeUtf8 rawPinfo
    in  r { rawPathInfo = rawPinfo, pathInfo = pInfo }
  where
    dropFrontSlash ("":"":[]) = [] -- homepage, a single slash
    dropFrontSlash ("":path) = path
    dropFrontSlash path = path

addCookiesToRequest :: Request -> Session Request
addCookiesToRequest req = do
  oldClientCookies <- getClientCookies
  let requestPath = "/" `T.append` T.intercalate "/" (pathInfo req)
  currentUTCTime <- liftIO getCurrentTime
  let cookiesForRequest =
        Map.filter
          (\c -> checkCookieTime currentUTCTime c
              && checkCookiePath requestPath c)
          oldClientCookies
  let cookiePairs = [ (Cookie.setCookieName c, Cookie.setCookieValue c)
                    | c <- map snd $ Map.toList cookiesForRequest
                    ]
  let cookieValue = L8.toStrict . toLazyByteString $ Cookie.renderCookies cookiePairs
      addCookieHeader rest
        | null cookiePairs = rest
        | otherwise = ("Cookie", cookieValue) : rest
  return $ req { requestHeaders = addCookieHeader $ requestHeaders req }
    where checkCookieTime t c =
            case Cookie.setCookieExpires c of
              Nothing -> True
              Just t' -> t < t'
          checkCookiePath p c =
            case Cookie.setCookiePath c of
              Nothing -> True
              Just p' -> p' `S8.isPrefixOf` TE.encodeUtf8 p

extractSetCookieFromSResponse :: SResponse -> Session SResponse
extractSetCookieFromSResponse response = do
  let setCookieHeaders =
        filter (("Set-Cookie"==) . fst) $ simpleHeaders response
  let newClientCookies = map (Cookie.parseSetCookie . snd) setCookieHeaders
  modifyClientCookies
    (Map.union
       (Map.fromList [(Cookie.setCookieName c, c) | c <- newClientCookies ]))
  return response

-- | Similar to 'request', but allows setting the request body as a plain
-- 'L.ByteString'.
srequest :: SRequest -> Session SResponse
srequest (SRequest req bod) = do
    refChunks <- liftIO $ newIORef $ L.toChunks bod
    request $
      req
        { requestBody = atomicModifyIORef refChunks $ \bss ->
            case bss of
                [] -> ([], S.empty)
                x:y -> (y, x)
        }

runResponse :: IORef SResponse -> Response -> IO ResponseReceived
runResponse ref res = do
    refBuilder <- newIORef mempty
    let add y = atomicModifyIORef refBuilder $ \x -> (x `mappend` y, ())
    withBody $ \body -> body add (return ())
    builder <- readIORef refBuilder
    let lbs = toLazyByteString builder
        len = L.length lbs
    -- Force evaluation of the body to have exceptions thrown at the right
    -- time.
    seq len $ writeIORef ref $ SResponse s h $ toLazyByteString builder
    return ResponseReceived
  where
    (s, h, withBody) = responseToStream res

assertBool :: HasCallStack => String -> Bool -> Session ()
assertBool s b = unless b $ assertFailure s

assertString :: HasCallStack => String -> Session ()
assertString s = unless (null s) $ assertFailure s

assertFailure :: HasCallStack => String -> Session ()
assertFailure = liftIO . HUnit.assertFailure

assertContentType :: HasCallStack => ByteString -> SResponse -> Session ()
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

assertStatus :: HasCallStack => Int -> SResponse -> Session ()
assertStatus i SResponse{simpleStatus = s} = assertBool (concat
    [ "Expected status code "
    , show i
    , ", but received "
    , show sc
    ]) $ i == sc
  where
    sc = H.statusCode s

assertBody :: HasCallStack => L.ByteString -> SResponse -> Session ()
assertBody lbs SResponse{simpleBody = lbs'} = assertBool (concat
    [ "Expected response body "
    , show $ L8.unpack lbs
    , ", but received "
    , show $ L8.unpack lbs'
    ]) $ lbs == lbs'

assertBodyContains :: HasCallStack => L.ByteString -> SResponse -> Session ()
assertBodyContains lbs SResponse{simpleBody = lbs'} = assertBool (concat
    [ "Expected response body to contain "
    , show $ L8.unpack lbs
    , ", but received "
    , show $ L8.unpack lbs'
    ]) $ strict lbs `S.isInfixOf` strict lbs'
  where
    strict = S.concat . L.toChunks

assertHeader :: HasCallStack => CI ByteString -> ByteString -> SResponse -> Session ()
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

assertNoHeader :: HasCallStack => CI ByteString -> SResponse -> Session ()
assertNoHeader header SResponse{simpleHeaders = h} =
    case lookup header h of
        Nothing -> return ()
        Just s -> assertString $ concat
            [ "Unexpected header "
            , show header
            , " containing "
            , show s
            ]

-- |
--
-- Since 3.0.6
assertClientCookieExists :: HasCallStack => String -> ByteString -> Session ()
assertClientCookieExists s cookieName = do
  cookies <- getClientCookies
  assertBool s $ Map.member cookieName cookies

-- |
--
-- Since 3.0.6
assertNoClientCookieExists :: HasCallStack => String -> ByteString -> Session ()
assertNoClientCookieExists s cookieName = do
  cookies <- getClientCookies
  assertBool s $ not $ Map.member cookieName cookies

-- |
--
-- Since 3.0.6
assertClientCookieValue :: HasCallStack => String -> ByteString -> ByteString -> Session ()
assertClientCookieValue s cookieName cookieValue = do
  cookies <- getClientCookies
  case Map.lookup cookieName cookies of
    Nothing ->
      assertFailure (s ++ " (cookie does not exist)")
    Just c  ->
      assertBool
        (concat
          [ s
          , " (actual value "
          , show $ Cookie.setCookieValue c
          , " expected value "
          , show cookieValue
          , ")"
          ]
        )
        (Cookie.setCookieValue c == cookieValue)
