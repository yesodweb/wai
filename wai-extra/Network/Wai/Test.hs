{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
    , WaiTestFailure (..)
    ) where

import Network.Wai
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, evalStateT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad (unless)
import Control.DeepSeq (deepseq)
import Control.Exception (throwIO, Exception)
import Data.Typeable (Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Blaze.ByteString.Builder (toLazyByteString)
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.HTTP.Types as H
import Data.CaseInsensitive (CI)
import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.IORef
import Data.Monoid (mempty, mappend)

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
    let pInfo = dropFrontSlash $ T.split (== '/') $ TE.decodeUtf8 rawPinfo
    in  r { rawPathInfo = rawPinfo, pathInfo = pInfo }
  where
    dropFrontSlash ("":"":[]) = [] -- homepage, a single slash
    dropFrontSlash ("":path) = path
    dropFrontSlash path = path

srequest :: SRequest -> Session SResponse
srequest (SRequest req bod) = do
    app <- ask
    refChunks <- liftIO $ newIORef $ L.toChunks bod
    let req' = req
            { requestBody = atomicModifyIORef refChunks $ \bss ->
                case bss of
                    [] -> ([], S.empty)
                    x:y -> (y, x)
            }
    liftIO $ do
        ref <- newIORef $ error "runResponse gave no result"
        ResponseReceived <- app req' (runResponse ref)
        readIORef ref
    -- FIXME cookie processing
    --return sres

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

assertBool :: String -> Bool -> Session ()
assertBool s b = unless b $ assertFailure s

assertString :: String -> Session ()
assertString s = unless (null s) $ assertFailure s

assertFailure :: String -> Session ()
assertFailure msg = msg `deepseq` liftIO (throwIO (WaiTestFailure msg))

data WaiTestFailure = WaiTestFailure String
    deriving (Show, Eq, Typeable)
instance Exception WaiTestFailure

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
