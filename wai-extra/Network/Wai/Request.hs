{-# LANGUAGE DeriveDataTypeable #-}

-- | Some helpers for interrogating a WAI 'Request'.
module Network.Wai.Request (
    appearsSecure,
    guessApproot,
    RequestSizeException (..),
    requestSizeCheck,
) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Network.HTTP.Types (HeaderName)
import Network.Wai

-- | Does this request appear to have been made over an SSL connection?
--
-- This function first checks @'isSecure'@, but also checks for headers that may
-- indicate a secure connection even in the presence of reverse proxies.
--
-- Note: these headers can be easily spoofed, so decisions which require a true
-- SSL connection (i.e. sending sensitive information) should only use
-- @'isSecure'@. This is not always the case though: for example, deciding to
-- force a non-SSL request to SSL by redirect. One can safely choose not to
-- redirect when the request /appears/ secure, even if it's actually not.
--
-- @since 3.0.7
appearsSecure :: Request -> Bool
appearsSecure request =
    isSecure request
        || any
            (uncurry matchHeader)
            [ ("HTTPS", (== "on"))
            , ("HTTP_X_FORWARDED_SSL", (== "on"))
            , ("HTTP_X_FORWARDED_SCHEME", (== "https"))
            , ("HTTP_X_FORWARDED_PROTO", (== ["https"]) . take 1 . C.split ',')
            , ("X-Forwarded-Proto", (== "https")) -- Used by Nginx and AWS ELB.
            ]
  where
    matchHeader :: HeaderName -> (ByteString -> Bool) -> Bool
    matchHeader h f = maybe False f $ lookup h $ requestHeaders request

-- | Guess the \"application root\" based on the given request.
--
-- The application root is the basis for forming URLs pointing at the current
-- application. For more information and relevant caveats, please see
-- "Network.Wai.Middleware.Approot".
--
-- @since 3.0.7
guessApproot :: Request -> ByteString
guessApproot req =
    (if appearsSecure req then "https://" else "http://")
        `S.append` fromMaybe "localhost" (requestHeaderHost req)

-- | see 'requestSizeCheck'
--
-- @since 3.0.15
newtype RequestSizeException
    = RequestSizeException Word64
    deriving (Eq, Ord, Typeable)

instance Exception RequestSizeException

instance Show RequestSizeException where
    showsPrec p (RequestSizeException limit) =
        showString "Request Body is larger than "
            . showsPrec p limit
            . showString " bytes."

-- | Check request body size to avoid server crash when request is too large.
--
-- This function first checks @'requestBodyLength'@, if content-length is known
-- but larger than limit, or it's unknown but we have received too many chunks,
-- a 'RequestSizeException' are thrown when user use @'requestBody'@ to extract
-- request body inside IO.
--
-- @since 3.0.15
requestSizeCheck :: Word64 -> Request -> IO Request
requestSizeCheck maxSize req =
    case requestBodyLength req of
        KnownLength len ->
            if len > maxSize
                then return $ setRequestBodyChunks (throwIO $ RequestSizeException maxSize) req
                else return req
        ChunkedBody -> do
            currentSize <- newIORef 0
            let rbody = do
                    bs <- getRequestBodyChunk req
                    total <-
                        atomicModifyIORef' currentSize $ \sz ->
                            let nextSize = sz + fromIntegral (S.length bs)
                             in (nextSize, nextSize)
                    if total > maxSize
                        then throwIO (RequestSizeException maxSize)
                        else return bs
            return $ setRequestBodyChunks rbody req
