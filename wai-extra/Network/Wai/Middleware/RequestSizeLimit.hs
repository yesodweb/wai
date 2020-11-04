-- | The functions in this module allow you to limit the total size of incoming request bodies.
--
-- Limiting incoming request body size helps protect your server against denial-of-service (DOS) attacks,
-- in which an attacker sends huge bodies to your server.
module Network.Wai.Middleware.RequestSizeLimit
    (
    -- * Middleware
      requestSizeLimitMiddleware
    -- * Constructing 'RequestSizeLimitSettings'
    , defaultRequestSizeLimitSettings
    -- * 'RequestSizeLimitSettings' and accessors
    , RequestSizeLimitSettings
    , setMaxLengthForRequest
    , setOnLengthExceeded
    ) where

import Network.Wai
import Network.Wai.Request
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word64)
import Network.HTTP.Types.Status (requestEntityTooLarge413)
import qualified Data.ByteString.Lazy.Char8 as LS8
import Control.Exception (try, catch)
import Data.Monoid ((<>))
import Network.Wai.Middleware.RequestSizeLimit.Internal (RequestSizeLimitSettings(..), setMaxLengthForRequest, setOnLengthExceeded)

-- | Create a 'RequestSizeLimitSettings' with these settings:
--
-- * 2MB size limit for all requests
-- * When the limit is exceeded, return a plain text response describing the error, with a 413 status code.
--
-- @since 3.1.1
defaultRequestSizeLimitSettings :: RequestSizeLimitSettings
defaultRequestSizeLimitSettings = RequestSizeLimitSettings
    { maxLengthForRequest = \_req -> pure $ Just $ 2 * 1024 * 1024
    , onLengthExceeded = \maxLen _app req sendResponse -> sendResponse (tooLargeResponse maxLen (requestBodyLength req))
    }

-- | Middleware to limit request bodies to a certain size.
--
-- This uses 'requestSizeCheck' under the hood; see that function for details.
--
-- @since 3.1.1
requestSizeLimitMiddleware :: RequestSizeLimitSettings -> Middleware
requestSizeLimitMiddleware settings app req sendResponse = do
    maybeMaxLen <- (maxLengthForRequest settings) req

    case maybeMaxLen of
        Nothing -> app req sendResponse
        Just maxLen -> do
            eitherSizeExceptionOrNewReq <- try (requestSizeCheck maxLen req)
            case eitherSizeExceptionOrNewReq of
                -- In the case of a known-length request, RequestSizeException will be thrown immediately
                Left (RequestSizeException _maxLen) -> handleLengthExceeded maxLen
                -- In the case of a chunked request (unknown length), RequestSizeException will be thrown during the processing of a body
                Right newReq -> app newReq sendResponse `catch` \(RequestSizeException _maxLen) -> handleLengthExceeded maxLen

    where
        handleLengthExceeded maxLen = (onLengthExceeded settings) maxLen app req sendResponse

tooLargeResponse :: Word64 -> RequestBodyLength -> Response
tooLargeResponse maxLen bodyLen = responseLBS
    requestEntityTooLarge413
    [("Content-Type", "text/plain")]
    (BSL.concat
        [ "Request body too large to be processed. The maximum size is "
        , (LS8.pack (show maxLen))
        , " bytes; your request body was "
        , case bodyLen of
            KnownLength knownLen -> (LS8.pack (show knownLen)) <> " bytes."
            ChunkedBody -> "split into chunks, whose total size is unknown, but exceeded the limit."
        , " If you're the developer of this site, you can configure the maximum length with `requestSizeLimitMiddleware`."
        ])
