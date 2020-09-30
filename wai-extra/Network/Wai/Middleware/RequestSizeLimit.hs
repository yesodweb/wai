module Network.Wai.Middleware.RequestSizeLimit (requestSizeLimitMiddleware) where

import Network.Wai
import Network.Wai.Request
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word64)
import Network.HTTP.Types.Status (requestEntityTooLarge413)
import qualified Data.ByteString.Lazy.Char8 as LS8
import Control.Exception (try, catch)

-- | Middleware to limit request bodies to a certain size. 
-- If the body is too large, this middleware returns HTTP 413, with a plain text body describing the error.
--
-- This uses 'requestSizeCheck' under the hood; see that function for details.
--
-- @since 3.1.1
requestSizeLimitMiddleware :: Word64 -> Middleware
requestSizeLimitMiddleware maxLen app req sendResponse = do
    let sendTooLargeResponse = sendResponse (tooLargeResponse maxLen (requestBodyLength req))

    eitherSizeExceptionOrNewReq <- try (requestSizeCheck maxLen req)
    case eitherSizeExceptionOrNewReq of
        -- In the case of a known-length request, RequestSizeException will be thrown immediately
        Left (RequestSizeException _maxLen) -> sendTooLargeResponse
        -- In the case of a chunked request (unknown length), RequestSizeException will be thrown during the processing of a body
        Right newReq -> app newReq sendResponse `catch` \(RequestSizeException _maxLen) -> sendTooLargeResponse

tooLargeResponse :: Word64 -> RequestBodyLength -> Response
tooLargeResponse maxLen bodyLen = responseLBS
    requestEntityTooLarge413
    [("Content-Type", "text/plain")]
    (BSL.concat 
        [ "Request body too large to be processed. The maximum size is "
        , (LS8.pack (show maxLen))
        , " bytes; your request body was "
        , case bodyLen of
            KnownLength bodyLen -> (LS8.pack (show bodyLen)) <> " bytes."
            ChunkedBody -> "split into chunks, whose total size is unknown, but exceeded the limit."
        , " . If you're the developer of this site, you can configure the maximum length with `requestSizeLimitMiddleware`."
        ])
