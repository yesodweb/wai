-- | Internal constructors and helper functions. Note that no guarantees are given for stability of these interfaces.
module Network.Wai.Middleware.RequestSizeLimit.Internal (
    RequestSizeLimitSettings (..),
    setMaxLengthForRequest,
    setOnLengthExceeded,
) where

import Data.Word (Word64)
import Network.Wai (Middleware, Request)

-- | Settings to configure 'requestSizeLimitMiddleware'.
--
-- This type (but not the constructor, or record fields) is exported from "Network.Wai.Middleware.RequestSizeLimit".
-- Since the constructor isn't exported, create a default value with 'defaultRequestSizeLimitSettings' first,
-- then set the values using 'setMaxLengthForRequest' and 'setOnLengthExceeded' (See the examples below).
--
-- If you need to access the constructor directly, it's exported from "Network.Wai.Middleware.RequestSizeLimit.Internal".
--
-- ==== __Examples__
--
-- ===== Conditionally setting the limit based on the request
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Network.Wai
-- > import Network.Wai.Middleware.RequestSizeLimit
-- >
-- > let megabyte = 1024 * 1024
-- > let sizeForReq req = if pathInfo req == ["upload", "image"] then pure $ Just $ megabyte * 20 else pure $ Just $ megabyte * 2
-- > let finalSettings = setMaxLengthForRequest sizeForReq defaultRequestSizeLimitSettings
--
-- ===== JSON response
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Network.Wai
-- > import Network.Wai.Middleware.RequestSizeLimit
-- > import Network.HTTP.Types.Status (requestEntityTooLarge413)
-- > import Data.Aeson
-- > import Data.Text (Text)
-- >
-- > let jsonResponse = \_maxLen _app _req sendResponse -> sendResponse $ responseLBS requestEntityTooLarge413 [("Content-Type", "application/json")] (encode $ object ["error" .= ("request size too large" :: Text)])
-- > let finalSettings = setOnLengthExceeded jsonResponse defaultRequestSizeLimitSettings
--
-- @since 3.1.1
data RequestSizeLimitSettings = RequestSizeLimitSettings
    { maxLengthForRequest :: Request -> IO (Maybe Word64)
    -- ^ Function to determine the maximum request size in bytes for the request. Return 'Nothing' for no limit. Since 3.1.1
    , onLengthExceeded :: Word64 -> Middleware
    -- ^ Callback function when maximum length is exceeded. The 'Word64' argument is the limit computed by 'maxLengthForRequest'. Since 3.1.1
    }

-- | Function to determine the maximum request size in bytes for the request. Return 'Nothing' for no limit.
--
-- @since 3.1.1
setMaxLengthForRequest
    :: (Request -> IO (Maybe Word64))
    -> RequestSizeLimitSettings
    -> RequestSizeLimitSettings
setMaxLengthForRequest fn settings = settings{maxLengthForRequest = fn}

-- | Callback function when maximum length is exceeded. The 'Word64' argument is the limit computed by 'setMaxLengthForRequest'.
--
-- @since 3.1.1
setOnLengthExceeded
    :: (Word64 -> Middleware) -> RequestSizeLimitSettings -> RequestSizeLimitSettings
setOnLengthExceeded fn settings = settings{onLengthExceeded = fn}
