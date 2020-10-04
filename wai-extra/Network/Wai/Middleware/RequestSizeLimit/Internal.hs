-- | Internal constructors and helper functions. Note that no guarantees are given for stability of these interfaces.
module Network.Wai.Middleware.RequestSizeLimit.Internal
    ( RequestSizeLimitSettings(..)
    ) where

import Network.Wai
import Data.Word (Word64)

-- | Settings to configure 'requestSizeLimitMiddleware'.
--
-- This type and its accessors (but not the constructor) are exported from "Network.Wai.Middleware.RequestSizeLimit".
-- Since the constructor isn't exported, create a default value with 'defaultRequestSizeLimitSettings' first, then override the individual fields.
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
-- > let finalSettings = defaultRequestSizeLimitSettings { maxLengthForRequest = sizeForReq }
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
-- > let finalSettings = defaultRequestSizeLimitSettings { onLengthExceeded = jsonResponse }
--
-- @since 3.1.1
data RequestSizeLimitSettings = RequestSizeLimitSettings
    { maxLengthForRequest :: Request -> IO (Maybe Word64) -- ^ Function to determine the maximum request size in bytes for the request. Return 'Nothing' for no limit. Since 3.1.1
    , onLengthExceeded :: Word64 -> Middleware -- ^ Callback function when maximum length is exceeded. The 'Word64' argument is the limit computed by 'maxLengthForRequest'. Since 3.1.1
    }
