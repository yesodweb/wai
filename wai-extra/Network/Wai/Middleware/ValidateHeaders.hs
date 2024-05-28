-- | This module provides a middleware to validate response headers.
-- [RFC 9110](https://www.rfc-editor.org/rfc/rfc9110.html#section-5) constrains the allowed octets in header names and values:
--
-- * Header names are [tokens](https://www.rfc-editor.org/rfc/rfc9110#section-5.6.2), i.e. visible ASCII characters (octets 33 to 126 inclusive) except delimiters.
-- * Header values should be limited to visible ASCII characters, the whitespace characters space and horizontal tab and octets 128 to 255. Headers values may not have trailing whitespace (see [RFC 9110 Section 5.5](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.5)). Folding is not allowed.
--
-- 'validateHeadersMiddleware' enforces these constraints for response headers by responding with a 500 Internal Server Error when an offending character is present. This is meant to catch programmer errors early on and reduce attack surface.
module Network.Wai.Middleware.ValidateHeaders
    ( -- * Middleware
      validateHeadersMiddleware
      -- * Settings
    , ValidateHeadersSettings (..)
    , defaultValidateHeadersSettings
      -- * Types
    , InvalidHeader (..)
    , InvalidHeaderReason (..)
    ) where

import Data.CaseInsensitive (original)
import Data.Char (chr)
import Data.Word (Word8)
import Network.HTTP.Types (Header, ResponseHeaders, internalServerError500)
import Network.Wai (Middleware, Response, responseHeaders, responseLBS)
import Text.Printf (printf)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL

-- | Middleware to validate response headers.
--
-- @since 3.1.15
validateHeadersMiddleware :: ValidateHeadersSettings -> Middleware
validateHeadersMiddleware settings app req respond =
    app req respond'
    where
        respond' response = case getInvalidHeader $ responseHeaders response of
            Just invalidHeader -> onInvalidHeader settings invalidHeader app req respond
            Nothing -> respond response

-- | Configuration for 'validateHeadersMiddleware'.
--
-- @since 3.1.15
data ValidateHeadersSettings = ValidateHeadersSettings
  { -- | Called when an invalid header is present.
    onInvalidHeader :: InvalidHeader -> Middleware
  }

-- | Default configuration for 'validateHeadersMiddleware'.
-- Checks that each header meets the requirements listed at the top of this module: Allowed octets for name and value and no trailing whitespace in the value.
--
-- @since 3.1.15
defaultValidateHeadersSettings :: ValidateHeadersSettings
defaultValidateHeadersSettings = ValidateHeadersSettings
  { onInvalidHeader = \invalidHeader _app _req respond -> respond $ invalidHeaderResponse invalidHeader
  }

-- | Description of an invalid header.
--
-- @since 3.1.15
data InvalidHeader = InvalidHeader Header InvalidHeaderReason

-- | Reasons a header might be invalid.
--
-- @since 3.1.15
data InvalidHeaderReason
  -- | Header name contains an invalid octet.
  = InvalidOctetInHeaderName Word8
  -- | Header value contains an invalid octet.
  | InvalidOctetInHeaderValue Word8
  -- | Header value contains trailing whitespace.
  | TrailingWhitespaceInHeaderValue

-- Internal stuff.
-- 'getInvalidHeader' returns an appropriate 'InvalidHeader' for a given header if applicable.
-- 'invalidHeaderResponse' creates a 'Response' for a given 'InvalidHeader'.

getInvalidHeader :: ResponseHeaders -> Maybe InvalidHeader
getInvalidHeader = firstJust . map go
    where
        firstJust :: [Maybe a] -> Maybe a
        firstJust [] = Nothing
        firstJust (Just x : _) = Just x
        firstJust (_ : xs) = firstJust xs

        go :: Header -> Maybe InvalidHeader
        go header@(name, value) = InvalidHeader header <$> firstJust
          [ InvalidOctetInHeaderName <$> BS.find (not . isValidHeaderNameOctet) (original name)
          , InvalidOctetInHeaderValue <$> BS.find (not . isValidHeaderValueOctet) value
          , if hasTrailingWhitespace value then Just TrailingWhitespaceInHeaderValue else Nothing
          ]

isValidHeaderNameOctet :: Word8 -> Bool
isValidHeaderNameOctet octet =
    isVisibleASCII octet && not (isDelimiter octet)

isValidHeaderValueOctet :: Word8 -> Bool
isValidHeaderValueOctet octet =
    isVisibleASCII octet || isWhitespace octet || isObsText octet

isVisibleASCII :: Word8 -> Bool
isVisibleASCII octet = octet >= 33 && octet <= 126

isDelimiter :: Word8 -> Bool
isDelimiter octet = chr (fromIntegral octet) `elem` ("\"(),/:;<=>?@[\\]{}" :: String)

-- Whitespace characters are only horizontal tab and space here.
isWhitespace :: Word8 -> Bool
isWhitespace octet = octet == 0x9 || octet == 0x20

isObsText :: Word8 -> Bool
isObsText octet = octet >= 0x80

hasTrailingWhitespace :: BS.ByteString -> Bool
hasTrailingWhitespace bs
  | BS.length bs == 0 = False
  | otherwise = isWhitespace (BS.index bs 0) || isWhitespace (BS.index bs $ BS.length bs - 1)

invalidHeaderResponse :: InvalidHeader -> Response
invalidHeaderResponse (InvalidHeader (headerName, headerValue) reason) =
    responseLBS internalServerError500 [("Content-Type", "text/plain")] $ BSL.concat
        [ "Invalid response header found:\n"
        , "In header '"
        , BSL.fromStrict $ original headerName
        , "' with value '"
        , BSL.fromStrict headerValue
        , "': "
        , showReason reason
        , "\nYou are seeing this error message because validateHeadersMiddleware is enabled."
        ]
    where
        showReason (InvalidOctetInHeaderName octet) = "Name contains invalid octet " <> showOctet octet
        showReason (InvalidOctetInHeaderValue octet) = "Value contains invalid octet " <> showOctet octet
        showReason TrailingWhitespaceInHeaderValue = "Value contains trailing whitespace."

        showOctet octet
            | isVisibleASCII octet = BSL.fromStrict $ BS8.pack $ printf "'%c' (0x%02X)" (chr $ fromIntegral octet) octet
            | otherwise = BSL.fromStrict $ BS8.pack $ printf "0x%02X" octet
