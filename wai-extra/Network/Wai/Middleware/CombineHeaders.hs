{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{- |
Sometimes incoming requests don't stick to the
"no duplicate headers" invariant, for a number
of possible reasons (e.g. proxy servers blindly
adding headers), or your application (or other
middleware) blindly adds headers.

In those cases, you can use this 'Middleware'
to make sure that headers that /can/ be combined
/are/ combined. (e.g. applications might only
check the first \"Accept\" header and fail, while
there might be another one that would match)
    -}
module Network.Wai.Middleware.CombineHeaders
    ( combineHeaders
    , CombineSettings
    , defaultCombineSettings
    , HeaderMap
    , HandleType
    , defaultHeaderMap
    -- * Adjusting the settings
    , setHeader
    , removeHeader
    , setHeaderMap
    , regular
    , keepOnly
    , setRequestHeaders
    , setResponseHeaders
    ) where

import qualified Data.ByteString as B
import qualified Data.List as L (foldl', reverse)
import qualified Data.Map.Strict as M
import Data.Word8 (_comma, _space, _tab)
import Network.HTTP.Types (Header, HeaderName, RequestHeaders)
import qualified Network.HTTP.Types.Header as H
import Network.Wai (Middleware, requestHeaders, mapResponseHeaders)
import Network.Wai.Util (dropWhileEnd)

-- | The mapping of 'HeaderName' to 'HandleType'
type HeaderMap = M.Map HeaderName HandleType

-- | These settings define which headers should be combined,
-- if the combining should happen on incoming (request) headers
-- and if it should happen on outgoing (response) headers.
--
-- Any header you put in the header map *will* be used to
-- combine those headers with commas. There's no check to see
-- if it is a header that allows comma-separated lists, so if
-- you want to combine custom headers, go ahead.
--
-- (You can check the documentation of 'defaultCombineSettings'
-- to see which standard headers are specified to be able to be
-- combined)
--
-- @since 3.1.13.0
data CombineSettings = CombineSettings {
    combineHeaderMap :: HeaderMap,
    -- ^ Which headers should be combined? And how? (cf. 'HandleType')
    combineRequestHeaders :: Bool,
    -- ^ Should request headers be combined?
    combineResponseHeaders :: Bool
    -- ^ Should response headers be combined?
} deriving (Eq, Show)

-- | Settings that combine request headers,
-- but don't touch response headers.
--
-- All types of headers that /can/ be combined
-- (as defined in the spec) /will/ be combined.
--
-- To be exact, this is the list:
--
-- * Accept
-- * Accept-CH
-- * Accept-Charset
-- * Accept-Encoding
-- * Accept-Language
-- * Accept-Post
-- * Access-Control-Allow-Headers
-- * Access-Control-Allow-Methods
-- * Access-Control-Expose-Headers
-- * Access-Control-Request-Headers
-- * Allow
-- * Alt-Svc @(KeepOnly \"clear\"")@
-- * Cache-Control
-- * Clear-Site-Data @(KeepOnly \"*\")@
-- * Connection
-- * Content-Encoding
-- * Content-Language
-- * Digest
-- * If-Match
-- * If-None-Match @(KeepOnly \"*\")@
-- * Link
-- * Permissions-Policy
-- * TE
-- * Timing-Allow-Origin @(KeepOnly \"*\")@
-- * Trailer
-- * Transfer-Encoding
-- * Upgrade
-- * Via
-- * Vary @(KeepOnly \"*\")@
-- * Want-Digest
--
-- N.B. Any header name that has \"KeepOnly\" after it
-- will be combined like normal, unless one of the values
-- is the one mentioned (\"*\" most of the time), then
-- that value is used and all others are dropped.
--
-- @since 3.1.13.0
defaultCombineSettings :: CombineSettings
defaultCombineSettings = CombineSettings {
    combineHeaderMap = defaultHeaderMap,
    combineRequestHeaders = True,
    combineResponseHeaders = False
}

-- | Override the 'HeaderMap' of the 'CombineSettings'
--  (default: 'defaultHeaderMap')
--
-- @since 3.1.13.0
setHeaderMap :: HeaderMap -> CombineSettings -> CombineSettings
setHeaderMap mp set = set{combineHeaderMap = mp}

-- | Set whether the combining of headers should be applied to
-- the incoming request headers. (default: True)
--
-- @since 3.1.13.0
setRequestHeaders :: Bool -> CombineSettings -> CombineSettings
setRequestHeaders b set = set{combineRequestHeaders = b}

-- | Set whether the combining of headers should be applied to
-- the outgoing response headers. (default: False)
--
-- @since 3.1.13.0
setResponseHeaders :: Bool -> CombineSettings -> CombineSettings
setResponseHeaders b set = set{combineResponseHeaders = b}

-- | Convenience function to add a header to the header map or,
-- if it is already in the map, to change the 'HandleType'.
--
-- @since 3.1.13.0
setHeader :: HeaderName -> HandleType -> CombineSettings -> CombineSettings
setHeader name typ settings =
    settings {
        combineHeaderMap = M.insert name typ $ combineHeaderMap settings
    }

-- | Convenience function to remove a header from the header map.
--
-- @since 3.1.13.0
removeHeader :: HeaderName -> CombineSettings -> CombineSettings
removeHeader name settings =
    settings {
        combineHeaderMap = M.delete name $ combineHeaderMap settings
    }

-- | This middleware will reorganize the incoming and/or outgoing
-- headers in such a way that it combines any duplicates of
-- headers that, on their own, can normally have more than one
-- value, and any other headers will stay untouched.
--
-- This middleware WILL change the global order of headers
-- (they will be put in alphabetical order), but keep the
-- order of the same type of header. I.e. if there are 3
-- \"Set-Cookie\" headers, the first one will still be first,
-- the second one will still be second, etc. But now they are
-- guaranteed to be next to each other.
--
-- N.B. This 'Middleware' assumes the headers it combines
-- are correctly formatted. If one of the to-be-combined
-- headers is malformed, the new combined header will also
-- (probably) be malformed.
--
-- @since 3.1.13.0
combineHeaders :: CombineSettings -> Middleware
combineHeaders CombineSettings{..} app req resFunc =
    app newReq $ resFunc . adjustRes
  where
    newReq
        | combineRequestHeaders = req { requestHeaders = mkNewHeaders oldHeaders }
        | otherwise = req
    oldHeaders = requestHeaders req
    adjustRes
        | combineResponseHeaders = mapResponseHeaders mkNewHeaders
        | otherwise = id
    mkNewHeaders =
        M.foldrWithKey' finishHeaders [] . L.foldl' go mempty
    go acc hdr@(name, _) =
        M.alter (checkHeader hdr) name acc
    checkHeader :: Header -> Maybe HeaderHandling -> Maybe HeaderHandling
    checkHeader (name, newVal) = Just . \case
        Nothing -> (name `M.lookup` combineHeaderMap, [newVal])
        -- Yes, this reverses the order of headers, but these
        -- will be reversed again in 'finishHeaders'
        Just (mHandleType, hdrs) -> (mHandleType, newVal : hdrs)

-- | Unpack 'HeaderHandling' back into 'Header's again
finishHeaders :: HeaderName -> HeaderHandling -> RequestHeaders -> RequestHeaders
finishHeaders name (shouldCombine, xs) hdrs =
    case shouldCombine of
        Just typ -> (name, combinedHeader typ) : hdrs
        Nothing ->
            -- Yes, this reverses the headers, but they
            -- were already reversed by 'checkHeader'
            L.foldl' (\acc el -> (name, el) : acc) hdrs xs
  where
    combinedHeader Regular = combineHdrs xs
    combinedHeader (KeepOnly val)
        | val `elem` xs = val
        | otherwise = combineHdrs xs
    -- headers were reversed, so do 'reverse' before combining
    combineHdrs = B.intercalate ", " . fmap clean . L.reverse
    clean = dropWhileEnd $ \w -> w == _comma || w == _space || w == _tab

type HeaderHandling = (Maybe HandleType, [B.ByteString])

-- | Both will concatenate with @,@ (commas), but 'KeepOnly' will drop all
-- values except the given one if present (e.g. in case of wildcards/special values)
--
-- For example: If there are multiple @"Clear-Site-Data"@ headers, but one of
-- them is the wildcard @\"*\"@ value, using @'KeepOnly' "*"@ will cause all
-- others to be dropped and only the wildcard value to remain.
-- (The @\"*\"@ wildcard in this case means /ALL site data/ should be cleared,
-- so no need to include more)
--
-- @since 3.1.13.0
data HandleType
    = Regular
    | KeepOnly B.ByteString
   deriving (Eq, Show)

-- | Use the regular strategy when combining headers.
-- (i.e. merge into one header and separate values with commas)
--
-- @since 3.1.13.0
regular :: HandleType
regular = Regular

-- | Use the regular strategy when combining headers,
-- but if the exact supplied 'ByteString' is encountered
-- then discard all other values and only keep that value.
--
-- e.g. @keepOnly "*"@ will drop all other encountered values
--
-- @since 3.1.13.0
keepOnly :: B.ByteString -> HandleType
keepOnly = KeepOnly

-- | The default collection of HTTP headers that can be combined
-- in case there are multiples in one request or response.
--
-- See the documentation of 'defaultCombineSettings' for the exact list.
--
-- @since 3.1.13.0
defaultHeaderMap :: HeaderMap
defaultHeaderMap = M.fromList
    [ (H.hAccept, Regular)
    , ("Accept-CH", Regular)
    , (H.hAcceptCharset, Regular)
    , (H.hAcceptEncoding, Regular)
    , (H.hAcceptLanguage, Regular)
    , ("Accept-Post", Regular)
    , ("Access-Control-Allow-Headers" , Regular) -- wildcard? yes, but can just add to list
    , ("Access-Control-Allow-Methods" , Regular) -- wildcard? yes, but can just add to list
    , ("Access-Control-Expose-Headers" , Regular) -- wildcard? yes, but can just add to list
    , ("Access-Control-Request-Headers", Regular)
    , (H.hAllow, Regular)
    , ("Alt-Svc", KeepOnly "clear") -- special "clear" value (if any is "clear", only keep that one)
    , (H.hCacheControl, Regular)
    , ("Clear-Site-Data", KeepOnly "*") -- wildcard (if any is "*", only keep that one)

    -- If "close" and anything else is used together, it's already F-ed,
    -- so just combine them.
    , (H.hConnection, Regular)

    , (H.hContentEncoding, Regular)
    , (H.hContentLanguage, Regular)
    , ("Digest", Regular)

    -- We could handle this, but it's experimental AND
    -- will be replaced by "Permissions-Policy"
    -- , "Feature-Policy" -- "semicolon ';' separated"

    , (H.hIfMatch, Regular)
    , (H.hIfNoneMatch, KeepOnly "*") -- wildcard? (if any is "*", only keep that one)
    , ("Link", Regular)
    , ("Permissions-Policy", Regular)
    , (H.hTE, Regular)
    , ("Timing-Allow-Origin", KeepOnly "*") -- wildcard? (if any is "*", only keep that one)
    , (H.hTrailer, Regular)
    , (H.hTransferEncoding, Regular)
    , (H.hUpgrade, Regular)
    , (H.hVia, Regular)
    , (H.hVary, KeepOnly "*") -- wildcard? (if any is "*", only keep that one)
    , ("Want-Digest", Regular)
    ]
