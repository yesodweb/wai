{-# LANGUAGE LambdaCase #-}
module Network.Wai.Middleware.CombineHeaders
    ( combineHeaders
    , CombineSettings (..)
    , defaultCombineSettings
    ) where

import qualified Data.ByteString as B
import qualified Data.List as L (foldl', reverse)
import qualified Data.Map.Strict as M
import Data.Word8 (_comma, _space)
import Network.HTTP.Types (HeaderName, RequestHeaders)
import qualified Network.HTTP.Types.Header as H
import Network.Wai (Application, requestHeaders, mapResponseHeaders)
import Network.Wai.Header (dropWhileEnd)

-- |
--
-- @since 3.1.13.0
data CombineSettings = CombineSettings {
    combineRequestHeaders :: Bool,
    -- ^ Should request headers be combined?
    combineResponseHeaders :: Bool,
    -- ^ Should response headers be combined?
    combineHeaderMap :: M.Map HeaderName HandleType
    -- ^ Which headers should be combined? And how?
} deriving (Eq, Show)

-- | Settings that combine request headers, but don't touch response headers.
--
-- All types of headers that /can/ be combined /will/ be combined.
--
-- @since 3.1.13.0
defaultCombineSettings :: CombineSettings
defaultCombineSettings = CombineSettings {
    combineRequestHeaders = True,
    combineResponseHeaders = False,
    combineHeaderMap = defaultHeaderMap
}

-- |
-- This middleware will reorganize the incoming headers in
-- such a way that it combines any duplicates of headers
-- that, on their own, can normally have more than one value,
-- and any other headers will stay untouched.
--
-- This middleware WILL change the global order of headers
-- (they will be put in alphabetical order), but keep the
-- order of the same type of header. I.e. if there are 3
-- \"Cookie\" headers, the first one will still be first, the
-- second one will still be second, etc. But now they are
-- guaranteed to be next to each other.
--
-- N.B. This 'Middleware' assumes the headers it combines
-- are correctly formatted. If one of the to-be-combined
-- headers is malformed, the new combined header will also
-- (probably) be malformed.
--
-- @since 3.1.13.0
combineHeaders :: CombineSettings -> Application -> Application
combineHeaders (CombineSettings doReq doRes headerMap) app req resFunc =
    app newReq $ resFunc . adjustRes
  where
    newReq
        | doReq = req { requestHeaders = mkNewHeaders oldHeaders }
        | otherwise = req
    oldHeaders = requestHeaders req
    adjustRes
        | doRes = mapResponseHeaders mkNewHeaders
        | otherwise = id
    mkNewHeaders =
        M.foldrWithKey' finishHeaders [] . L.foldl' go mempty
    go acc hdr@(name, _) =
        M.alter (checkHeader hdr) name acc
    checkHeader (name, newVal) = Just . \case
        Nothing -> (name `M.lookup` headerMap, [newVal])
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
    combineHdrs = B.intercalate ", " . fmap clean . L.reverse
    clean = dropWhileEnd $ \w -> w == _comma || w == _space

type HeaderHandling = (Maybe HandleType, [B.ByteString])

-- | Both will concatenate with @,@ (commas), but 'KeepOnly' will
-- drop all headers except the given one (in case of wildcards/special values)
data HandleType
    = Regular
    | KeepOnly B.ByteString
   deriving (Eq, Show)

defaultHeaderMap :: M.Map HeaderName HandleType
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
    , ("Alt-Svc" , KeepOnly "clear") -- special "clear" value (if any is "clear", only keep that one)
    , (H.hCacheControl, Regular)
    , ("Clear-Site-Data" , KeepOnly "*") -- wildcard (if any is "*", only keep that one)

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
