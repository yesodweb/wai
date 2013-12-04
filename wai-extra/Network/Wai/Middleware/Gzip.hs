{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
-- |
-- Module        : Network.Wai.Middleware.Gzip
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Automatic gzip compression of responses.
--
---------------------------------------------------------
module Network.Wai.Middleware.Gzip
    ( gzip
    , GzipSettings
    , gzipFiles
    , GzipFiles (..)
    , gzipCheckMime
    , def
    , defaultCheckMime
    ) where

import Network.Wai
import Data.Maybe (fromMaybe, isJust)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as S
import Data.Default
import Network.HTTP.Types (Status, Header)
import Control.Monad.IO.Class (liftIO)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import qualified Data.Conduit as C
import qualified Data.Conduit.Zlib as CZ
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Blaze (builderToByteStringFlush)
import Blaze.ByteString.Builder (fromByteString)
import Control.Exception (try, SomeException)
import qualified Data.Set as Set
import Network.Wai.Internal

data GzipSettings = GzipSettings
    { gzipFiles :: GzipFiles
    , gzipCheckMime :: S.ByteString -> Bool
    }

data GzipFiles = GzipIgnore | GzipCompress | GzipCacheFolder FilePath
    deriving (Show, Eq, Read)

instance Default GzipSettings where
    def = GzipSettings GzipIgnore defaultCheckMime

defaultCheckMime :: S.ByteString -> Bool
defaultCheckMime bs =
    S8.isPrefixOf "text/" bs || bs' `Set.member` toCompress
  where
    bs' = fst $ S.breakByte 59 bs -- semicolon
    toCompress = Set.fromList
        [ "application/json"
        , "application/javascript"
        , "application/ecmascript"
        ]

-- | Use gzip to compress the body of the response.
--
-- Analyzes the \"Accept-Encoding\" header from the client to determine
-- if gzip is supported.
--
-- Possible future enhancements:
--
-- * Only compress if the response is above a certain size.
gzip :: GzipSettings -> Middleware
gzip set app env = do
    res <- app env
    case res of
        ResponseFile{} | gzipFiles set == GzipIgnore -> return res
        _ -> if "gzip" `elem` enc && not isMSIE6 && not (isEncoded res)
                then
                    case (res, gzipFiles set) of
                        (ResponseFile s hs file Nothing, GzipCacheFolder cache) ->
                            case lookup "content-type" hs of
                                Just m
                                    | gzipCheckMime set m -> liftIO $ compressFile s hs file cache
                                _ -> return res
                        _ -> return $ compressE set res
                else return res
  where
    enc = fromMaybe [] $ (splitCommas . S8.unpack)
                    `fmap` lookup "Accept-Encoding" (requestHeaders env)
    ua = fromMaybe "" $ lookup "user-agent" $ requestHeaders env
    isMSIE6 = "MSIE 6" `S.isInfixOf` ua
    isEncoded res = isJust $ lookup "Content-Encoding" $ responseHeaders res

compressFile :: Status -> [Header] -> FilePath -> FilePath -> IO Response
compressFile s hs file cache = do
    e <- doesFileExist tmpfile
    if e
        then onSucc
        else do
            createDirectoryIfMissing True cache
            x <-
               try $ C.runResourceT $ CB.sourceFile file
                C.$$ CZ.gzip C.=$ CB.sinkFile tmpfile
            either onErr (const onSucc) x
  where
    onSucc = return $ ResponseFile s (fixHeaders hs) tmpfile Nothing

    onErr :: SomeException -> IO Response
    onErr = const $ return $ ResponseFile s hs file Nothing -- FIXME log the error message

    tmpfile = cache ++ '/' : map safe file
    safe c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
    safe '-' = '-'
    safe '_' = '_'
    safe _ = '_'

compressE :: GzipSettings
          -> Response
          -> Response
compressE set res =
    case lookup "content-type" hs of
        Just m | gzipCheckMime set m ->
            let hs' = fixHeaders hs
             in ResponseSource s hs' $ \f -> wb $ \b -> f $
                                       b C.$= builderToByteStringFlush
                                         C.$= CZ.compressFlush 1 (CZ.WindowBits 31)
                                         C.$= CL.map (fmap fromByteString)
        _ -> res
  where
    (s, hs, wb) = responseToSource res

-- Remove Content-Length header, since we will certainly have a
-- different length after gzip compression.
fixHeaders :: [Header] -> [Header]
fixHeaders =
    (("Content-Encoding", "gzip") :) . filter notLength
  where
    notLength (x, _) = x /= "content-length"

splitCommas :: String -> [String]
splitCommas [] = []
splitCommas x =
    let (y, z) = break (== ',') x
     in y : splitCommas (dropWhile (== ' ') $ drop 1 z)
