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
    , gzip'
    , GzipSettings
    , gzipFiles
    , GzipFiles (..)
    , def
    , defaultCheckMime
    ) where

import Network.Wai
import Network.Wai.Zlib
import Data.Maybe (fromMaybe)
import Data.Enumerator (($$), joinI, (=$), run)
import Data.Enumerator.Binary (enumFile, iterHandle)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as S
import Data.Default
import Network.HTTP.Types (Status, Header)
import Control.Monad.IO.Class (liftIO)
import qualified Codec.Zlib.Enum as CZE
import qualified System.IO as SIO
import System.Directory (doesFileExist, createDirectoryIfMissing)

data GzipSettings = GzipSettings
    { gzipFiles :: GzipFiles
    , gzipCheckMime :: S.ByteString -> Bool
    }

data GzipFiles = GzipIgnore | GzipCompress | GzipCacheFolder FilePath
    deriving (Show, Eq, Read)

instance Default GzipSettings where
    def = GzipSettings GzipIgnore defaultCheckMime

defaultCheckMime :: S.ByteString -> Bool
defaultCheckMime = S8.isPrefixOf "text/"

-- | Use gzip to compress the body of the response.
--
-- Analyzes the \"Accept-Encoding\" header from the client to determine
-- if gzip is supported.
--
-- Possible future enhancements:
--
-- * Only compress if the response is above a certain size.
gzip :: Bool -- ^ should we gzip files?
     -> Middleware
gzip files = gzip' def
    { gzipFiles = if files then GzipCompress else GzipIgnore
    }

gzip' :: GzipSettings -> Middleware
gzip' set app env = do
    res <- app env
    case res of
        ResponseFile{} | gzipFiles set == GzipIgnore -> return res
        _ -> if "gzip" `elem` enc && not isMSIE6
                then
                    case (res, gzipFiles set) of
                        (ResponseFile s hs file Nothing, GzipCacheFolder cache) ->
                            case lookup "content-type" hs of
                                Just m
                                    | gzipCheckMime set m -> liftIO $ compressFile s hs file cache
                                _ -> return res
                        _ -> return $ ResponseEnumerator $ compressE set $ responseEnumerator res
                else return res
  where
    enc = fromMaybe [] $ (splitCommas . S8.unpack)
                    `fmap` lookup "Accept-Encoding" (requestHeaders env)
    ua = fromMaybe "" $ lookup "user-agent" $ requestHeaders env
    isMSIE6 = "MSIE 6" `S.isInfixOf` ua

compressFile :: Status -> [Header] -> FilePath -> FilePath -> IO Response
compressFile s hs file cache = do
    e <- doesFileExist tmpfile
    if e
        then onSucc
        else do
            createDirectoryIfMissing True cache
            x <- SIO.withFile tmpfile SIO.WriteMode $ \h ->
                   run
                 $ enumFile file
                $$ CZE.gzip
                =$ iterHandle h
            either (const onErr) (const onSucc) x
  where
    onSucc = return $ ResponseFile s (fixHeaders hs) tmpfile Nothing
    onErr = return $ ResponseFile s hs file Nothing
    tmpfile = cache ++ '/' : map safe file
    safe c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
    safe '-' = '-'
    safe '_' = '_'
    safe _ = '_'

compressE :: GzipSettings
          -> (forall a. ResponseEnumerator a)
          -> (forall a. ResponseEnumerator a)
compressE set re f =
    re f'
    --e s hs'
  where
    f' s hs =
        case lookup "content-type" hs of
            Just m | gzipCheckMime set m -> joinI $ compress $$ f s (fixHeaders hs)
            _ -> f s hs

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
