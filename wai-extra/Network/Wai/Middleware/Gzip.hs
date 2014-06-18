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
import Data.Default.Class
import Network.HTTP.Types (Status, Header)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Blaze.ByteString.Builder (fromByteString)
import Control.Exception (try, SomeException)
import qualified Data.Set as Set
import Network.Wai.Internal
import qualified Data.Streaming.Blaze as B
import qualified Data.Streaming.Zlib as Z
import qualified Blaze.ByteString.Builder as Blaze
import Control.Monad (unless)
import Data.Function (fix)
import Control.Exception (throwIO)
import qualified System.IO as IO
import Data.ByteString.Lazy.Internal (defaultChunkSize)

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
gzip set app env sendResponse = app env $ \res ->
    case res of
        ResponseRaw{} -> sendResponse res
        ResponseFile{} | gzipFiles set == GzipIgnore -> sendResponse res
        _ -> if "gzip" `elem` enc && not isMSIE6 && not (isEncoded res)
                then
                    case (res, gzipFiles set) of
                        (ResponseFile s hs file Nothing, GzipCacheFolder cache) ->
                            case lookup "content-type" hs of
                                Just m
                                    | gzipCheckMime set m -> compressFile s hs file cache sendResponse
                                _ -> sendResponse res
                        _ -> compressE set res sendResponse
                else sendResponse res
  where
    enc = fromMaybe [] $ (splitCommas . S8.unpack)
                    `fmap` lookup "Accept-Encoding" (requestHeaders env)
    ua = fromMaybe "" $ lookup "user-agent" $ requestHeaders env
    isMSIE6 = "MSIE 6" `S.isInfixOf` ua
    isEncoded res = isJust $ lookup "Content-Encoding" $ responseHeaders res

compressFile :: Status -> [Header] -> FilePath -> FilePath -> (Response -> IO a) -> IO a
compressFile s hs file cache sendResponse = do
    e <- doesFileExist tmpfile
    if e
        then onSucc
        else do
            createDirectoryIfMissing True cache
            x <- try $
                 IO.withBinaryFile file IO.ReadMode $ \inH ->
                 IO.withBinaryFile tmpfile IO.WriteMode $ \outH -> do
                    deflate <- Z.initDeflate 7 $ Z.WindowBits 31
                    -- FIXME this code should write to a temporary file, then
                    -- rename to the final file
                    let goPopper popper = fix $ \loop -> do
                            res <- popper
                            case res of
                                Z.PRDone -> return ()
                                Z.PRNext bs -> do
                                    S.hPut outH bs
                                    loop
                                Z.PRError e -> throwIO e
                    fix $ \loop -> do
                        bs <- S.hGetSome inH defaultChunkSize
                        unless (S.null bs) $ do
                            Z.feedDeflate deflate bs >>= goPopper
                            loop
                    goPopper $ Z.finishDeflate deflate
            either onErr (const onSucc) (x :: Either SomeException ()) -- FIXME bad! don't catch all exceptions like that!
  where
    onSucc = sendResponse $ responseFile s (fixHeaders hs) tmpfile Nothing

    onErr _ = sendResponse $ responseFile s hs file Nothing -- FIXME log the error message

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
          -> (Response -> IO a)
          -> IO a
compressE set res sendResponse =
    case lookup "content-type" hs of
        Just m | gzipCheckMime set m ->
            let hs' = fixHeaders hs
             in wb $ \body -> sendResponse $ responseStream s hs' $ \sendChunk flush -> do
                    (blazeRecv, blazeFinish) <- B.newBlazeRecv B.defaultStrategy
                    deflate <- Z.initDeflate 1 (Z.WindowBits 31)
                    let sendBuilder builder = do
                            popper <- blazeRecv builder
                            fix $ \loop -> do
                                bs <- popper
                                unless (S.null bs) $ do
                                    sendBS bs
                                    loop
                        sendBS bs = Z.feedDeflate deflate bs >>= deflatePopper
                        flushBuilder = do
                            sendBuilder Blaze.flush
                            deflatePopper $ Z.flushDeflate deflate
                            flush
                        deflatePopper popper = fix $ \loop -> do
                            res <- popper
                            case res of
                                Z.PRDone -> return ()
                                Z.PRNext bs' -> do
                                    sendChunk $ fromByteString bs'
                                    loop
                                Z.PRError e -> throwIO e

                    body sendBuilder flushBuilder
                    sendBuilder Blaze.flush
                    deflatePopper $ Z.finishDeflate deflate
        _ -> sendResponse res
  where
    (s, hs, wb) = responseToStream res

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
