{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.File (
    RspFileInfo(..)
  , conditionalRequest
  , addContentHeadersForFilePart
  ) where

import Control.Applicative ((<|>))
import Data.Array ((!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as B (pack, readInteger)
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Network.HTTP.Date
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Header as H
import Network.Wai
import qualified Network.Wai.Handler.Warp.FileInfoCache as I
import Network.Wai.Handler.Warp.Header
import Numeric (showInt)

----------------------------------------------------------------

data RspFileInfo = WithoutBody H.Status
                 | WithBody H.Status H.ResponseHeaders Integer Integer
                 deriving (Eq,Show)

----------------------------------------------------------------

conditionalRequest :: I.FileInfo
                   -> H.ResponseHeaders -> IndexedHeader
                   -> RspFileInfo
conditionalRequest finfo hs0 reqidx = case fromJust mx of
    nobody@(WithoutBody _) -> nobody
    WithBody s _ off len   -> let hs = (H.hLastModified,date) :
                                       addContentHeaders hs0 off len size
                              in WithBody s hs off len
  where
    mtime = I.fileInfoTime finfo
    size  = I.fileInfoSize finfo
    date  = I.fileInfoDate finfo
    mx = ifmodified    reqidx size mtime
                         <|> ifunmodified  reqidx size mtime
                         <|> ifrange       reqidx size mtime
                         <|> unconditional reqidx size mtime

----------------------------------------------------------------

ifModifiedSince :: IndexedHeader -> Maybe HTTPDate
ifModifiedSince reqidx = reqidx ! idxIfModifiedSince >>= parseHTTPDate

ifUnmodifiedSince :: IndexedHeader -> Maybe HTTPDate
ifUnmodifiedSince reqidx = reqidx ! idxIfUnmodifiedSince >>= parseHTTPDate

ifRange :: IndexedHeader -> Maybe HTTPDate
ifRange reqidx = reqidx ! idxIfRange >>= parseHTTPDate

----------------------------------------------------------------

ifmodified :: IndexedHeader -> Integer -> HTTPDate -> Maybe RspFileInfo
ifmodified reqidx size mtime = do
    date <- ifModifiedSince reqidx
    if date /= mtime
       then unconditional reqidx size mtime
       else Just $ WithoutBody H.notModified304

ifunmodified :: IndexedHeader -> Integer -> HTTPDate -> Maybe RspFileInfo
ifunmodified reqidx size mtime = do
    date <- ifUnmodifiedSince reqidx
    if date == mtime
       then unconditional reqidx size mtime
       else Just $ WithoutBody H.preconditionFailed412

ifrange :: IndexedHeader -> Integer -> HTTPDate -> Maybe RspFileInfo
ifrange reqidx size mtime = do
    date <- ifRange reqidx
    rng  <- reqidx ! idxRange
    if date == mtime
       then parseRange rng size
       else Just $ WithBody H.ok200 [] 0 size

unconditional :: IndexedHeader -> Integer -> HTTPDate -> Maybe RspFileInfo
unconditional reqidx size _ = case reqidx ! idxRange of
    Nothing  -> Just $ WithBody H.ok200 [] 0 size
    Just rng -> parseRange rng size

----------------------------------------------------------------

parseRange :: ByteString -> Integer -> Maybe RspFileInfo
parseRange rng size = case parseByteRanges rng of
    Nothing    -> Just $ WithoutBody H.requestedRangeNotSatisfiable416
    Just []    -> Just $ WithoutBody H.requestedRangeNotSatisfiable416
    Just (r:_) -> let (!beg, !end) = checkRange r size
                      !len = end - beg + 1
                      s = if beg == 0 && end == size - 1 then
                              H.ok200
                            else
                              H.partialContent206
                  in Just $ WithBody s [] beg len

checkRange :: H.ByteRange -> Integer -> (Integer, Integer)
checkRange (H.ByteRangeFrom   beg)     size = (beg, size - 1)
checkRange (H.ByteRangeFromTo beg end) size = (beg,  min (size - 1) end)
checkRange (H.ByteRangeSuffix count)   size = (max 0 (size - count), size - 1)

-- | Parse the value of a Range header into a 'H.ByteRanges'.
parseByteRanges :: B.ByteString -> Maybe H.ByteRanges
parseByteRanges bs1 = do
    bs2 <- stripPrefix "bytes=" bs1
    (r, bs3) <- range bs2
    ranges (r:) bs3
  where
    range bs2 = do
        (i, bs3) <- B.readInteger bs2
        if i < 0 -- has prefix "-" ("-0" is not valid, but here treated as "0-")
            then Just (H.ByteRangeSuffix (negate i), bs3)
            else do
                bs4 <- stripPrefix "-" bs3
                case B.readInteger bs4 of
                    Just (j, bs5) | j >= i -> Just (H.ByteRangeFromTo i j, bs5)
                    _ -> Just (H.ByteRangeFrom i, bs4)
    ranges front bs3
        | B.null bs3 = Just (front [])
        | otherwise = do
            bs4 <- stripPrefix "," bs3
            (r, bs5) <- range bs4
            ranges (front . (r:)) bs5

    stripPrefix x y
        | x `B.isPrefixOf` y = Just (B.drop (B.length x) y)
        | otherwise = Nothing

----------------------------------------------------------------

-- | @contentRangeHeader beg end total@ constructs a Content-Range 'H.Header'
-- for the range specified.
contentRangeHeader :: Integer -> Integer -> Integer -> H.Header
contentRangeHeader beg end total = (H.hContentRange, range)
  where
    range = B.pack
      -- building with ShowS
      $ 'b' : 'y': 't' : 'e' : 's' : ' '
      : (if beg > end then ('*':) else
          showInt beg
          . ('-' :)
          . showInt end)
      ( '/'
      : showInt total "")

addContentHeaders :: H.ResponseHeaders -> Integer -> Integer -> Integer -> H.ResponseHeaders
addContentHeaders hs off len size = hs''
  where
    contentRange = contentRangeHeader off (off + len - 1) size
    lengthBS = L.toStrict $ B.toLazyByteString $ B.integerDec len
    hs' = (H.hContentLength, lengthBS):(H.hAcceptRanges, "bytes"):hs
    hs'' = if len == size then hs' else contentRange:hs'

-- |
--
-- >>> addContentHeadersForFilePart [] (FilePart 2 10 16)
-- [("Content-Range","bytes 2-11/16"),("Content-Length","10"),("Accept-Ranges","bytes")]
-- >>> addContentHeadersForFilePart [] (FilePart 0 16 16)
-- [("Content-Length","16"),("Accept-Ranges","bytes")]
addContentHeadersForFilePart :: H.ResponseHeaders -> FilePart -> H.ResponseHeaders
addContentHeadersForFilePart hs part = addContentHeaders hs off len size
  where
    off = filePartOffset part
    len = filePartByteCount part
    size = filePartFileSize part
