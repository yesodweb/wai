{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.File (
    RspFileInfo(..)
  , conditionalRequest
  , addContentHeadersForFilePart
  , parseByteRanges
  ) where

import Control.Applicative ((<|>))
import Data.Array ((!))
import qualified Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Char8 as B (pack, readInteger)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe, isJust)
import Network.HTTP.Date
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Header as H
import Network.Wai
import qualified Network.Wai.Handler.Warp.FileInfoCache as I
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.PackInt
import Numeric (showInt)

#ifndef MIN_VERSION_http_types
#define MIN_VERSION_http_types(x,y,z) 1
#endif

-- $setup
-- >>> import Test.QuickCheck

----------------------------------------------------------------

data RspFileInfo = WithoutBody H.Status
                 | WithBody H.Status H.ResponseHeaders Integer Integer
                 deriving (Eq,Show)

----------------------------------------------------------------

conditionalRequest :: I.FileInfo
                   -> H.ResponseHeaders -> IndexedHeader
                   -> RspFileInfo
conditionalRequest finfo hs0 reqidx = case condition of
    nobody@(WithoutBody _) -> nobody
    WithBody s _ off len   -> let !hs = if isJust (lookup H.hLastModified hs0)
                                          then addContentHeaders hs0 off len size
                                          else (H.hLastModified,date) : addContentHeaders hs0 off len size
                              in WithBody s hs off len
  where
    !mtime = I.fileInfoTime finfo
    !size  = I.fileInfoSize finfo
    !date  = I.fileInfoDate finfo
    !mcondition = ifmodified    reqidx size mtime
              <|> ifunmodified  reqidx size mtime
              <|> ifrange       reqidx size mtime
    !condition = fromMaybe (unconditional reqidx size) mcondition

----------------------------------------------------------------

ifModifiedSince :: IndexedHeader -> Maybe HTTPDate
ifModifiedSince reqidx = reqidx ! fromEnum ReqIfModifiedSince >>= parseHTTPDate

ifUnmodifiedSince :: IndexedHeader -> Maybe HTTPDate
ifUnmodifiedSince reqidx = reqidx ! fromEnum ReqIfUnmodifiedSince >>= parseHTTPDate

ifRange :: IndexedHeader -> Maybe HTTPDate
ifRange reqidx = reqidx ! fromEnum ReqIfRange >>= parseHTTPDate

----------------------------------------------------------------

ifmodified :: IndexedHeader -> Integer -> HTTPDate -> Maybe RspFileInfo
ifmodified reqidx size mtime = do
    date <- ifModifiedSince reqidx
    return $ if date /= mtime
             then unconditional reqidx size
             else WithoutBody H.notModified304

ifunmodified :: IndexedHeader -> Integer -> HTTPDate -> Maybe RspFileInfo
ifunmodified reqidx size mtime = do
    date <- ifUnmodifiedSince reqidx
    return $ if date == mtime
             then unconditional reqidx size
             else WithoutBody H.preconditionFailed412

ifrange :: IndexedHeader -> Integer -> HTTPDate -> Maybe RspFileInfo
ifrange reqidx size mtime = do
    date <- ifRange reqidx
    rng  <- reqidx ! fromEnum ReqRange
    return $ if date == mtime
             then parseRange rng size
             else WithBody H.ok200 [] 0 size

unconditional :: IndexedHeader -> Integer -> RspFileInfo
unconditional reqidx size = case reqidx ! fromEnum ReqRange of
    Nothing  -> WithBody H.ok200 [] 0 size
    Just rng -> parseRange rng size

----------------------------------------------------------------

parseRange :: ByteString -> Integer -> RspFileInfo
parseRange rng size = case parseByteRanges rng of
    Nothing    -> WithoutBody H.requestedRangeNotSatisfiable416
    Just []    -> WithoutBody H.requestedRangeNotSatisfiable416
    Just (r:_) -> let (!beg, !end) = checkRange r size
                      !len = end - beg + 1
                      s = if beg == 0 && end == size - 1 then
                              H.ok200
                            else
                              H.partialContent206
                  in WithBody s [] beg len

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

contentRange :: H.HeaderName
#if MIN_VERSION_http_types(0,9,0)
contentRange = H.hContentRange
#else
contentRange = "Content-Range"
#endif

-- | @contentRangeHeader beg end total@ constructs a Content-Range 'H.Header'
-- for the range specified.
contentRangeHeader :: Integer -> Integer -> Integer -> H.Header
contentRangeHeader beg end total = (contentRange, range)
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

acceptRange :: H.HeaderName
#if MIN_VERSION_http_types(0,9,0)
acceptRange = H.hAcceptRanges
#else
acceptRange = "Accept-Ranges"
#endif

addContentHeaders :: H.ResponseHeaders -> Integer -> Integer -> Integer -> H.ResponseHeaders
addContentHeaders hs off len size
  | len == size = hs'
  | otherwise   = let !ctrng = contentRangeHeader off (off + len - 1) size
                  in ctrng:hs'
  where
    !lengthBS = packIntegral len
    !hs' = (H.hContentLength, lengthBS) : (acceptRange,"bytes") : hs

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
