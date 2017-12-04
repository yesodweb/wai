{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.File (
    RspFileInfo(..)
  , conditionalRequest
  , addContentHeadersForFilePart
  , H.parseByteRanges
  ) where

import Data.Array ((!))
import qualified Data.ByteString.Char8 as C8 (pack)
import Network.HTTP.Date
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Header as H
import Network.Wai

import qualified Network.Wai.Handler.Warp.FileInfoCache as I
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.PackInt


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
    WithBody s _ off len   -> let !hs = (H.hLastModified,date) :
                                        addContentHeaders hs0 off len size
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
parseRange rng size = case H.parseByteRanges rng of
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

----------------------------------------------------------------

-- | @contentRangeHeader beg end total@ constructs a Content-Range 'H.Header'
-- for the range specified.
contentRangeHeader :: Integer -> Integer -> Integer -> H.Header
contentRangeHeader beg end total = (H.hContentRange, range)
  where
    range = C8.pack
      -- building with ShowS
      $ 'b' : 'y': 't' : 'e' : 's' : ' '
      : (if beg > end then ('*':) else
          showInt beg
          . ('-' :)
          . showInt end)
      ( '/'
      : showInt total "")

addContentHeaders :: H.ResponseHeaders -> Integer -> Integer -> Integer -> H.ResponseHeaders
addContentHeaders hs off len size
  | len == size = hs'
  | otherwise   = let !ctrng = contentRangeHeader off (off + len - 1) size
                  in ctrng:hs'
  where
    !lengthBS = packIntegral len
    !hs' = (H.hContentLength, lengthBS) : (H.hAcceptRanges,"bytes") : hs

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
