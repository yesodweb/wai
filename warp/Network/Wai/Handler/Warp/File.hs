{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.File where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Exception
import Data.Maybe
import qualified Network.HTTP.Types as H
import Network.Wai
import qualified Network.Wai.Handler.Warp.FileInfoCache as I
import Network.Wai.Handler.Warp.Types
import Network.Wai.Internal

----------------------------------------------------------------

fileRange :: H.Status -> H.ResponseHeaders -> FilePath
          -> Maybe FilePart -> Maybe HeaderValue
          -> (FilePath -> IO I.FileInfo)
          -> IO (Either IOException
                        (H.Status, H.ResponseHeaders, Integer, Integer))
fileRange s0 hs0 path Nothing mRange get = do
    efinfo <- try (get path)
    case efinfo of
        Left  e     -> return $ Left e
        Right finfo -> do
            let ret = fileRangeSized s0 hs0 Nothing mRange $ I.fileInfoSize finfo
            return $ Right ret
fileRange s0 hs0 _ mPart@(Just part) mRange _ =
    return . Right $ fileRangeSized s0 hs0 mPart mRange size
  where
    size = filePartFileSize part

fileRangeSized :: H.Status -> H.ResponseHeaders
               -> Maybe FilePart -> Maybe HeaderValue -> Integer
               -> (H.Status, H.ResponseHeaders, Integer, Integer)
fileRangeSized s0 hs0 mPart mRange fileSize = (s, hs, beg, len)
  where
    part = fromMaybe (chooseFilePart fileSize mRange) mPart
    beg = filePartOffset part
    len = filePartByteCount part
    (s, hs) = adjustForFilePart s0 hs0 $ FilePart beg len fileSize
