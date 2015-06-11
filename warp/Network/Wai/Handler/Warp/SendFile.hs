{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.SendFile where

import Network.Sendfile
import Network.Socket (Socket)
import Network.Wai.Handler.Warp.Types

defaultSendFile :: Socket -> SendFile
#ifdef SENDFILEFD
defaultSendFile s fid off len act hdr = case mfid of
    Nothing -> sendfileWithHeader   s path (PartOfFile off len) act hdr
    Just fd -> sendfileFdWithHeader s fd   (PartOfFile off len) act hdr
  where
    path = fileIdPath fid
    mfid = fileIdFd fid
#else
defaultSendFile s fid off len act hdr =
    sendfileWithHeader s path (PartOfFile off len) act hdr
  where
    path = fileIdPath fid
#endif
