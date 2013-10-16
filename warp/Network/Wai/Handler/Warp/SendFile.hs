{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.SendFile where

import Data.ByteString (ByteString)
import Network.Sendfile
import Network.Socket (Socket)
import qualified Network.Wai.Handler.Warp.FdCache as F
import Network.Wai.Handler.Warp.Types

defaultSendFile :: Socket -> FilePath -> Integer -> Integer -> IO () -> [ByteString] -> IO ()
defaultSendFile s path off len act hdr = sendfileWithHeader s path (PartOfFile off len) act hdr


#if SENDFILEFD
setSendFile :: Connection -> Maybe F.MutableFdCache -> Connection
setSendFile conn Nothing    = conn
setSendFile conn (Just fdcs) = case connSendFileOverride conn of
    NotOverride -> conn
    Override s  -> conn { connSendFile = sendFile fdcs s }

sendFile :: F.MutableFdCache -> Socket -> FilePath -> Integer -> Integer -> IO () -> [ByteString] -> IO ()
sendFile fdcs s path off len act hdr = do
    (fd, fresher) <- F.getFd fdcs path
    sendfileFdWithHeader s fd (PartOfFile off len) (act>>fresher) hdr
#else
setSendFile :: Connection -> Maybe F.MutableFdCache -> Connection
setSendFile conn _ = conn
#endif
