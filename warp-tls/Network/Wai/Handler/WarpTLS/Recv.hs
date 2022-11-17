{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.WarpTLS.Recv where

import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.IORef (IORef)
import qualified Data.IORef as I
import qualified Network.TLS as TLS
import Network.Wai.Handler.Warp.Internal
import System.IO.Error (isEOFError)
import UnliftIO.Exception (throwIO, handle, fromException)

makeRecv :: TLS.Context -> IO (Recv, RecvBuf)
makeRecv ctx = do
    ref <- I.newIORef ""
    return (recv ref ctx, recvBuf ref ctx)

-- TLS version of recv with a cache for leftover input data.
-- The cache is shared with recvBuf.
recv :: IORef ByteString -> TLS.Context -> Recv
recv cref ctx = do
    cached <- I.readIORef cref
    if cached /= "" then do
        I.writeIORef cref ""
        return cached
      else
        recv' ctx

-- TLS version of recv (decrypting) without a cache.
recv' :: TLS.Context -> Recv
recv' ctx = handle onEOF $ TLS.recvData ctx
  where
    onEOF e
      | Just TLS.Error_EOF <- fromException e       = return S.empty
      | Just ioe <- fromException e, isEOFError ioe = return S.empty                  | otherwise                                   = throwIO e

-- TLS version of recvBuf with a cache for leftover input data.
recvBuf :: IORef ByteString -> TLS.Context -> RecvBuf
recvBuf cref ctx buf siz = do
    cached <- I.readIORef cref
    (ret, leftover) <- fill cached buf siz $ recv' ctx
    I.writeIORef cref leftover
    return ret

fill :: ByteString -> Buffer -> BufSize -> Recv -> IO (Bool,ByteString)
fill bs0 buf0 siz0 rcv
  | siz0 <= len0 = do
      let (bs, leftover) = S.splitAt siz0 bs0
      void $ copy buf0 bs
      return (True, leftover)
  | otherwise = do
      buf <- copy buf0 bs0
      loop buf (siz0 - len0)
  where
    len0 = S.length bs0
    loop _   0   = return (True, "")
    loop buf siz = do
      bs <- rcv
      let len = S.length bs
      if len == 0 then return (False, "")
        else if len <= siz then do
          buf' <- copy buf bs
          loop buf' (siz - len)
        else do
          let (bs1,bs2) = S.splitAt siz bs
          void $ copy buf bs1
          return (True, bs2)
