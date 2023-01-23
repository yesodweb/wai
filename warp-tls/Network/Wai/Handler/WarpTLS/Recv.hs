{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.WarpTLS.Recv where

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.IORef (IORef)
import qualified Data.IORef as I
import qualified Network.TLS as TLS
import Network.Wai.Handler.Warp.Internal
import System.IO.Error (isEOFError)
import UnliftIO.Exception (throwIO, handle, fromException)

makeRecv :: TLS.Context -> IO Recv
makeRecv ctx = do
    ref <- I.newIORef ""
    return $ recv ref ctx

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
