{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.Wai.Handler.Warp.HTTP2.HPACK where

import Control.Arrow (first)
import qualified Control.Exception as E
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as B8
import Data.CaseInsensitive (foldedCase)
import Data.IORef (readIORef, writeIORef)
import Network.HPACK
import qualified Network.HTTP.Types as H
import Network.HTTP2
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.Response
import qualified Network.Wai.Handler.Warp.Settings as S
import Network.Wai.Handler.Warp.Types

-- $setup
-- >>> :set -XOverloadedStrings

-- Set-Cookie: contains only one cookie value.
-- So, we don't need to split it.
hpackEncodeHeader :: Context -> InternalInfo -> S.Settings
                  -> H.Status -> H.ResponseHeaders
                  -> IO Builder
hpackEncodeHeader ctx ii settings s h = do
    hdr1 <- addServerAndDate h
    let hdr2 = (":status", status) : map (first foldedCase) hdr1
    hpackEncodeRawHeaders ctx hdr2
  where
    status = B8.pack $ show $ H.statusCode s
    dc = dateCacher ii
    rspidxhdr = indexResponseHeader h
    defServer = S.settingsServerName settings
    addServerAndDate = addDate dc rspidxhdr . addServer defServer rspidxhdr

hpackEncodeCIHeaders :: Context -> [H.Header] -> IO Builder
hpackEncodeCIHeaders ctx = hpackEncodeRawHeaders ctx . map (first foldedCase)

hpackEncodeRawHeaders :: Context -> [(B.ByteString, B.ByteString)] -> IO Builder
hpackEncodeRawHeaders Context{encodeDynamicTable} hdr = do
    ehdrtbl <- readIORef encodeDynamicTable
    (ehdrtbl', builder) <- encodeHeaderBuilder defaultEncodeStrategy ehdrtbl hdr
    writeIORef encodeDynamicTable ehdrtbl'
    return builder

----------------------------------------------------------------

hpackDecodeHeader :: HeaderBlockFragment -> Context -> IO HeaderList
hpackDecodeHeader hdrblk Context{decodeDynamicTable} = do
    hdrtbl <- readIORef decodeDynamicTable
    (hdrtbl', hdr) <- decodeHeader hdrtbl hdrblk `E.onException` cleanup
    writeIORef decodeDynamicTable hdrtbl'
    return hdr
  where
    cleanup = E.throwIO $ ConnectionError CompressionError "cannot decompress the header"
