{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Network.Wai.Handler.Warp.HTTP2.HPACK where

import Control.Arrow (first)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B8
import Data.CaseInsensitive (foldedCase)
import Network.HPACK hiding (Buffer)
import qualified Network.HTTP.Types as H
import Network.HTTP2
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.Response
import qualified Network.Wai.Handler.Warp.Settings as S
import Network.Wai.Handler.Warp.Types

-- $setup
-- >>> :set -XOverloadedStrings

strategy :: EncodeStrategy
strategy = EncodeStrategy { compressionAlgo = Linear, useHuffman = False }

-- Set-Cookie: contains only one cookie value.
-- So, we don't need to split it.
hpackEncodeHeader :: Context -> Buffer -> BufSize
                  -> InternalInfo -> S.Settings
                  -> H.Status -> H.ResponseHeaders
                  -> IO (HeaderList, Int)
hpackEncodeHeader Context{..} buf siz ii settings s hdr0 = do
    hdr1 <- addServerAndDate hdr0
    let hs = (":status", status) : map (first foldedCase) hdr1
    encodeHeaderBuffer buf siz strategy True encodeDynamicTable hs
  where
    status = B8.pack $ show $ H.statusCode s
    dc = dateCacher ii
    rspidxhdr = indexResponseHeader hdr0
    defServer = S.settingsServerName settings
    addServerAndDate = addDate dc rspidxhdr . addServer defServer rspidxhdr

hpackEncodeHeaderLoop :: Context -> Buffer -> BufSize -> HeaderList
                      -> IO (HeaderList, Int)
hpackEncodeHeaderLoop Context{..} buf siz hs =
    encodeHeaderBuffer buf siz strategy False encodeDynamicTable hs

----------------------------------------------------------------

hpackDecodeHeader :: HeaderBlockFragment -> Context -> IO HeaderList
hpackDecodeHeader hdrblk Context{..} =
    decodeHeader decodeDynamicTable hdrblk `E.catch` \(E.SomeException _) ->
        E.throwIO $ ConnectionError CompressionError "cannot decompress the header"
