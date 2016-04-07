{-# LANGUAGE OverloadedStrings, BangPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Network.Wai.Handler.Warp.HTTP2.HPACK (
    hpackEncodeHeader
  , hpackEncodeHeaderLoop
  , hpackDecodeHeader
  , just
  ) where

import qualified Control.Exception as E
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Network.HPACK hiding (Buffer)
import Network.HPACK.Token
import qualified Network.HTTP.Types as H
import Network.HTTP2
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.PackInt
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
                  -> H.Status -> (TokenHeaderList,ValueTable)
                  -> IO (TokenHeaderList, Int)
hpackEncodeHeader Context{..} buf siz ii settings s (ths0,tbl) = do
    let !defServer = S.settingsServerName settings
        !ths1 = addHeader tokenServer defServer tbl ths0
    date <- getDate ii
    let !ths2 = addHeader tokenDate date tbl ths1
        !status = packStatus s
        !ths3 = (tokenStatus, status) : ths2
    encodeTokenHeader buf siz strategy True encodeDynamicTable ths3

{-# INLINE addHeader #-}
addHeader :: Token -> ByteString -> ValueTable -> TokenHeaderList -> TokenHeaderList
addHeader t v tbl ths = case getHeaderValue t tbl of
    Nothing -> (t,v) : ths
    Just _  -> ths

hpackEncodeHeaderLoop :: Context -> Buffer -> BufSize -> TokenHeaderList
                      -> IO (TokenHeaderList, Int)
hpackEncodeHeaderLoop Context{..} buf siz hs =
    encodeTokenHeader buf siz strategy False encodeDynamicTable hs

----------------------------------------------------------------

hpackDecodeHeader :: HeaderBlockFragment -> Context -> IO (TokenHeaderList, ValueTable)
hpackDecodeHeader hdrblk Context{..} = do
    tbl@(_,vt) <- decodeTokenHeader decodeDynamicTable hdrblk `E.catch` handl
    unless (checkRequestHeader vt) $
        E.throwIO $ ConnectionError ProtocolError "the header key is illegal"
    return tbl
  where
    handl IllegalHeaderName =
        E.throwIO $ ConnectionError ProtocolError "the header key is illegal"
    handl _ =
        E.throwIO $ ConnectionError CompressionError "cannot decompress the header"

{-# INLINE checkRequestHeader #-}
checkRequestHeader :: ValueTable -> Bool
checkRequestHeader reqvt
  | getHeaderValue tokenStatus     reqvt /= Nothing     = False
  | getHeaderValue tokenPath       reqvt == Nothing     = False
  | getHeaderValue tokenMethod     reqvt == Nothing     = False
  | getHeaderValue tokenAuthority  reqvt == Nothing     = False
  | getHeaderValue tokenConnection reqvt /= Nothing     = False
  | just (getHeaderValue tokenTE reqvt) (/= "trailers") = False
  | otherwise                                           = True

{-# INLINE just #-}
just :: Maybe a -> (a -> Bool) -> Bool
just Nothing  _    = False
just (Just x) p
  | p x            = True
  | otherwise      = False
