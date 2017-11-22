{-# LANGUAGE OverloadedStrings, BangPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Network.Wai.Handler.Warp.HTTP2.HPACK (
    hpackEncodeHeader
  , hpackEncodeHeaderLoop
  , hpackDecodeHeader
  , just
  , addNecessaryHeaders
  , addHeader -- testing
  ) where

import qualified Control.Exception as E
import Network.HPACK hiding (Buffer)
import Network.HPACK.Token
import Network.HTTP2

import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.PackInt
import qualified Network.Wai.Handler.Warp.Settings as S
import Network.Wai.Handler.Warp.Types

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

{-# INLINE addHeader #-}
addHeader :: Token -> ByteString -> ValueTable -> TokenHeaderList -> TokenHeaderList
addHeader t "" tbl ths = case getHeaderValue t tbl of
    Nothing -> ths
    _       -> filter ((/= tokenServer) . fst) ths
addHeader t v tbl ths = case getHeaderValue t tbl of
    Nothing -> (t,v) : ths
    _       -> ths

addNecessaryHeaders :: Context
                    -> Rspn
                    -> InternalInfo
                    -> S.Settings
                    -> IO TokenHeaderList
addNecessaryHeaders Context{..} rspn ii settings = do
    date <- getDate ii
    let !s = rspnStatus rspn
        !status = packStatus s
        !defServer = S.settingsServerName settings
        (!ths0,tbl) = rspnHeaders rspn
        !ths1 = addHeader tokenServer defServer tbl ths0
        !ths2 = addHeader tokenDate date tbl ths1
        !ths3 = (tokenStatus, status) : ths2
    return ths3

----------------------------------------------------------------

strategy :: EncodeStrategy
strategy = EncodeStrategy { compressionAlgo = Linear, useHuffman = False }

-- Set-Cookie: contains only one cookie value.
-- So, we don't need to split it.
hpackEncodeHeader :: Context -> Buffer -> BufSize
                  -> TokenHeaderList
                  -> IO (TokenHeaderList, Int)
hpackEncodeHeader Context{..} buf siz ths =
    encodeTokenHeader buf siz strategy True encodeDynamicTable ths

hpackEncodeHeaderLoop :: Context -> Buffer -> BufSize
                      -> TokenHeaderList
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
  | just mMethod (== "CONNECT") = isNothing mPath && isNothing mScheme
  | isJust mStatus              = False
  | isNothing mMethod           = False
  | isNothing mScheme           = False
  | isNothing mPath             = False
  | mPath       == Just ""      = False
  | isJust mConnection          = False
  | just mTE (/= "trailers")    = False
  | otherwise                   = True
  where
    mStatus     = getHeaderValue tokenStatus reqvt
    mScheme     = getHeaderValue tokenScheme reqvt
    mPath       = getHeaderValue tokenPath reqvt
    mMethod     = getHeaderValue tokenMethod reqvt
    mConnection = getHeaderValue tokenConnection reqvt
    mTE         = getHeaderValue tokenTE reqvt

{-# INLINE just #-}
just :: Maybe a -> (a -> Bool) -> Bool
just Nothing  _    = False
just (Just x) p
  | p x            = True
  | otherwise      = False
