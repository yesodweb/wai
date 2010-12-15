module Network.Wai.Zlib (compress) where

import Prelude hiding (head)
import Network.Wai
import Data.ByteString (ByteString)
import Data.Enumerator
    ( Enumeratee, checkDone, Stream (..), continue
    , (>>==), head, ($$), joinI
    )
import Blaze.ByteString.Builder (Builder, fromByteString)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Control.Monad.IO.Class (liftIO)

import Codec.Zlib

-- Note: this function really should return a stream of ByteStrings, but the
-- WAI protocol needs Builders anyway.
compress :: Enumeratee Builder Builder IO a
compress step0 = joinI $ builderToByteString $$ do
    def <- liftIO $ initDeflate 7 $ WindowBits 31
    loop def step0
  where
    loop def = checkDone $ step def
    step def k = do
        minput <- head
        case minput of
            Nothing -> do
                bss <- liftIO $ finishDeflate def drain
                k (Chunks bss) >>== return
            Just input -> do
                bss <- liftIO $ withDeflateInput def input drain
                case bss of
                    [] -> step def k
                    _ -> k (Chunks bss) >>== loop def

drain =
    go id
  where
    go front mbs' = do
        mbs <- mbs'
        case mbs of
            Nothing -> return $ map fromByteString $ front []
            Just bs -> go (front . (:) bs) mbs'

    {-

compressIter :: (acc -> ByteString -> IO (Either acc acc))
             -> Deflate
             -> acc
             -> ByteString
             -> IO (Either acc acc)
compressIter iter def acc bsI = withDeflateInput def bsI $ drain iter acc

drain :: (acc -> ByteString -> IO (Either acc acc))
      -> acc
      -> IO (Maybe ByteString)
      -> IO (Either acc acc)
drain iter acc pop = do
    mbs <- pop
    case mbs of
        Nothing -> return $ Right acc
        Just bs -> do
            eacc' <- iter acc bs
            case eacc' of
                Left acc' -> return $ Left acc'
                Right acc' -> drain iter acc' pop
    -}
