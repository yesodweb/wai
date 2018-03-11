-- | A light-weight wrapper around @Network.Wai@ to provide easy conduit support.
module Network.Wai.Conduit
    ( -- * Request body
      sourceRequestBody
      -- * Response body
    , responseSource
    , responseRawSource
      -- * Re-export
    , module Network.Wai
    ) where

import Network.Wai
import Data.Conduit
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Control.Monad (unless)
import Network.HTTP.Types
import Data.ByteString.Builder (Builder)
import Data.IORef
import qualified Data.Conduit.List as CL

-- | Stream the request body.
--
-- Since 3.0.0
sourceRequestBody :: MonadIO m => Request -> Source m ByteString
sourceRequestBody req =
    loop
  where
    go = liftIO (requestBody req)

    loop = do
        bs <- go
        unless (S.null bs) $ do
            yield bs
            loop

-- | Create an HTTP response out of a @Source@.
--
-- Since 3.0.0
responseSource :: Status -> ResponseHeaders -> Source IO (Flush Builder) -> Response
responseSource s hs src = responseStream s hs $ \send flush ->
    src $$ CL.mapM_ (\mbuilder ->
        case mbuilder of
            Chunk b -> send b
            Flush -> flush)

-- | Create a raw response using a @Source@ and @Sink@ to represent the input
-- and output, respectively.
--
-- Since 3.0.0
responseRawSource :: (MonadIO m, MonadIO n)
                  => (Source m ByteString -> Sink ByteString n () -> IO ())
                  -> Response
                  -> Response
responseRawSource app =
    responseRaw app'
  where
    app' recv send =
        app src sink
      where
        src = do
            bs <- liftIO recv
            unless (S.null bs) $ do
                yield bs
                src
        sink = CL.mapM_ $ liftIO . send
