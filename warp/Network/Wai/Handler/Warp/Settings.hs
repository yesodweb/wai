module Network.Wai.Handler.Warp.Settings where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as S
import Data.Conduit
import Data.Conduit.Network (HostPreference (HostIPv4))
import Network.Wai
import Network.Wai.Handler.Warp.Timeout
import Network.Wai.Handler.Warp.Types
import System.IO (hPrint, stderr)

-- | Various Warp server settings. This is purposely kept as an abstract data
-- type so that new settings can be added without breaking backwards
-- compatibility. In order to create a 'Settings' value, use 'defaultSettings'
-- and record syntax to modify individual records. For example:
--
-- > defaultSettings { settingsTimeout = 20 }
data Settings = Settings
    { settingsPort :: Int -- ^ Port to listen on. Default value: 3000
    , settingsHost :: HostPreference -- ^ Default value: HostIPv4
    , settingsOnException :: SomeException -> IO () -- ^ What to do with exceptions thrown by either the application or server. Default: ignore server-generated exceptions (see 'InvalidRequest') and print application-generated applications to stderr.
    , settingsOnOpen :: IO () -- ^ What to do when a connection is open. Default: do nothing.
    , settingsOnClose :: IO ()  -- ^ What to do when a connection is close. Default: do nothing.
    , settingsTimeout :: Int -- ^ Timeout value in seconds. Default value: 30
    , settingsIntercept :: Request -> Maybe (Source (ResourceT IO) S.ByteString -> Connection -> ResourceT IO ())
    , settingsManager :: Maybe Manager -- ^ Use an existing timeout manager instead of spawning a new one. If used, 'settingsTimeout' is ignored. Default is 'Nothing'
    , settingsFdCacheDuration :: Int -- ^ Cache duratoin time of file descriptors in seconds. Default value: 10
    , settingsResourceTPerRequest :: Bool
      -- ^ If @True@, each request\/response pair will run in a separate
      -- @ResourceT@. This provides more intuitive behavior for dynamic code,
      -- but can hinder performance in high-throughput cases. File servers can
      -- safely set to @False@ for increased performance. Default is @True@.
    }

-- | The default settings for the Warp server. See the individual settings for
-- the default value.
defaultSettings :: Settings
defaultSettings = Settings
    { settingsPort = 3000
    , settingsHost = HostIPv4
    , settingsOnException = \e ->
        case fromException e of
            Just x -> go x
            Nothing ->
                when (go' $ fromException e) $
                    hPrint stderr e
    , settingsOnOpen = return ()
    , settingsOnClose = return ()
    , settingsTimeout = 30
    , settingsIntercept = const Nothing
    , settingsManager = Nothing
    , settingsFdCacheDuration = 10
    , settingsResourceTPerRequest = True
    }
  where
    go :: InvalidRequest -> IO ()
    go _ = return ()
    go' (Just ThreadKilled) = False
    go' _ = True
