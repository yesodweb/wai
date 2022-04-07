module Network.Wai.Test.Internal where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.State as ST
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Network.Wai (Application)
import qualified Web.Cookie as Cookie

type Session = ReaderT Application (ST.StateT ClientState IO)

-- |
--
-- Since 3.0.6
type ClientCookies = Map ByteString Cookie.SetCookie

newtype ClientState = ClientState
    { clientCookies :: ClientCookies
    }

-- |
--
-- Since 3.0.20.0
initState :: ClientState
initState = ClientState Map.empty

-- | Like 'runSession', but if allows you to hand in cookies and get
-- the updated cookies back.  One use case for this is writing tests
-- that address the application under test alternatingly through rest
-- api and through db handle.
--
-- Since 3.0.20.0
runSessionWith :: ClientState -> Session a -> Application -> IO (a, ClientState)
runSessionWith st session app = ST.runStateT (runReaderT session app) st
