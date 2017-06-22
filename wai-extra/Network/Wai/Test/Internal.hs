module Network.Wai.Test.Internal where

import Network.Wai
import qualified Control.Monad.Trans.State as ST
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Web.Cookie as Cookie
import Data.ByteString (ByteString)

type Session = ReaderT Application (ST.StateT ClientState IO)

-- |
--
-- Since 3.0.6
type ClientCookies = Map ByteString Cookie.SetCookie

data ClientState = ClientState
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
