{-# LANGUAGE BangPatterns #-}

-- | In-memory TLS session manager.
--
-- * Limitation: you can set the maximum size of the session data database.
-- * Automatic pruning: old session data over their lifetime are pruned automatically.
-- * Energy saving: no dedicate pruning thread is running when the size of session data database is zero.
-- * (Replay resistance: each session data is used at most once to prevent replay attacks against 0RTT early data of TLS 1.3.)

module Network.TLS.SessionManager (
    Config(..)
  , defaultConfig
  , newSessionManager
  ) where

import Control.Exception (assert)
import Control.Reaper
import Data.IORef
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as Q
import Network.TLS (SessionID, SessionData, SessionManager(..))
import qualified System.Clock as C

import Network.TLS.Imports

----------------------------------------------------------------

-- | Configuration for session managers.
data Config = Config {
    -- | Ticket lifetime in seconds.
      ticketLifetime :: !Int
    -- | Pruning delay in seconds. This is set to 'reaperDelay'.
    , pruningDelay   :: !Int
    -- | The limit size of session data entries.
    , dbMaxSize      :: !Int
    }

-- | Lifetime: 1 day , delay: 10 minutes, limit: 10,000 entries.
defaultConfig :: Config
defaultConfig = Config {
      ticketLifetime = 86400
    , pruningDelay   = 6000
    , dbMaxSize      = 10000
    }

----------------------------------------------------------------

type Sec = Int64
type Value = (SessionData, IORef Availability)
type DB = OrdPSQ SessionID Sec Value
type Item = (SessionID, Sec, Value, Operation)

data Operation = Add | Del
data Use = SingleUse | MultipleUse
data Availability = Fresh | Used

----------------------------------------------------------------

-- | Creating an in-memory session manager.
newSessionManager :: Config -> IO SessionManager
newSessionManager conf = do
    let lifetime = fromIntegral $ ticketLifetime conf
        maxsiz = dbMaxSize conf
    reaper <- mkReaper defaultReaperSettings {
          reaperEmpty  = Q.empty
        , reaperCons   = cons maxsiz
        , reaperAction = clean
        , reaperNull   = Q.null
        , reaperDelay  = pruningDelay conf * 1000000
        }
    return SessionManager {
        sessionResume     = resume reaper MultipleUse
      , sessionEstablish  = establish reaper lifetime
      , sessionInvalidate = invalidate reaper
      }

cons :: Int -> Item -> DB -> DB
cons lim (k,t,v,Add) db
  | lim <= 0            = Q.empty
  | Q.size db == lim    = case Q.minView db of
      Nothing          -> assert False $ Q.insert k t v Q.empty
      Just (_,_,_,db') -> Q.insert k t v db'
  | otherwise           = Q.insert k t v db
cons _   (k,_,_,Del) db = Q.delete k db

clean :: DB -> IO (DB -> DB)
clean olddb = do
    currentTime <- C.sec <$> C.getTime C.Monotonic
    let !pruned = snd $ Q.atMostView currentTime olddb
    return $ merge pruned
  where
    ins db (k,p,v) = Q.insert k p v db
    -- There is not 'merge' API.
    -- We hope that newdb is smaller than pruned.
    merge pruned newdb = foldl' ins pruned entries
      where
        entries = Q.toList newdb

----------------------------------------------------------------

establish :: Reaper DB Item -> Sec
          -> SessionID -> SessionData -> IO ()
establish reaper lifetime k sd = do
    ref <- newIORef Fresh
    !p <- ((+ lifetime) . C.sec) <$> C.getTime C.Monotonic
    let !v = (sd,ref)
    reaperAdd reaper (k,p,v,Add)

resume :: Reaper DB Item -> Use
       -> SessionID -> IO (Maybe SessionData)
resume reaper use k = do
    db <- reaperRead reaper
    case Q.lookup k db of
      Nothing             -> return Nothing
      Just (p,v@(sd,ref)) -> do
           case use of
               SingleUse -> do
                   available <- atomicModifyIORef' ref check
                   reaperAdd reaper (k,p,v,Del)
                   return $ if available then Just sd else Nothing
               MultipleUse -> return $ Just sd
  where
    check Fresh = (Used,True)
    check Used  = (Used,False)

invalidate :: Reaper DB Item
           -> SessionID -> IO ()
invalidate reaper k = do
    db <- reaperRead reaper
    case Q.lookup k db of
      Nothing    -> return ()
      Just (p,v) -> reaperAdd reaper (k,p,v,Del)
