{-# LANGUAGE BangPatterns #-}

-- | In-memory TLS session manager.
--
-- * Limitation: you can set the maximum size of the session data database.
-- * Automatic pruning: old session data over their lifetime are pruned automatically.
-- * Replay resistance: each session data is used at most once to prevent replay attacks.
-- * Energy saving: no dedicate pruning thread is running when the size of session database is zero.

module Network.TLS.SessionManager (
    Config(..)
  , defaultConfig
  , newSessionManager
  ) where

import Control.Reaper
import Data.IORef
import Data.List (foldl')
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as Q
import Data.Time (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)
import Network.TLS (SessionID, SessionData, SessionManager(..))

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

type Value = (SessionData, IORef Availability)
type DB = OrdPSQ SessionID UTCTime Value
type Item = (SessionID, UTCTime, Value, Operation)

data Operation = Add | Del
data Availability = Fresh | Used

----------------------------------------------------------------

-- | Creating a in-memory session manager.
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
        sessionResume     = resume reaper
      , sessionEstablish  = establish reaper lifetime
      , sessionInvalidate = invalidate reaper
      }

cons :: Int -> Item -> DB -> DB
cons lim (k,t,v,Add) db
  | Q.size db == lim    = case Q.minView db of
      Nothing          -> Q.insert k t v Q.empty -- not happens, just in case
      Just (_,_,_,db') -> Q.insert k t v db'
  | otherwise           = Q.insert k t v db
cons _   (k,_,_,Del) db = Q.delete k db

clean :: DB -> IO (DB -> DB)
clean olddb = do
    currentTime <- getCurrentTime
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

establish :: Reaper DB Item -> NominalDiffTime
          -> SessionID -> SessionData -> IO ()
establish reaper lifetime k sd = do
    ref <- newIORef Fresh
    !p <- addUTCTime lifetime <$> getCurrentTime
    let !v = (sd,ref)
    reaperAdd reaper (k,p,v,Add)

resume :: Reaper DB Item
       -> SessionID -> IO (Maybe SessionData)
resume reaper k = do
    db <- reaperRead reaper
    case Q.lookup k db of
      Nothing             -> return Nothing
      Just (p,v@(sd,ref)) -> do
          available <- atomicModifyIORef' ref check
          reaperAdd reaper (k,p,v,Del)
          return $ if available then Just sd else Nothing
  where
    check Fresh = (Used,True)
    check Used  = (Used,False)

invalidate :: Reaper DB Item
           -> SessionID -> IO ()
invalidate reaper k = do
    -- repaerDelete does not exist
    -- So, let's set the entry used and hope that it will be
    -- cleaned in the future.
    db <- reaperRead reaper
    case Q.lookup k db of
      Nothing          -> return ()
      Just (_,(_,ref)) -> atomicModifyIORef' ref $ \_ -> (Used, ())
