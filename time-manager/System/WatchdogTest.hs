{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module System.WatchdogTest where

import Data.Word (Word64)
import GHC.Conc (STM, TVar, atomically, newTVarIO, readTVar, writeTVar)

data Watchdog a = Watchdog
    { wdContext :: TVar (ContextFor a)
    , wdHasTimedOut :: TVar Bool
    , wdIsSupervised :: TVar Bool
    }
    deriving (Eq)

newWatchDog :: WatchdogFor a => IO (Watchdog a)
newWatchDog = do
    wdContext <- newTVarIO =<< initialContext
    wdHasTimedOut <- newTVarIO False
    wdIsSupervised <- newTVarIO False
    pure Watchdog{..}

class WatchdogFor a where
    data ContextFor a

    initialContext :: IO (ContextFor a)

    -- | @oldContext -> newContext -> action@
    --
    -- This function is used by the active 'Watchdog' to
    -- figure out what it should do if the context changes.
    --
    -- Here you can describe how to handle the updates you
    -- will make to the context.
    decide :: ContextFor a -> ContextFor a -> IO Action

-- | Updates the context of the provided 'Watchdog'.
--
-- Can be used to, for example:
--
--   * bump a counter
--   * proceed to the following phase
--   * etc.
update :: (ContextFor a -> ContextFor a) -> Watchdog a -> IO ()
update f = atomically . updateSTM f

-- | If you want to update the context in an STM transaction.
updateSTM :: (ContextFor a -> ContextFor a) -> Watchdog a -> STM ()
updateSTM f wd =
    readTVar ctxVar >>= writeTVar ctxVar . f
  where
    ctxVar = wdContext wd

newtype Timeout = TimeoutInUs Word64
    deriving (Eq, Show)

setTimeout :: Word64 -> Action
setTimeout = SetTimeout . TimeoutInUs

data Action
    = Ignore -- doesn't influence the running timeout
    | SetTimeout Timeout -- sets the timeout to the given microseconds
    | Idle -- stops timeout and waits for a new action
    | Abort -- cancels the timeout (can not undo)
    | TimedOut -- immediately triggers the timeout (synonymous with @SetTimeout 0@)

data Http1

data Http1Phase
    = Start
    | AcceptConnection
    | ReadHeaders
    | RunningApp
    | ReadBody
    | WriteResponse
    | Delegated
    deriving (Eq, Show)

rule :: Http1Phase -> Action
rule = \case
    RunningApp -> Idle
    ReadHeaders -> setTimeout $ 3 * 1000 * 1000
    Delegated -> Abort
    _ -> setTimeout $ 10 * 1000 * 1000

instance WatchdogFor Http1 where
    data ContextFor Http1 = Http1Context
        { phase :: Http1Phase
        , tick :: Int
        }

    initialContext =
        pure $
            Http1Context
                { phase = Start
                , tick = 0
                }
    decide oldCtx newCtx
        -- We moved on to the next phase, so we take a new action
        | phase oldCtx /= phase newCtx = pure . rule $ phase newCtx
        -- If the tick moved, we'll check in what phase we are to decide
        -- what to do.
        | tick oldCtx /= tick newCtx =
            case phase newCtx of
                ReadHeaders -> pure Ignore
                ReadBody -> pure $ rule ReadBody
                _ -> undefined
        | otherwise = pure Ignore

-- Few things we have to decide on when starting a Watchdog:
--
--   * You can start a Watchdog, given an initial state (Context)
--   * Given the state, the Watchdog should be able to know when to finish
--   * You should be able to update the state
--   *
