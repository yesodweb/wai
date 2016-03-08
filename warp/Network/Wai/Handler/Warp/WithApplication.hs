
module Network.Wai.Handler.Warp.WithApplication (
  withApplication,
  testWithApplication,
  openFreePort,
) where

import           Control.Concurrent
import           Control.Exception
import           Network.Socket
import           Network.Wai
import           Network.Wai.Handler.Warp.Run
import           Network.Wai.Handler.Warp.Settings
import           Network.Wai.Handler.Warp.Types

data App
  = App {
    appThread :: ThreadId,
    appKilled :: Waiter (),
    appPort :: Int
  }

-- | Runs the given 'Application' on a free port. Passes the port to the given
-- operation and executes it, while the 'Application' is running. Shuts down the
-- server before returning.
withApplication :: IO Application -> (Port -> IO a) -> IO a
withApplication mkApp action = do
  app <- mkApp
  bracket (acquire app) free (\ runningApp -> action (appPort runningApp))
  where
    acquire :: Application -> IO App
    acquire app = do
      start <- mkWaiter
      killed <- mkWaiter
      thread <- forkIO $ do
        (port, sock) <- openFreePort
        let settings =
              defaultSettings{
                settingsBeforeMainLoop = notify start port
              }
        runSettingsSocket settings sock app
          `finally` notify killed ()
      port <- waitFor start
      return $ App thread killed port

    free :: App -> IO ()
    free runningApp = do
      killThread $ appThread runningApp
      waitFor $ appKilled runningApp

-- | Same as 'withApplication' but with different exception handling: If the
-- given 'Application' throws an exception, 'testWithApplication' will re-throw
-- the exception to the calling thread, possibly interrupting the execution of
-- the given operation.
--
-- This is handy for running tests against an 'Application' over a real network
-- port. When running tests, it's useful to let exceptions thrown by your
-- 'Application' propagate to the main thread of the test-suite.
--
-- __The exception handling makes this function unsuitable for use in production.__
-- Use 'withApplication' instead.
testWithApplication :: IO Application -> (Port -> IO a) -> IO a
testWithApplication mkApp action = do
  callingThread <- myThreadId
  app <- mkApp
  let wrappedApp request respond =
        app request respond `catch` \ e -> do
          throwTo callingThread (e :: SomeException)
          throwIO e
  withApplication (return wrappedApp) action

data Waiter a
  = Waiter {
    notify :: a -> IO (),
    waitFor :: IO a
  }

mkWaiter :: IO (Waiter a)
mkWaiter = do
  mvar <- newEmptyMVar
  return $ Waiter {
    notify = putMVar mvar,
    waitFor = readMVar mvar
  }

-- | Opens a socket on a free port and returns both port and socket.
openFreePort :: IO (Port, Socket)
openFreePort = do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  port <- socketPort s
  return (fromIntegral port, s)
