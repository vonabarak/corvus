{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test infrastructure for running the Corvus daemon.
-- Provides functions to start the daemon with a test database
-- and connect to it via RPC for integration testing.
module Test.VM.Daemon
  ( -- * Daemon lifecycle
    TestDaemon (..)
  , startTestDaemon
  , stopTestDaemon
  , withTestDaemon

    -- * RPC client helpers
  , withDaemonConnection
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (atomically, writeTVar)
import Control.Exception (bracket, catch)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Corvus.Client.Connection (Connection, ConnectionError, withConnection)
import Corvus.Qemu.Config (defaultQemuConfig, qcBasePath)
import Corvus.Server (runServer)
import Corvus.Types (ListenAddress (..), ServerState (..), newServerState)
import Data.Pool (Pool)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist.Sql (SqlBackend)
import Network.Socket (Family (..), SockAddr (..), SocketType (..), close, connect, defaultProtocol, socket)
import qualified Network.Socket as NS
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))
import Test.Database (TestEnv (..), createTestTempDir)

--------------------------------------------------------------------------------
-- Test Daemon Types
--------------------------------------------------------------------------------

-- | A running test daemon instance
data TestDaemon = TestDaemon
  { tdState :: !ServerState
  -- ^ Server state (for direct access if needed)
  , tdThread :: !(Async ())
  -- ^ Server thread
  , tdSocketPath :: !FilePath
  -- ^ Socket path for client connections
  , tdTempDir :: !FilePath
  -- ^ Temporary directory (for cleanup)
  }

--------------------------------------------------------------------------------
-- Daemon Lifecycle
--------------------------------------------------------------------------------

-- | Start a test daemon with the given test environment
startTestDaemon :: TestEnv -> IO TestDaemon
startTestDaemon env = do
  -- Create temporary directory for the daemon socket
  tempDir <- createTestTempDir

  let socketPath = tempDir </> "daemon.sock"
      listenAddr = UnixAddress socketPath
      qemuBasePath = tempDir </> "VMs"

  -- Create QEMU config with test-specific base path
  let qemuConfig = defaultQemuConfig {qcBasePath = Just qemuBasePath}
  createDirectoryIfMissing True qemuBasePath

  -- Create server state with the test database pool
  state <- newServerState (tePool env) qemuConfig

  -- Start the server in a background thread
  serverThread <- async $ runServer state listenAddr

  -- Wait for the socket to be available
  waitForSocket socketPath

  pure
    TestDaemon
      { tdState = state
      , tdThread = serverThread
      , tdSocketPath = socketPath
      , tdTempDir = tempDir
      }

-- | Stop a test daemon
stopTestDaemon :: TestDaemon -> IO ()
stopTestDaemon daemon = do
  -- Signal shutdown
  atomically $ writeTVar (ssShutdownFlag (tdState daemon)) True

  -- Cancel the server thread
  cancel (tdThread daemon)

  -- Clean up temp directory
  removePathForcibly (tdTempDir daemon)

-- | Run an action with a test daemon
withTestDaemon :: TestEnv -> (TestDaemon -> IO a) -> IO a
withTestDaemon env = bracket (startTestDaemon env) stopTestDaemon

--------------------------------------------------------------------------------
-- RPC Client Helpers
--------------------------------------------------------------------------------

-- | Run an action with a connection to the test daemon
withDaemonConnection
  :: TestDaemon
  -> (Connection -> IO a)
  -> IO (Either ConnectionError a)
withDaemonConnection daemon =
  withConnection (UnixAddress (tdSocketPath daemon))

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Wait for a Unix socket to become available
waitForSocket :: FilePath -> IO ()
waitForSocket path = go 50
  where
    go 0 = error $ "Timeout waiting for daemon socket: " <> path
    go n = do
      available <- checkSocketAvailable path
      unless available $ do
        threadDelay 100000 -- 100ms
        go (n - 1)

-- | Check if a Unix socket is available for connection
checkSocketAvailable :: FilePath -> IO Bool
checkSocketAvailable path = do
  result <- tryConnect
  case result of
    Left _ -> pure False
    Right _ -> pure True
  where
    tryConnect = do
      sock <- socket AF_UNIX Stream defaultProtocol
      (connect sock (SockAddrUnix path) >> close sock >> pure (Right ()))
        `catch` (\(_ :: IOError) -> close sock >> pure (Left ()))
