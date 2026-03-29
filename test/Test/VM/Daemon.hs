{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test infrastructure for running the Corvus daemon.
-- Provides functions to start the daemon with a test database
-- and connect to it via RPC for integration testing.
module Test.VM.Daemon
  ( -- * Daemon lifecycle
    TestDaemon (..)
  , startTestDaemon
  , startTestDaemonWithConfig
  , stopTestDaemon
  , withTestDaemon
  , withTestDaemonConfig

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
import Corvus.Qemu.Config (QemuConfig (..), defaultQemuConfig)
import Corvus.Server (runServer)
import Corvus.Types (ListenAddress (..), ServerState (..), newServerState)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Network.Socket (Family (..), SockAddr (..), SocketType (..), close, connect, defaultProtocol, socket)
import qualified Network.Socket as NS
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))
import Test.Database (TestEnv (..), createTestTempDir)
import Test.Settings (getTestLogLevel)

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
      runtimeDir = tempDir </> "run"

  -- Create QEMU config with test-specific paths and no background poller
  -- (healthcheckInterval=0 disables the poller to avoid QGA socket contention)
  let qemuConfig = defaultQemuConfig {qcBasePath = Just qemuBasePath, qcRuntimeDir = Just runtimeDir, qcHealthcheckInterval = 0}
  createDirectoryIfMissing True qemuBasePath
  createDirectoryIfMissing True runtimeDir

  -- Create server state with the test database pool
  logLevel <- getTestLogLevel
  state <- newServerState (tePool env) qemuConfig
  let state' = state {ssLogLevel = logLevel}

  -- Start the server in a background thread
  serverThread <- async $ runServer state' listenAddr

  -- Wait for the socket to be available
  waitForSocket socketPath

  pure
    TestDaemon
      { tdState = state'
      , tdThread = serverThread
      , tdSocketPath = socketPath
      , tdTempDir = tempDir
      }

-- | Start a test daemon with a custom QemuConfig modifier
startTestDaemonWithConfig :: TestEnv -> (QemuConfig -> QemuConfig) -> IO TestDaemon
startTestDaemonWithConfig env modifyConfig = do
  tempDir <- createTestTempDir

  let socketPath = tempDir </> "daemon.sock"
      listenAddr = UnixAddress socketPath
      qemuBasePath = tempDir </> "VMs"
      runtimeDir = tempDir </> "run"

  let qemuConfig = modifyConfig $ defaultQemuConfig {qcBasePath = Just qemuBasePath, qcRuntimeDir = Just runtimeDir, qcHealthcheckInterval = 0}
  createDirectoryIfMissing True qemuBasePath
  createDirectoryIfMissing True runtimeDir

  logLevel <- getTestLogLevel
  state <- newServerState (tePool env) qemuConfig
  let state' = state {ssLogLevel = logLevel}

  serverThread <- async $ runServer state' listenAddr

  waitForSocket socketPath

  pure
    TestDaemon
      { tdState = state'
      , tdThread = serverThread
      , tdSocketPath = socketPath
      , tdTempDir = tempDir
      }

-- | Stop a test daemon.
-- Individual test brackets (withTestVmGuestExecWithDisk etc.) handle VM cleanup.
-- Runtime dirs are per-daemon, so no cross-daemon interference.
stopTestDaemon :: TestDaemon -> IO ()
stopTestDaemon daemon = do
  -- Signal shutdown
  atomically $ writeTVar (ssShutdownFlag (tdState daemon)) True

  -- Cancel the server thread
  cancel (tdThread daemon)

  -- Brief delay to let background threads notice cancellation
  threadDelay 100000

  -- Clean up temp directory (includes per-daemon runtime dir)
  removePathForcibly (tdTempDir daemon)

-- | Run an action with a test daemon
withTestDaemon :: TestEnv -> (TestDaemon -> IO a) -> IO a
withTestDaemon env = bracket (startTestDaemon env) stopTestDaemon

-- | Run an action with a test daemon using a custom QemuConfig modifier
withTestDaemonConfig :: TestEnv -> (QemuConfig -> QemuConfig) -> (TestDaemon -> IO a) -> IO a
withTestDaemonConfig env modifyConfig = bracket (startTestDaemonWithConfig env modifyConfig) stopTestDaemon

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
