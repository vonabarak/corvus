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
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Exception (SomeException, bracket, catch, try)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Corvus.Client.Connection (Connection, ConnectionError, withConnection)
import Corvus.Qemu.Config (QemuConfig (..), defaultQemuConfig)
import Corvus.Qemu.Netns (startNamespace)
import Corvus.Server (runServer)
import Corvus.Types (ListenAddress (..), ServerState (..), newServerState)
import qualified Data.Text as T
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Network.Socket (Family (..), SockAddr (..), SocketType (..), close, connect, defaultProtocol, socket)
import qualified Network.Socket as NS
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))
import System.Posix.Process (ProcessStatus, getProcessStatus)
import System.Posix.Signals (sigTERM, signalProcess)
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

  -- Start the network namespace (don't call handleStartup — it would
  -- kill VMs from parallel tests sharing the same database)
  startTestNamespace state'

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

  startTestNamespace state'

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

  -- Kill namespace manager if running
  stopTestNamespace (tdState daemon)

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

-- | Run an action with a connection to the test daemon.
-- Retries up to 3 times on transient connection failures (resource exhausted)
-- which can occur when many tests run in parallel.
withDaemonConnection
  :: TestDaemon
  -> (Connection -> IO a)
  -> IO (Either ConnectionError a)
withDaemonConnection daemon action = go (3 :: Int)
  where
    go 0 = withConnection (UnixAddress (tdSocketPath daemon)) action
    go n = do
      result <- withConnection (UnixAddress (tdSocketPath daemon)) action
      case result of
        Left _ -> do
          threadDelay 200000 -- 200ms backoff
          go (n - 1)
        Right _ -> pure result

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

-- | Start the network namespace for a test daemon.
-- Only creates the namespace — does NOT run handleStartup which would
-- interfere with parallel tests sharing the same database.
startTestNamespace :: ServerState -> IO ()
startTestNamespace state = do
  nsResult <- startNamespace
  case nsResult of
    Left _ -> pure () -- Namespace creation failed, tests without networking will still work
    Right nsPid ->
      atomically $ writeTVar (ssNamespacePid state) (Just (fromIntegral nsPid))

-- | Stop the network namespace for a test daemon.
stopTestNamespace :: ServerState -> IO ()
stopTestNamespace state = do
  mNsPid <- readTVarIO (ssNamespacePid state)
  case mNsPid of
    Just nsPid -> do
      _ <- try $ signalProcess sigTERM (fromIntegral nsPid) :: IO (Either SomeException ())
      _ <- try $ getProcessStatus True False (fromIntegral nsPid) :: IO (Either SomeException (Maybe ProcessStatus))
      atomically $ writeTVar (ssNamespacePid state) Nothing
    Nothing -> pure ()
