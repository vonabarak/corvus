{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VM process management.
-- Handles starting and killing QEMU processes.
module Corvus.Qemu.Process
  ( -- * Starting VMs
    StartVmResult (..)
  , startVm

    -- * Stopping VMs
  , KillResult (..)
  , killVmProcess
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logInfoN, logWarnN)
import Corvus.Process (StopResult (..), stopProcess)
import Corvus.Qemu.Command (generateQemuCommandWithSockets)
import Corvus.Qemu.Config (QemuConfig, getEffectiveBasePath)
import Corvus.Qemu.Runtime (createVmRuntimeDir, getGuestAgentSocket, getMonitorSocket, getQmpSocket, getSerialSocket, getSpiceSocket, getVmRuntimeDir)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend)
import System.IO (Handle)
import System.Process (ProcessHandle, StdStream (..), createProcess, getPid, proc, std_err, std_out)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Result of starting a VM
data StartVmResult
  = -- | VM started, returns PID, process handle, and stderr handle
    VmStarted !Int !ProcessHandle !(Maybe Handle)
  | -- | VM not found in database
    VmNotFound
  | -- | Error starting VM
    VmStartError !Text
  deriving (Show)

-- Show instance for ProcessHandle (opaque)
instance Show ProcessHandle where
  show _ = "<ProcessHandle>"

-- | Result of stopping a VM process.
--
-- Backwards-compatible names preserved from the SIGKILL-only era.
-- 'KillSuccess' now covers any clean termination (SIGTERM-responsive or
-- forced SIGKILL); 'KillError' covers genuine signalling failures.
data KillResult
  = KillSuccess
  | KillNotRunning
  | KillError !Text
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Starting VMs
--------------------------------------------------------------------------------

-- | Start a VM with the given configuration.
-- Returns immediately with PID and ProcessHandle (QEMU runs in foreground).
-- The caller is responsible for waiting on the process handle.
--
-- When mNamespacePid is Just, TAP file descriptors are created inside the
-- namespace for managed network interfaces. QEMU itself always runs in the
-- host namespace (so user-mode networking, SPICE, etc. remain accessible).
startVm :: (MonadIO m, MonadLogger m) => Pool SqlBackend -> QemuConfig -> Int64 -> Maybe Int -> m StartVmResult
startVm pool config vmId mNamespacePid = do
  -- Create runtime directory
  _ <- liftIO $ createVmRuntimeDir config vmId

  -- Get base path for images
  basePath <- liftIO $ getEffectiveBasePath config

  -- Get socket paths
  monitorSock <- liftIO $ getMonitorSocket config vmId
  qmpSock <- liftIO $ getQmpSocket config vmId
  spiceSock <- liftIO $ getSpiceSocket config vmId
  serialSock <- liftIO $ getSerialSocket config vmId
  guestAgentSock <- liftIO $ getGuestAgentSocket config vmId
  vmRuntimeDir <- liftIO $ getVmRuntimeDir config vmId

  -- Generate command (TAP fds are created inside namespace during resolution)
  mCmd <- liftIO $ runSqlPool (generateQemuCommandWithSockets config vmId basePath monitorSock qmpSock spiceSock serialSock guestAgentSock vmRuntimeDir mNamespacePid) pool
  case mCmd of
    Nothing -> pure VmNotFound
    Just (binary, args) -> do
      -- Log the full command
      logInfoN $ "Starting VM " <> T.pack (show vmId) <> " with command:"
      logInfoN $ T.pack $ unwords (binary : args)

      result <-
        liftIO $
          try $
            createProcess
              (proc binary args)
                { std_out = CreatePipe
                , std_err = CreatePipe
                }
      case result of
        Left (e :: SomeException) -> do
          logWarnN $ "Failed to spawn QEMU process for VM " <> T.pack (show vmId) <> ": " <> T.pack (show e)
          pure $ VmStartError $ T.pack $ show e
        Right (_, _, mStderr, ph) -> do
          mPid <- liftIO $ getPid ph
          case mPid of
            Just pid -> do
              let pidInt = fromIntegral pid
              logInfoN $ "VM " <> T.pack (show vmId) <> " started with PID " <> T.pack (show pidInt)
              pure $ VmStarted pidInt ph mStderr
            Nothing -> do
              logWarnN $ "Failed to get PID for VM " <> T.pack (show vmId)
              pure $ VmStartError "Failed to get process PID"

--------------------------------------------------------------------------------
-- Stopping VMs
--------------------------------------------------------------------------------

-- | Stop a running QEMU process.
--
-- Sends @SIGTERM@ first to give QEMU a chance to flush qcow2 writes and
-- exit cleanly; escalates to @SIGKILL@ if the process is still alive after
-- a short grace period (see 'Corvus.Process.stopProcess').
--
-- The caller is expected to have already tried higher-level graceful paths
-- — guest agent shutdown or @qmpShutdown@ (ACPI) — before reaching here.
-- This function is the bail-out for reset, daemon shutdown, and orphan
-- cleanup: the VM isn't responding, just get it off the host.
killVmProcess :: (MonadIO m, MonadLogger m) => Int64 -> Int -> m KillResult
killVmProcess vmId pid = do
  let name = "qemu(vm=" <> T.pack (show vmId) <> ")"
  result <- stopProcess name (fromIntegral pid) Nothing 0 5
  case result of
    StoppedByTerm -> pure KillSuccess
    StoppedByKill -> pure KillSuccess
    StoppedGracefully -> pure KillSuccess
    NotRunning -> pure KillNotRunning
    StopFailed err -> pure $ KillError err
