{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VM process management.
-- Handles starting and killing QEMU processes.
module Corvus.Qemu.Process
  ( -- * Starting VMs
    StartVmResult (..),
    startVm,

    -- * Killing VMs
    KillResult (..),
    killVmProcess,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN, logInfoN, logWarnN)
import Corvus.Qemu.Command (generateQemuCommandWithSockets)
import Corvus.Qemu.Config (QemuConfig, getEffectiveBasePath)
import Corvus.Qemu.Runtime (createVmRuntimeDir, getMonitorSocket, getQmpSocket, getSpiceSocket, getVmRuntimeDir)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.Types (ProcessID)
import System.Process (ProcessHandle, StdStream (..), createProcess, getPid, proc, std_err, std_out)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Result of starting a VM
data StartVmResult
  = -- | VM started, returns PID and process handle
    VmStarted !Int !ProcessHandle
  | -- | VM not found in database
    VmNotFound
  | -- | Error starting VM
    VmStartError !Text
  deriving (Show)

-- Show instance for ProcessHandle (opaque)
instance Show ProcessHandle where
  show _ = "<ProcessHandle>"

-- | Result of killing a VM process
data KillResult
  = KillSuccess
  | KillNotRunning
  | KillError !Text
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Starting VMs
--------------------------------------------------------------------------------

-- | Start a VM with the given configuration
-- Returns immediately with PID and ProcessHandle (QEMU runs in foreground)
-- The caller is responsible for waiting on the process handle
startVm :: (MonadIO m, MonadLogger m) => Pool SqlBackend -> QemuConfig -> Int64 -> m StartVmResult
startVm pool config vmId = do
  -- Create runtime directory
  _ <- liftIO $ createVmRuntimeDir vmId

  -- Get base path for images
  basePath <- liftIO $ getEffectiveBasePath config

  -- Get socket paths
  monitorSock <- liftIO $ getMonitorSocket vmId
  qmpSock <- liftIO $ getQmpSocket vmId
  spiceSock <- liftIO $ getSpiceSocket vmId
  vmRuntimeDir <- liftIO $ getVmRuntimeDir vmId

  -- Generate command
  mCmd <- liftIO $ runSqlPool (generateQemuCommandWithSockets config vmId basePath monitorSock qmpSock spiceSock vmRuntimeDir) pool
  case mCmd of
    Nothing -> pure VmNotFound
    Just (binary, args) -> do
      -- Log the full command
      logDebugN $ "Starting VM " <> T.pack (show vmId) <> " with command:"
      logDebugN $ T.pack $ unwords (binary : args)

      result <-
        liftIO $
          try $
            createProcess
              (proc binary args)
                { std_out = CreatePipe,
                  std_err = CreatePipe
                }
      case result of
        Left (e :: SomeException) -> do
          logWarnN $ "Failed to spawn QEMU process for VM " <> T.pack (show vmId) <> ": " <> T.pack (show e)
          pure $ VmStartError $ T.pack $ show e
        Right (_, _, _, ph) -> do
          -- Get PID from process handle
          mPid <- liftIO $ getPid ph
          case mPid of
            Just pid -> do
              let pidInt = fromIntegral pid
              logInfoN $ "VM " <> T.pack (show vmId) <> " started with PID " <> T.pack (show pidInt)
              pure $ VmStarted pidInt ph
            Nothing -> do
              logWarnN $ "Failed to get PID for VM " <> T.pack (show vmId)
              pure $ VmStartError "Failed to get process PID"

--------------------------------------------------------------------------------
-- Killing VMs
--------------------------------------------------------------------------------

-- | Kill a VM process by PID (SIGKILL)
killVmProcess :: (MonadIO m, MonadLogger m) => Int64 -> Int -> m KillResult
killVmProcess vmId pid = do
  logDebugN $ "Attempting to kill VM " <> T.pack (show vmId) <> " with PID " <> T.pack (show pid)
  result <- liftIO $ try $ signalProcess sigKILL (fromIntegral pid :: ProcessID)
  case result of
    Left (e :: SomeException) -> do
      let errMsg = T.pack $ show e
      if "does not exist" `T.isInfixOf` errMsg || "No such process" `T.isInfixOf` errMsg
        then do
          logDebugN $ "VM " <> T.pack (show vmId) <> " process " <> T.pack (show pid) <> " not running"
          pure KillNotRunning
        else do
          logWarnN $ "Failed to kill VM " <> T.pack (show vmId) <> ": " <> errMsg
          pure $ KillError errMsg
    Right () -> do
      logInfoN $ "Killed VM " <> T.pack (show vmId) <> " process " <> T.pack (show pid)
      pure KillSuccess
