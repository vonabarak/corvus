{-# LANGUAGE OverloadedStrings #-}

-- | VM process management — daemon-side facade.
--
-- Phase 3: the actual @createProcess@ / signal calls happen on
-- the @corvus-nodeagent@ side. This module marshals intent (which
-- command to run, which PID to stop) and routes through
-- 'Corvus.NodeAgentClient'. The daemon never spawns or signals
-- QEMU directly.
module Corvus.Node.Process
  ( -- * Starting VMs
    StartVmResult (..)
  , startVm

    -- * Stopping VMs
  , KillResult (..)
  , killVmProcess
  )
where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logInfoN, logWarnN)
import qualified Corvus.NetAgentClient as NA
import Corvus.Node.Command (generateQemuCommandWithSockets)
import Corvus.Node.Runtime (getGuestAgentSocket, getMonitorSocket, getQmpSocket, getSerialSocket, getVmRuntimeDir)
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Database.Persist.Postgresql (runSqlPool)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Result of starting a VM
data StartVmResult
  = -- | VM started; carries the agent-reported PID.
    VmStarted !Int
  | -- | VM not found in database
    VmNotFound
  | -- | Error starting VM
    VmStartError !Text
  deriving (Show)

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

-- | Start a VM. Generates the QEMU command from DB rows
-- daemon-side, then asks @corvus-nodeagent@ to spawn the process.
-- The agent owns the resulting child; the daemon learns its PID
-- and stores it on the 'Vm' row.
--
-- @mNetAgent@ is the netd handle threaded through to
-- 'generateQemuCommandWithSockets' so the command builder can ask
-- netd to provision persistent TAPs for managed network
-- interfaces. It is unrelated to nodeagent.
--
-- Returns 'VmStartError' carrying "nodeagent unavailable" when
-- 'ssNodeAgent' is currently 'Nothing'.
startVm
  :: (MonadIO m, MonadLogger m)
  => ServerState
  -> Int64
  -> Maybe NA.NetAgentClient
  -> m StartVmResult
startVm state vmId mNetAgent = do
  let pool = ssDbPool state
      config = ssQemuConfig state

  -- Compute the paths the command builder needs. The agent will
  -- mkdir-p the runtime dir itself before spawning; the daemon
  -- still needs to know the layout because the command line
  -- embeds these paths.
  basePath <- liftIO $ getEffectiveBasePath config
  monitorSock <- liftIO $ getMonitorSocket config vmId
  qmpSock <- liftIO $ getQmpSocket config vmId
  serialSock <- liftIO $ getSerialSocket config vmId
  guestAgentSock <- liftIO $ getGuestAgentSocket config vmId
  vmRuntimeDir <- liftIO $ getVmRuntimeDir config vmId

  mCmd <-
    liftIO $
      runSqlPool
        ( generateQemuCommandWithSockets
            config
            vmId
            basePath
            monitorSock
            qmpSock
            serialSock
            guestAgentSock
            vmRuntimeDir
            mNetAgent
        )
        pool
  case mCmd of
    Nothing -> pure VmNotFound
    Just (binary, args) -> do
      logInfoN $ "Starting VM " <> T.pack (show vmId) <> " via nodeagent:"
      logInfoN $ T.pack $ unwords (binary : args)
      mAgent <- liftIO $ readTVarIO (ssNodeAgent state)
      case mAgent of
        Nothing -> do
          logWarnN $ "nodeagent unavailable; cannot start VM " <> T.pack (show vmId)
          pure $ VmStartError "nodeagent unavailable"
        Just nac -> do
          r <-
            liftIO $
              NOA.processSpawnQemu
                nac
                vmId
                (T.pack binary)
                (map T.pack args)
          case r of
            Left e -> do
              logWarnN $
                "Failed to spawn QEMU for VM "
                  <> T.pack (show vmId)
                  <> ": "
                  <> T.pack (show e)
              pure $ VmStartError (T.pack (show e))
            Right pidW -> do
              logInfoN $
                "VM "
                  <> T.pack (show vmId)
                  <> " started with PID "
                  <> T.pack (show pidW)
              pure $ VmStarted (fromIntegral (pidW :: Word32))

--------------------------------------------------------------------------------
-- Stopping VMs
--------------------------------------------------------------------------------

-- | Stop a running QEMU process by asking the agent to
-- SIGTERM-then-SIGKILL the PID.
--
-- The caller is expected to have already tried higher-level
-- graceful paths — guest agent shutdown or @qmpShutdown@ (ACPI) —
-- before reaching here. This is the bail-out for reset, daemon
-- shutdown, and orphan cleanup.
--
-- Returns 'KillError' carrying "nodeagent unavailable" when the
-- agent connection is currently down.
killVmProcess
  :: (MonadIO m, MonadLogger m) => ServerState -> Int64 -> Int -> m KillResult
killVmProcess state vmId pid = do
  let pidW = fromIntegral pid :: Word32
  mAgent <- liftIO $ readTVarIO (ssNodeAgent state)
  case mAgent of
    Nothing -> do
      logWarnN $
        "nodeagent unavailable; cannot kill VM "
          <> T.pack (show vmId)
          <> " pid="
          <> T.pack (show pid)
      pure $ KillError "nodeagent unavailable"
    Just nac -> do
      r <- liftIO $ NOA.processStop nac pidW 5
      case r of
        Left e ->
          pure $ KillError (T.pack (show e))
        Right NOA.ProcessNotRunning -> pure KillNotRunning
        Right (NOA.ProcessStopFailed err) -> pure $ KillError err
        Right _ -> pure KillSuccess
