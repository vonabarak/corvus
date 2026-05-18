{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Daemon lifecycle handlers: startup and graceful shutdown.
-- Networks are managed by the corvus-netd agent — the daemon's
-- role here is restricted to draining stale DB state and asking
-- the agent to (re)apply / tear down running networks.
module Corvus.Handlers.Lifecycle
  ( -- * Action types
    Startup (..)
  , GracefulShutdown (..)
  )
where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Network (NetworkStart (..))
import Corvus.Handlers.Vm (VmStart (..))
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu.Process (killVmProcess)
import Corvus.Qemu.Virtiofsd (killVirtiofsdProcesses)
import Corvus.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addUTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (fromSqlKey)

--------------------------------------------------------------------------------
-- Startup Action
--------------------------------------------------------------------------------

-- | Daemon startup: clean stale state, start namespace, autostart networks and VMs.
newtype Startup = Startup {startupRetentionDays :: Int}

instance Action Startup where
  actionSubsystem _ = SubSystem
  actionCommand _ = "startup"
  actionExecute ctx (Startup retentionDays) = do
    let state = acState ctx
        taskKey = acTaskId ctx
        pool = ssDbPool state
    now <- getCurrentTime

    runServerLogging state $ do
      -- Mark stale "running" and "not_started" tasks as error (daemon crashed while processing)
      liftIO $ runSqlPool (updateWhere [TaskResult <-. [TaskRunning, TaskNotStarted], TaskId !=. taskKey] [TaskResult =. TaskError, TaskMessage =. Just "Daemon restarted"]) pool
      logInfoN "Marked stale running tasks as error"

      -- Kill orphaned QEMU processes and reset stale VMs
      staleVms <- liftIO $ runSqlPool (selectList [M.VmStatus <-. [VmStarting, VmRunning, VmStopping, VmPaused]] []) pool
      liftIO $ forM_ staleVms $ \(Entity vmKey vm) -> do
        case vmPid vm of
          Just pid -> runServerLogging state $ do
            logInfoN $ "Killing orphaned QEMU process for VM " <> vmName vm <> " (PID " <> T.pack (show pid) <> ")"
            _ <- killVmProcess (fromSqlKey vmKey) pid
            killVirtiofsdProcesses pool (fromSqlKey vmKey)
          Nothing -> pure ()
      liftIO $ runSqlPool (updateWhere [M.VmStatus <-. [VmStarting, VmRunning, VmStopping, VmPaused]] [M.VmStatus =. VmError, M.VmPid =. Nothing, M.VmHealthcheck =. Nothing, M.VmSpicePort =. Nothing]) pool
      logInfoN "Reset stale VMs to error state"

      -- Don't reset NetworkRunning rows here. The agent owns
      -- kernel state independently of this daemon process, so
      -- a daemon restart finds the bridge / dnsmasq still alive.
      -- The connect-and-hold async in app/daemon/Main.hs calls
      -- reapplyRunningNetworks on every (re)connect; its
      -- applyNetwork is idempotent, so a re-apply over already-
      -- live kernel state is a no-op.
      mAgent <- liftIO $ readTVarIO (ssNetAgent state)
      case mAgent of
        Nothing -> logWarnN "corvus-netd not yet connected; networking will retry once agent is up"
        Just _ -> logInfoN "corvus-netd connection ready"

      -- Autostart networks (before VMs, since VMs may depend on networks)
      autostartNetworks <- liftIO $ runSqlPool (selectList [M.NetworkAutostart ==. True] [Asc M.NetworkName]) pool
      unless (null autostartNetworks) $ do
        logInfoN $ "Autostarting " <> T.pack (show (length autostartNetworks)) <> " network(s)"
        liftIO $ forM_ autostartNetworks $ \(Entity nwKey nw) -> do
          let nwId = fromSqlKey nwKey
          nwResp <- runActionAsSubtask state (NetworkStart nwId) taskKey
          runServerLogging state $ case classifyResponse nwResp of
            (TaskError, Just err) -> logWarnN $ "Failed to autostart network " <> networkName nw <> ": " <> err
            _ -> logInfoN $ "Autostarted network " <> networkName nw

      -- Autostart VMs (after networks are up)
      autostartVms <- liftIO $ runSqlPool (selectList [M.VmAutostart ==. True] [Asc M.VmName]) pool
      unless (null autostartVms) $ do
        logInfoN $ "Autostarting " <> T.pack (show (length autostartVms)) <> " VM(s)"
        liftIO $ forM_ autostartVms $ \(Entity vmKey vm) -> do
          let vmId = fromSqlKey vmKey
          -- Only autostart VMs that are stopped
          when (vmStatus vm == VmStopped) $ do
            -- Reset error state VMs to stopped first
            when (vmStatus vm == VmError) $
              runSqlPool (update vmKey [M.VmStatus =. VmStopped]) pool
            vmResp <- runActionAsSubtask state (VmStart vmId) taskKey
            runServerLogging state $ case classifyResponse vmResp of
              (TaskError, Just err) -> logWarnN $ "Failed to autostart VM " <> vmName vm <> ": " <> err
              _ -> logInfoN $ "Autostarted VM " <> vmName vm

      -- Delete old task entries
      when (retentionDays > 0) $ do
        let cutoff = addUTCTime (fromIntegral $ negate $ retentionDays * 86400) now
        liftIO $ runSqlPool (deleteWhere [TaskStartedAt <. cutoff]) pool
        logInfoN $ "Cleaned up tasks older than " <> T.pack (show retentionDays) <> " days"

    pure RespStartupComplete

--------------------------------------------------------------------------------
-- Graceful Shutdown Action
--------------------------------------------------------------------------------

-- | Daemon shutdown: stop all running VMs and networks, teardown namespace.
data GracefulShutdown = GracefulShutdown

instance Action GracefulShutdown where
  actionSubsystem _ = SubSystem
  actionCommand _ = "shutdown"
  actionExecute ctx GracefulShutdown = do
    let state = acState ctx
        taskKey = acTaskId ctx
        pool = ssDbPool state

    runServerLogging state $ do
      -- Stop all running/starting VMs
      runningVms <- liftIO $ runSqlPool (selectList [M.VmStatus <-. [VmStarting, VmRunning, VmPaused]] []) pool
      logInfoN $ "Stopping " <> T.pack (show (length runningVms)) <> " running VM(s)"
      liftIO $ forM_ runningVms $ \(Entity vmKey vm) -> do
        case vmPid vm of
          Just pid -> runServerLogging state $ do
            logInfoN $ "Killing VM " <> vmName vm <> " (PID " <> T.pack (show pid) <> ")"
            -- Clear PID first (so process monitor thread doesn't interfere)
            liftIO $ runSqlPool (update vmKey [M.VmPid =. Nothing]) pool
            _ <- killVmProcess (fromSqlKey vmKey) pid
            killVirtiofsdProcesses pool (fromSqlKey vmKey)
            liftIO $ runSqlPool (update vmKey [M.VmStatus =. VmStopped, M.VmHealthcheck =. Nothing]) pool
          Nothing -> pure ()
      -- Also handle VMs in Stopping state (QEMU still running)
      stoppingVms <- liftIO $ runSqlPool (selectList [M.VmStatus ==. VmStopping] []) pool
      liftIO $ forM_ stoppingVms $ \(Entity vmKey vm) -> do
        case vmPid vm of
          Just pid -> runServerLogging state $ do
            logInfoN $ "Force-killing stopping VM " <> vmName vm <> " (PID " <> T.pack (show pid) <> ")"
            liftIO $ runSqlPool (update vmKey [M.VmPid =. Nothing]) pool
            _ <- killVmProcess (fromSqlKey vmKey) pid
            killVirtiofsdProcesses pool (fromSqlKey vmKey)
            liftIO $ runSqlPool (update vmKey [M.VmStatus =. VmStopped, M.VmHealthcheck =. Nothing]) pool
          Nothing -> pure ()

      -- Networks are NOT torn down here. The agent owns kernel
      -- state independently of this daemon's lifecycle, so a
      -- daemon stop (or restart) leaves bridges + dnsmasq alive.
      -- An admin who wants a network gone calls `crv network
      -- stop` explicitly, or stops corvus-netd.service.
      runningNetworks <- liftIO $ runSqlPool (selectList [M.NetworkRunning ==. True] []) pool
      logInfoN $ "Leaving " <> T.pack (show (length runningNetworks)) <> " running network(s) under the agent's care"

      -- Mark any remaining running tasks as error
      liftIO $ runSqlPool (updateWhere [TaskResult <-. [TaskRunning, TaskNotStarted], TaskId !=. taskKey] [TaskResult =. TaskError, TaskMessage =. Just "Daemon shutting down"]) pool
      logInfoN "Graceful shutdown complete"

    pure RespShutdownComplete
