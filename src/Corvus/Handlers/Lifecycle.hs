{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Daemon lifecycle handlers: startup and graceful shutdown.
--
-- Both networks (corvus-netd) and VMs / virtiofsd
-- (corvus-nodeagent) are managed by external agents now. The
-- daemon's role at startup and shutdown is restricted to
-- draining stale task rows and logging what the agents are
-- still keeping alive on the host. Process reaping is the
-- agent's own startup-cleanup pass.
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
import Corvus.Types
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
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

      -- Don't kill or reset VMs. corvus-nodeagent owns the
      -- QEMU + virtiofsd subprocesses independently of this
      -- daemon's lifecycle. The agent's own startup pass reaps
      -- any genuinely-orphaned processes (and scrubs runtime
      -- dirs); the daemon's reconnect-and-reapply loop in
      -- 'runNodeAgentConnection' will sync DB intent against
      -- what's actually alive once the connection comes up.
      --
      -- Specifically: if the daemon crashed mid-task, its VMs
      -- are typically still alive on the agent side, and the
      -- PIDs / SPICE-port / healthcheck rows in the DB are
      -- still valid. Wiping them would force a needless restart.
      runningVms <- liftIO $ runSqlPool (selectList [M.VmStatus <-. [VmStarting, VmRunning, VmStopping, VmPaused]] []) pool
      logInfoN $
        "Leaving "
          <> T.pack (show (length runningVms))
          <> " running/transitioning VM(s) under the agent's care"

      -- Don't reset NetworkRunning rows here. The agent owns
      -- kernel state independently of this daemon process, so
      -- a daemon restart finds the bridge / dnsmasq still alive.
      -- The connect-and-hold async in app/daemon/Main.hs calls
      -- reapplyRunningNetworks on every (re)connect; its
      -- applyNetwork is idempotent, so a re-apply over already-
      -- live kernel state is a no-op.
      agents <- liftIO $ readTVarIO (ssAgents state)
      let anyNetdLive = any (isJust . ncNetAgent) (Map.elems agents)
      if anyNetdLive
        then logInfoN "corvus-netd connection ready (one or more nodes)"
        else logWarnN "corvus-netd not yet connected; networking will retry once agent is up"

      -- Autostart networks (before VMs, since VMs may depend on networks)
      autostartNetworks <- liftIO $ runSqlPool (selectList [M.NetworkAutostart ==. True] [Asc M.NetworkName]) pool
      unless (null autostartNetworks) $ do
        logInfoN $ "Autostarting " <> T.pack (show (length autostartNetworks)) <> " network(s)"
        liftIO $ forM_ autostartNetworks $ \(Entity nwKey nw) -> do
          let nwId = fromSqlKey nwKey
          nwResp <- runActionAsSubtask ctx (NetworkStart nwId)
          runServerLogging state $ case classifyResponse nwResp of
            (TaskError, Just err) -> logWarnN $ "Failed to autostart network " <> networkName nw <> ": " <> err
            _ -> logInfoN $ "Autostarted network " <> networkName nw

      -- Autostart VMs (after networks are up)
      autostartVms <- liftIO $ runSqlPool (selectList [M.VmAutostart ==. True] [Asc M.VmName]) pool
      unless (null autostartVms) $ do
        logInfoN $ "Autostarting " <> T.pack (show (length autostartVms)) <> " VM(s)"
        liftIO $ forM_ autostartVms $ \(Entity vmKey vm) -> do
          let vmId = fromSqlKey vmKey
          -- Only autostart VMs that are stopped or saved. VmStart on a
          -- VmSaved row resumes from the saved-state file (the same verb
          -- handles both cold boot and resume).
          when (vmStatus vm `elem` [VmStopped, VmSaved]) $ do
            -- Reset error state VMs to stopped first
            when (vmStatus vm == VmError) $
              runSqlPool (update vmKey [M.VmStatus =. VmStopped]) pool
            vmResp <- runActionAsSubtask ctx (VmStart vmId)
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
      -- VMs are NOT torn down here. corvus-nodeagent owns
      -- QEMU + virtiofsd subprocesses independently of this
      -- daemon's lifecycle, so a daemon stop (or restart)
      -- leaves them alive on the host. The next daemon
      -- startup will re-attach and reconcile DB intent
      -- against the agent's runtime state.
      runningVms <- liftIO $ runSqlPool (selectList [M.VmStatus <-. [VmStarting, VmRunning, VmStopping, VmPaused]] []) pool
      logInfoN $
        "Leaving "
          <> T.pack (show (length runningVms))
          <> " running/transitioning VM(s) under the agent's care"

      -- Same for networks: corvus-netd owns kernel state.
      runningNetworks <- liftIO $ runSqlPool (selectList [M.NetworkRunning ==. True] []) pool
      logInfoN $
        "Leaving "
          <> T.pack (show (length runningNetworks))
          <> " running network(s) under the agent's care"

      -- Mark any remaining running tasks as error
      liftIO $ runSqlPool (updateWhere [TaskResult <-. [TaskRunning, TaskNotStarted], TaskId !=. taskKey] [TaskResult =. TaskError, TaskMessage =. Just "Daemon shutting down"]) pool
      logInfoN "Graceful shutdown complete"

    pure RespShutdownComplete
