{-# LANGUAGE OverloadedStrings #-}

-- | Background VM monitoring and daemon-side (re)attachment.
--
-- Each running VM has a forked supervisor thread ('attachVmMonitor')
-- that polls the node agent for liveness and reconciles the DB row
-- when QEMU exits. 'reattachVmMonitors' restores those threads after
-- a daemon/agent reconnect.
--
-- The per-node autostart replay that runs alongside
-- 'reattachVmMonitors' lives in the umbrella module
-- ('Corvus.Handlers.Vm') because it issues 'VmStart' actions defined
-- there, and this module must not import back into the umbrella.
module Corvus.Handlers.Vm.Monitor
  ( attachVmMonitor
  , reattachVmMonitors
  , reapplyVm
  , releaseManagedTaps
  , pollVmUntilExit
  , ExitOutcome (..)
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.Handlers.Vm.Db
  ( getVmStatusOnly
  , hasNetdMediatedNetIf
  , setVmError
  , setVmStarted
  , setVmStopped
  )
import Corvus.Model (Vm (vmGuestAgent, vmName), VmId, VmStatus (..))
import qualified Corvus.Model as M
import qualified Corvus.NetAgentClient as NA
import qualified Corvus.NetAgentClient.Spec as Spec
import qualified Corvus.NodeAgentClient as NOA
import qualified Corvus.NodeAgentClient.Spec as NSpec
import Corvus.NodeRouting (withVmNetAgent, withVmNodeAgent)
import Corvus.Types (ServerState, lookupNetAgentMaybe, runServerLogging, ssDbPool, ssQemuConfig)
import Data.Int (Int64)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Database.Persist (Entity (..), entityVal, selectList, (<-.), (==.))
import Database.Persist.Sql (fromSqlKey, runSqlPool, toSqlKey)

-- | Tell the agent to drop every netd-allocated TAP attached to
-- the given VM (both managed and bridge NICs). Used by the post-
-- QEMU-exit supervisor thread. Best-effort: errors are logged via
-- the agent client, not propagated to the caller — the VM is
-- gone either way.
releaseManagedTaps :: ServerState -> Int64 -> IO ()
releaseManagedTaps state vmId = do
  let vmKey = toSqlKey vmId :: VmId
  ifaces <-
    runSqlPool
      (selectList [M.NetworkInterfaceVmId ==. vmKey] [])
      (ssDbPool state)
  let netdMediated = filter (isNetdMediated . entityVal) ifaces
      isNetdMediated ni =
        M.networkInterfaceInterfaceType ni == M.NetBridge
          || isJust (M.networkInterfaceNetworkId ni)
  _ <- withVmNetAgent state vmId $ \nac ->
    mapM_
      ( \(Entity ifaceKey _) ->
          let tapName = Spec.corvusTapName (fromSqlKey ifaceKey)
           in void (NA.deleteTap nac tapName)
      )
      netdMediated
  pure ()

-- | Poll the node agent every 1 s for VM liveness via
-- 'NOA.vmStatus'. Returns when the agent reports anything other
-- than 'VmAgentRunning' (stopped / errored / unknown all count as
-- "gone"), or when the agent itself disappears.
pollVmUntilExit :: ServerState -> Int64 -> IO ExitOutcome
pollVmUntilExit state vmId = loop 0
  where
    -- Consecutive non-running reads required before concluding the
    -- VM has exited. A vmStart / reboot-quirk re-spawn briefly drops
    -- the VM from the agent ledger (removeVm → spawn → insertVm).
    -- Now that the agent's lifecycle handlers run concurrently with
    -- status polls (they no longer serialise on the session loop),
    -- this poller can observe that transient gap mid-(re)start —
    -- a lingering monitor from a prior run is especially prone to
    -- catching the next start's window. Debounce so a momentary
    -- "not running" read can't trigger a false 'setVmStopped'; a
    -- genuine exit stays non-running and is concluded after the
    -- confirmation window (~5 s). The monitor is anyway redundant
    -- for operator-driven stops (those flip the row synchronously),
    -- so the small added latency is harmless.
    confirmReads :: Int
    confirmReads = 5
    loop misses = do
      outer <- withVmNodeAgent state vmId $ \nac -> NOA.vmStatus nac vmId
      case outer of
        Left _ -> pure ExitAgentGone
        Right (Left _) -> pure ExitAgentGone
        Right (Right status) -> case NOA.vasState status of
          NOA.VmAgentRunning -> threadDelay 1000000 >> loop 0
          st ->
            let outcome = case st of
                  NOA.VmAgentStopped -> ExitClean
                  NOA.VmAgentErrored ->
                    ExitErrored (fromIntegral (NOA.vasLastExitCode status))
                  _ -> ExitVanished
             in if misses + 1 >= confirmReads
                  then pure outcome
                  else threadDelay 1000000 >> loop (misses + 1)

-- | How a VM's monitor loop concluded. Drives the DB-status
-- reconciliation in 'attachVmMonitor'.
data ExitOutcome
  = ExitClean
  | ExitErrored !Int
  | ExitVanished
  | ExitAgentGone

-- | Fork a background thread that waits for VM @vmId@ to exit on
-- the agent side, then reconciles DB state.
--
-- Called by the VM-start handler right after 'NOA.vmStart' returns,
-- and by 'reattachVmMonitors' for each VM the daemon finds already
-- running when it (re)connects to the agent.
attachVmMonitor :: ServerState -> Int64 -> IO ()
attachVmMonitor state vmId = do
  _ <- forkIO $ runServerLogging state $ do
    logDebugN $ "Polling VM " <> T.pack (show vmId) <> " liveness via nodeagent"
    outcome <- liftIO $ pollVmUntilExit state vmId
    -- Skip reconciliation if a competing handler (e.g.
    -- 'handleVmReset') already committed a terminal status.
    -- Replaces the old "Vm.pid was cleared" signal.
    mStatus <- liftIO $ runSqlPool (getVmStatusOnly vmId) (ssDbPool state)
    case mStatus of
      Nothing ->
        logDebugN $ "VM " <> T.pack (show vmId) <> " was deleted; monitor exiting"
      Just VmStopped ->
        logDebugN $
          "VM "
            <> T.pack (show vmId)
            <> " already marked stopped (likely by reset); skipping status update"
      Just VmError ->
        logDebugN $
          "VM "
            <> T.pack (show vmId)
            <> " already marked error; skipping status update"
      Just VmSaved ->
        logDebugN $
          "VM "
            <> T.pack (show vmId)
            <> " already marked saved; skipping status update"
      Just VmSaving ->
        -- The save executor owns the terminal flip from VmSaving to
        -- VmSaved / VmError; the monitor only watches for unexpected
        -- exits. QMP @quit@ is part of the save flow so an observed
        -- exit here is expected — leave the row alone.
        logDebugN $
          "VM "
            <> T.pack (show vmId)
            <> " is being saved; skipping monitor status update"
      Just VmMigrating ->
        -- The migration orchestrator owns the terminal flip.
        logDebugN $
          "VM "
            <> T.pack (show vmId)
            <> " is being migrated; skipping monitor status update"
      Just _ -> case outcome of
        ExitClean -> do
          logInfoN $ "VM " <> T.pack (show vmId) <> " exited normally"
          liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
        ExitErrored code -> do
          let msg = "QEMU exited with error code " <> T.pack (show code)
          logWarnN $
            "VM "
              <> T.pack (show vmId)
              <> " "
              <> msg
          liftIO $ runSqlPool (setVmError vmId msg) (ssDbPool state)
        ExitVanished -> do
          logInfoN $
            "VM "
              <> T.pack (show vmId)
              <> " no longer in agent ledger; marking stopped"
          liftIO $ runSqlPool (setVmStopped vmId) (ssDbPool state)
        ExitAgentGone ->
          logDebugN $
            "VM "
              <> T.pack (show vmId)
              <> " monitor exiting: agent disconnected"
    -- Tell netd it can drop the VM's managed TAPs.
    liftIO $ releaseManagedTaps state vmId
  pure ()

-- | On daemon (re)connect to the agent: walk the DB for every VM
-- whose intent is "should be running" (status in
-- @{Starting, Running, Paused}@), ask the agent for current
-- status, and reconcile:
--
--   * 'VmAgentRunning' — agent still has the VM; re-attach the
--     monitor thread.
--   * 'VmAgentStopped' / 'VmAgentErrored' — agent observed the
--     exit while the daemon was down; reflect it in the DB.
--   * 'VmAgentUnknown' — agent has no record (e.g. it restarted
--     and reaped the orphan QEMU on startup). Re-issue 'vmStart'
--     to honour the daemon's intent. 'vmStart' is idempotent, so
--     this is also safe if the agent had the VM and we're just
--     catching up.
--
-- Paused VMs lose their pause state across an agent restart —
-- they come back as VmRunning. Documented trade-off; symmetric
-- to "agent restart = VM restart" from the parent plan.
reattachVmMonitors :: ServerState -> IO ()
reattachVmMonitors state = do
  let pool = ssDbPool state
  candidates <-
    runSqlPool
      ( selectList
          [ M.VmStatus
              <-. [VmStarting, VmLoading, VmRunning, VmPaused]
          ]
          []
      )
      pool
  runServerLogging state $
    forM_ candidates $ \(Entity vmKey vm) -> do
      let vmId = fromSqlKey vmKey
      outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> do
        rstat <- NOA.vmStatus nac vmId
        pure (nac, rstat)
      case outer of
        Left err ->
          logDebugN $
            "Skipping reattach for VM " <> vmName vm <> ": " <> err
        Right (nac, r) -> case r of
          Right status -> case NOA.vasState status of
            NOA.VmAgentRunning -> do
              logInfoN $
                "Re-attaching monitor for VM " <> vmName vm
              liftIO $ attachVmMonitor state vmId
            NOA.VmAgentStopped -> do
              logInfoN $
                "VM "
                  <> vmName vm
                  <> " exited cleanly while daemon was disconnected; reconciling"
              liftIO $ runSqlPool (setVmStopped vmId) pool
            NOA.VmAgentErrored -> do
              let msg =
                    "QEMU exited with error code "
                      <> T.pack (show (NOA.vasLastExitCode status))
                      <> " (observed while daemon was disconnected)"
              logWarnN $
                "VM " <> vmName vm <> ": " <> msg
              liftIO $ runSqlPool (setVmError vmId msg) pool
            NOA.VmAgentUnknown -> do
              logInfoN $
                "VM "
                  <> vmName vm
                  <> " not in agent ledger; re-issuing vmStart to honour DB intent"
              reapplyVm state nac vmId vm
          Left e ->
            logWarnN $
              "vmStatus RPC failed for VM "
                <> vmName vm
                <> ": "
                <> T.pack (show e)

-- | Re-issue 'vmStart' for one VM. Assembles 'VmSpec' from the
-- DB (same path 'launchVmViaAgent' uses on a cold start),
-- dispatches, and attaches the monitor on success. On any
-- failure the row lands in 'VmError' — a follow-up @crv vm
-- start@ can recover it.
reapplyVm :: ServerState -> NOA.NodeAgentClient -> Int64 -> Vm -> LoggingT IO ()
reapplyVm state nac vmId vm = do
  let pool = ssDbPool state
      cfg = ssQemuConfig state
  mNetAgent <- liftIO $ lookupNetAgentMaybe state (M.vmNodeId vm)
  needsNetd <- liftIO $ runSqlPool (hasNetdMediatedNetIf vmId) pool
  let netAgentForSpec = if needsNetd then mNetAgent else Nothing
      waitMs =
        if vmGuestAgent vm then 300000 else 0
  mSpec <- liftIO $ NSpec.assembleVmSpec pool cfg netAgentForSpec vmId waitMs
  case mSpec of
    Left err
      | "disappeared from DB" `T.isInfixOf` err -> do
          logWarnN $
            "VM "
              <> vmName vm
              <> " disappeared from DB during reapply; marking stopped"
          liftIO $ runSqlPool (setVmStopped vmId) pool
      | otherwise -> do
          logWarnN $
            "VM " <> vmName vm <> " reapply: assembleVmSpec failed: " <> err
          liftIO $ runSqlPool (setVmError vmId err) pool
    Right spec -> do
      r <- liftIO $ NOA.vmStart nac spec
      case r of
        Right info -> do
          logInfoN $ "VM " <> vmName vm <> " re-applied via vmStart"
          let pid = fromIntegral (NOA.vriQemuPid info) :: Int
          liftIO $ runSqlPool (setVmStarted vmId VmRunning pid) pool
          liftIO $ attachVmMonitor state vmId
        Left e -> do
          let msg = "vmStart reapply: " <> T.pack (show e)
          logWarnN $
            "vmStart reapply failed for VM "
              <> vmName vm
              <> ": "
              <> T.pack (show e)
          liftIO $ runSqlPool (setVmError vmId msg) pool
