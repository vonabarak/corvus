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
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.Handlers.Vm.Db
  ( completeVmMonitor
  , completeVmStop
  , hasNetdMediatedNetIf
  , setVmErrorIfCurrent
  , setVmStartedIfCurrent
  )
import Corvus.Model (Vm (vmName, vmStatus), VmId, VmStatus (..))
import qualified Corvus.Model as M
import qualified Corvus.NetAgentClient as NA
import qualified Corvus.NetAgentClient.Spec as Spec
import qualified Corvus.NodeAgentClient as NOA
import qualified Corvus.NodeAgentClient.Spec as NSpec
import Corvus.NodeRouting (withVmNetAgent, withVmNodeAgent)
import Corvus.Types (ServerState, lookupNetAgentMaybe, runServerLogging, ssDbPool, ssQemuConfig)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Database.Persist (Entity (..), entityVal, get, selectList, (<-.), (==.))
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
pollVmUntilExit :: ServerState -> Int64 -> Vm -> IO ExitOutcome
pollVmUntilExit state vmId observed = loop 0
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
    -- Stop watching as soon as another lifecycle operation takes ownership.
    -- Debouncing alone cannot distinguish an old run from a new start.
    loop misses = do
      current <- runSqlPool (get (toSqlKey vmId :: VmId)) (ssDbPool state)
      case current of
        Just vm
          | M.vmLifecycleRevision vm == M.vmLifecycleRevision observed
          , M.vmRuntimeGeneration vm == M.vmRuntimeGeneration observed
          , M.vmNodeId vm == M.vmNodeId observed ->
              poll misses
        _ -> pure ExitSuperseded
    poll misses = do
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
  | ExitSuperseded

-- | Fork a background thread that waits for VM @vmId@ to exit on
-- the agent side, then reconciles DB state.
--
-- Called by the VM-start handler right after 'NOA.vmStart' returns,
-- and by 'reattachVmMonitors' for each VM the daemon finds already
-- running when it (re)connects to the agent.
attachVmMonitor :: ServerState -> Int64 -> IO ()
attachVmMonitor state vmId = do
  -- Capture ownership before forking: the thread may not run until after a
  -- stop/migrate/start has already replaced the runtime it was attached for.
  observed <- runSqlPool (get (toSqlKey vmId :: VmId)) (ssDbPool state)
  forM_ observed $ \vm -> void $ forkIO $ runServerLogging state $ do
    logDebugN $ "Polling VM " <> T.pack (show vmId) <> " liveness via nodeagent"
    outcome <- liftIO $ pollVmUntilExit state vmId vm
    case outcome of
      ExitAgentGone -> logDebugN "VM monitor exiting: agent disconnected"
      ExitSuperseded -> logDebugN "VM monitor exiting: runtime ownership changed"
      _ -> do
        let reason = case outcome of
              ExitErrored code -> Just ("QEMU exited with error code " <> T.pack (show code))
              _ -> Nothing
        -- Recheck ownership atomically with the update: a lifecycle change
        -- can also happen while the final nodeagent request is in flight.
        committed <- liftIO $ runSqlPool (completeVmMonitor vmId vm reason) (ssDbPool state)
        when committed $ do
          logInfoN $ "VM " <> T.pack (show vmId) <> " exited; reconciled monitor status"
          liftIO $ releaseManagedTaps state vmId

-- | On daemon (re)connect to the agent: walk the DB for every VM
-- that might still have QEMU alive (status in
-- @{Starting, Loading, Running, Paused, Error}@), ask the agent for current
-- status, and reconcile:
--
--   * 'VmAgentRunning' — agent still has the VM; re-attach the
--     monitor thread.
--   * 'VmAgentStopped' / 'VmAgentErrored' — agent observed the
--     exit while the daemon was down; reflect it in the DB.
--   * 'VmAgentUnknown' — agent has no record (e.g. it restarted
--     and reaped the orphan QEMU on startup). Re-issue 'vmStart'
--     only for a non-error VM; an errored VM needs explicit reset.
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
              <-. [VmStarting, VmLoading, VmRunning, VmPaused, VmStopping, VmError]
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
          Right status | vmStatus vm == VmStopping -> do
            let reason = Just "Stop was interrupted by a connection loss; reset is required to confirm cleanup"
            _ <- liftIO $ runSqlPool (completeVmStop vmId (M.vmLifecycleRevision vm) (M.vmRuntimeGeneration vm) reason) pool
            logWarnN $ "VM " <> vmName vm <> ": interrupted stop retained for recovery (agent state " <> T.pack (show (NOA.vasState status)) <> ")"
          Right status -> case NOA.vasState status of
            NOA.VmAgentRunning -> do
              logInfoN $
                "Re-attaching monitor for VM " <> vmName vm
              liftIO $ attachVmMonitor state vmId
            NOA.VmAgentStopped -> do
              when (vmStatus vm /= VmError) $ do
                logInfoN $
                  "VM "
                    <> vmName vm
                    <> " exited cleanly while daemon was disconnected; reconciling"
                committed <- liftIO $ runSqlPool (completeVmMonitor vmId vm Nothing) pool
                when committed $ liftIO $ releaseManagedTaps state vmId
            NOA.VmAgentErrored -> do
              when (vmStatus vm /= VmError) $ do
                let msg =
                      "QEMU exited with error code "
                        <> T.pack (show (NOA.vasLastExitCode status))
                        <> " (observed while daemon was disconnected)"
                logWarnN $
                  "VM " <> vmName vm <> ": " <> msg
                committed <- liftIO $ runSqlPool (completeVmMonitor vmId vm (Just msg)) pool
                when committed $ liftIO $ releaseManagedTaps state vmId
            NOA.VmAgentUnknown -> do
              when (vmStatus vm /= VmError) $ do
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
      revision = M.vmLifecycleRevision vm
      generation = fromMaybe revision (M.vmRuntimeGeneration vm)
      failReapply message = void $ liftIO $ runSqlPool (setVmErrorIfCurrent vmId revision generation message) pool
  mNetAgent <- liftIO $ lookupNetAgentMaybe state (M.vmNodeId vm)
  needsNetd <- liftIO $ runSqlPool (hasNetdMediatedNetIf vmId) pool
  let netAgentForSpec = if needsNetd then mNetAgent else Nothing
  mSpec <- liftIO $ NSpec.assembleVmSpec pool cfg netAgentForSpec vmId (M.vmLifecycleRevision vm) (fromMaybe (M.vmLifecycleRevision vm) (M.vmRuntimeGeneration vm))
  case mSpec of
    Left err
      | "disappeared from DB" `T.isInfixOf` err -> do
          logWarnN $
            "VM "
              <> vmName vm
              <> " disappeared from DB during reapply; marking stopped"
          void $ liftIO $ runSqlPool (completeVmMonitor vmId vm Nothing) pool
      | otherwise -> do
          logWarnN $
            "VM " <> vmName vm <> " reapply: assembleVmSpec failed: " <> err
          failReapply err
    Right spec -> do
      r <- liftIO $ NOA.vmStart nac spec
      case r of
        Right (NOA.VmStartStarted _) -> do
          logInfoN $ "VM " <> vmName vm <> " re-applied via vmStart"
          completed <- liftIO $ runSqlPool (setVmStartedIfCurrent vmId revision generation VmRunning) pool
          when completed $ liftIO $ attachVmMonitor state vmId
        Right NOA.VmStartVsockCidBusy -> do
          let msg = "vmStart reapply: nodeagent reported a VSOCK CID collision"
          logWarnN $ "vmStart reapply failed for VM " <> vmName vm <> ": " <> msg
          failReapply msg
        Left e -> do
          let msg = "vmStart reapply: " <> T.pack (show e)
          logWarnN $
            "vmStart reapply failed for VM "
              <> vmName vm
              <> ": "
              <> T.pack (show e)
          failReapply msg
