{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.Vm.Lifecycle
  ( VmReboot (..)
  , VmStop (..)
  , VmPause (..)
  , VmSave (..)
  , VmReset (..)
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (atomically)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugN, logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Vm.Db
import Corvus.Handlers.Vm.Monitor (releaseManagedTaps)
import Corvus.Model (VmStatus (..))
import Corvus.Model hiding (DriveFormat, VmStatus)
import qualified Corvus.Model as M
import Corvus.Model.VmState (VmAction (..), validateTransition)
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol
import qualified Corvus.TaskScope as TaskScope
import Corvus.Types
import Data.Either (lefts, rights)
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Word (Word32)
import Database.Persist
import Database.Persist.Sql (runSqlPool, updateWhereCount)

handleVmStopValidate :: ServerState -> Int64 -> IO (Either Response (Vm, VmStatus))
handleVmStopValidate state vmId = do
  mVm <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure $ Left RespVmNotFound
    Just (vm, currentStatus) ->
      case validateTransition currentStatus ActionStop of
        Left errMsg -> pure $ Left $ RespInvalidTransition currentStatus errMsg
        Right _ -> pure $ Right (vm, currentStatus)

-- | Execute VM stop to completion (blocks until VmStopped).
--
-- After the refactor, the agent's 'vmStopGraceful' blocks until
-- QEMU has actually exited (or the timeout elapses), so the
-- daemon no longer needs to poll the DB. On graceful-timeout we
-- escalate to 'vmStopHard'.
handleVmStopExecute :: ActionContext -> Int64 -> Word32 -> Bool -> IO Response
handleVmStopExecute ctx vmId timeoutSec forceOnTimeout = do
  let state = acState ctx
  validated <- handleVmStopValidate state vmId
  case validated of
    Left errResp -> pure errResp
    Right (vm, _currentStatus) -> runServerLogging state $ do
      admitted <- liftIO $ runSqlPool (claimVmStop vmId vm) (ssDbPool state)
      if not admitted
        then pure $ RespError "VM lifecycle changed while stop was being admitted"
        else do
          let settle reason = liftIO $ runSqlPool (completeVmStop vmId (vmLifecycleRevision vm) (vmRuntimeGeneration vm) reason) (ssDbPool state)
              failStop message = do
                _ <- settle (Just message)
                pure $ RespError message
              markStopped = do
                completed <- settle Nothing
                if completed
                  then do
                    liftIO $ releaseManagedTaps state vmId
                    pure $ RespVmStateChanged VmStopped
                  else pure $ RespError "Stop was superseded by another lifecycle operation"
              -- Hard-kill via SIGTERM→SIGKILL. Reused by both the
              -- graceful-timeout escalation and the @timeoutSec == 0@
              -- short-circuit. 'vmStopHard' signals before taking the
              -- operation lock on the agent, so it can interrupt a stuck
              -- graceful stop already in flight for the same VM.
              forceStop = do
                outerHard <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmStopHard nac vmId Nothing
                case outerHard of
                  Left err ->
                    failStop ("vmStopHard: " <> err)
                  Right rh -> case rh of
                    Right res | NOA.vsrKind res `elem` [NOA.VmStopStopped, NOA.VmStopAlreadyStopped] -> markStopped
                    Right res -> failStop ("vmStopHard: " <> NOA.vsrMessage res)
                    Left e ->
                      failStop ("vmStopHard: " <> T.pack (show e))
          if timeoutSec == 0
            then do
              logInfoN $
                "VM "
                  <> T.pack (show vmId)
                  <> " stop requested with timeout=0; hard-killing immediately"
              forceStop
            else do
              -- Surface what the task is blocked on (visible via
              -- @crv task list/show/wait@) before the long graceful RPC.
              liftIO $
                pushTaskProgress
                  state
                  (fromSqlKey (acTaskId ctx))
                  ("graceful-shutdown wait, ETA " <> T.pack (show timeoutSec) <> "s")
                  (0, 0)
              -- The nodeagent sends QGA powerdown and ACPI, then waits for exit.
              outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmStopGraceful nac vmId timeoutSec
              case outer of
                Left err -> do
                  logWarnN $ "nodeagent unavailable; cannot stop VM: " <> err
                  failStop err
                Right r -> case r of
                  Left e -> do
                    logWarnN $ "vmStopGraceful RPC failed: " <> T.pack (show e)
                    if forceOnTimeout then forceStop else failStop ("vmStopGraceful: " <> T.pack (show e))
                  Right res -> case NOA.vsrKind res of
                    NOA.VmStopStopped -> markStopped
                    NOA.VmStopAlreadyStopped -> markStopped
                    NOA.VmStopTimeout
                      | not forceOnTimeout ->
                          failStop "Graceful shutdown timed out; VM preserved and image capture refused"
                    NOA.VmStopTimeout -> do
                      logWarnN $
                        "VM "
                          <> T.pack (show vmId)
                          <> " did not exit within graceful window; force-stopping"
                      forceStop
                    NOA.VmStopFailed ->
                      failStop ("vmStopGraceful failed: " <> NOA.vsrMessage res)

-- | Poll until VM status is VmStopped or VmError, or timeout.
handleVmPause :: ServerState -> Int64 -> IO Response
handleVmPause state vmId = runServerLogging state $ do
  mVm <- liftIO $ runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just (vm, currentStatus) ->
      case validateTransition currentStatus ActionPause of
        Left errMsg -> pure $ RespInvalidTransition currentStatus errMsg
        Right _ -> do
          logDebugN $ "Sending pause command to VM " <> T.pack (show vmId)
          outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmPause nac vmId
          case outer of
            Left err -> do
              logWarnN $ "nodeagent unavailable; cannot pause VM " <> T.pack (show vmId) <> ": " <> err
              pure $ RespInvalidTransition currentStatus err
            Right r -> case r of
              Left e -> do
                logWarnN $ "vmPause failed for VM " <> T.pack (show vmId) <> ": " <> T.pack (show e)
                pure $ RespInvalidTransition currentStatus ("vmPause: " <> T.pack (show e))
              Right () -> do
                logInfoN $ "VM " <> T.pack (show vmId) <> " paused"
                updated <-
                  liftIO $
                    runSqlPool
                      (updateWhereCount [VmId ==. toSqlKey vmId, VmLifecycleRevision ==. vmLifecycleRevision vm, VmRuntimeGeneration ==. vmRuntimeGeneration vm, M.VmStatus ==. currentStatus] [M.VmStatus =. VmPaused])
                      (ssDbPool state)
                pure $ if updated == 1 then RespVmStateChanged VmPaused else RespError "Pause was superseded by reset"

-- | Validate that a VM can be saved. Returns the current status, or
-- an error response. The status is forwarded to 'handleVmSaveExecute'
-- so the executor can commit the validator-picked 'VmSaving'
-- transition without re-reading the row.
handleVmSaveValidate :: ServerState -> Int64 -> IO (Either Response VmStatus)
handleVmSaveValidate state vmId = do
  mVm <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case mVm of
    Nothing -> pure $ Left RespVmNotFound
    Just (_, currentStatus) ->
      case validateTransition currentStatus ActionSave of
        Left errMsg -> pure $ Left $ RespInvalidTransition currentStatus errMsg
        Right _ -> pure $ Right currentStatus

-- | Execute the save flow.
--
-- Claims 'VmSaving', retaining runtime resources until confirmed exit,
-- then asks the agent to issue QMP @migrate@
-- to the state file, wait for the write to complete, and @quit@
-- QEMU. The agent owns the state file path
-- (\<basePath\>/\<vmName\>/state.qemu).
--
-- On success: 'ActionSaveDone' → 'VmSaved'.
-- On failure: 'ActionSaveFail' → 'VmError'; QEMU may still be alive
-- and an operator @vm reset@ is required to clean up.
--
-- 'attachVmMonitor' detects the QEMU exit via the agent's ledger
-- but its 'VmSaving' arm short-circuits — the save executor owns the
-- terminal flip.
handleVmSaveExecute :: ServerState -> Int64 -> IO Response
handleVmSaveExecute state vmId = runServerLogging state $ do
  validated <- liftIO $ handleVmSaveValidate state vmId
  case validated of
    Left errResp -> pure errResp
    Right currentStatus -> do
      observed <- liftIO $ runSqlPool (get (toSqlKey vmId :: VmId)) (ssDbPool state)
      case observed of
        Nothing -> pure RespVmNotFound
        Just vm | vmStatus vm /= currentStatus -> pure $ RespError "VM lifecycle changed while save was being admitted"
        Just vm -> do
          admitted <- liftIO $ runSqlPool (claimVmSave vmId vm) (ssDbPool state)
          if not admitted
            then pure $ RespError "VM lifecycle changed while save was being admitted"
            else do
              logInfoN $ "Saving VM " <> T.pack (show vmId) <> " state to disk"
              outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmSave nac vmId
              let reason = case outer of
                    Left err -> Just err
                    Right (Left err) -> Just (T.pack (show err))
                    Right (Right ()) -> Nothing
              completed <- liftIO $ runSqlPool (completeVmSave vmId (vmLifecycleRevision vm) (vmRuntimeGeneration vm) reason) (ssDbPool state)
              if not completed
                then pure $ RespError "Save was superseded by another lifecycle operation"
                else case reason of
                  Just msg -> pure $ RespInvalidTransition currentStatus msg
                  Nothing -> do
                    liftIO $ releaseManagedTaps state vmId
                    pure $ RespVmStateChanged VmSaved

-- | Handle VM reset command — the universal escape hatch.
--
-- Goes through 'validateTransition' so the FSM is the single
-- source of truth ('ActionReset' always returns 'Right VmStopped').
-- Admission advances the lifecycle revision and sets 'VmStopping', retaining
-- the runtime generation until termination is confirmed. Every reset fences
-- the nodeagent and calls 'NOA.vmStopHard', including from 'VmStopped' and
-- 'VmError', to invalidate pending starts even when no runtime is published.
-- Live QEMU processes are terminated with SIGTERM then SIGKILL if needed;
-- an in-flight save aborts when its QEMU process dies.
--
-- After drain, remove saved-state files on all participating hosts, including
-- partial destination-side migration state and state left by a failed reset.
--
-- Return after confirmed termination. Commit 'VmStopped' after drain and cleanup,
-- guarded by the lifecycle revision so stale completions cannot overwrite a
-- newer operation. Failures record an error for the matching runtime instead
-- of claiming that it stopped. Release managed TAPs before the stopped commit.
handleVmReset :: ActionContext -> Int64 -> IO Response
handleVmReset ctx vmId = runServerLogging state $ do
  mVm <- liftIO $ runSqlPool (get (toSqlKey vmId :: VmId)) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm -> case validateTransition (vmStatus vm) ActionReset of
      Left errMsg -> pure $ RespInvalidTransition (vmStatus vm) errMsg
      Right _ -> do
        mClaim <- liftIO $ runSqlPool (claimVmReset vmId) (ssDbPool state)
        case mClaim of
          Nothing -> pure $ RespInvalidTransition (vmStatus vm) "VM lifecycle changed while reset was being admitted"
          Just (revision, _generation) -> do
            liftIO $ atomically $ TaskScope.recordVmResetRevision (ssTasks state) (fromSqlKey (acRootTaskId ctx)) vmId revision
            let failReset message = do
                  liftIO $
                    runSqlPool
                      (updateWhere [VmId ==. toSqlKey vmId, VmLifecycleRevision ==. revision] [VmErrorMessage =. Just message])
                      (ssDbPool state)
                  pure $ RespError message
            -- First fence the nodeagent, including the no-live-process case:
            -- this is what invalidates a start still between helper spawn and
            -- ledger publication. Do not claim stopped on transport failure.
            agents <- liftIO $ resetAgents state vmId vm
            let clients = rights agents
            results <- liftIO $ mapConcurrently (\nac -> NOA.vmStopHard nac vmId (Just revision)) clients
            case lefts agents of
              err : _ -> failReset ("nodeagent unavailable during reset: " <> err)
              [] -> case sequence results of
                Left e -> failReset ("vmStopHard: " <> T.pack (show e))
                Right stopped
                  | all (\res -> NOA.vsrKind res `elem` [NOA.VmStopStopped, NOA.VmStopAlreadyStopped]) stopped -> do
                      liftIO $ watchResetDrain ctx state vmId revision (vmName vm) clients
                      pure $ RespVmStateChanged VmStopped
                  | otherwise -> failReset "vmStopHard did not confirm termination on every participating node"
  where
    -- Emergency RPCs must retain the reset fence even while the old runtime
    -- remains in the ledger. Ordinary requests use revision-scoped clients.
    state = (acState ctx) {ssActionRoot = Nothing}

-- Source plus every host used by this VM's task trees, including migration
-- destinations and transfer capabilities. After daemon restart the old host
-- registry is lost: conservatively fence all nodes for a persisted recovery.
resetAgents :: ServerState -> Int64 -> Vm -> IO [Either T.Text NOA.NodeAgentClient]
resetAgents state vmId vm = do
  touched <- atomically $ TaskScope.vmOperationNodes (ssTasks state) vmId
  recoveringNodes <-
    if vmStatus vm == VmStopping || isNothing touched
      then runSqlPool (selectKeysList [] []) (ssDbPool state)
      else pure []
  mapM
    (lookupNodeAgent state)
    (nub (vmNodeId vm : maybe [] (map toSqlKey) touched ++ recoveringNodes))

-- Keep the original nodeagent capability. A disconnected/restarted agent or
-- a missing tombstone is not proof that this attempt's workers have drained.
watchResetDrain :: ActionContext -> ServerState -> Int64 -> Int64 -> T.Text -> [NOA.NodeAgentClient] -> IO ()
watchResetDrain ctx state vmId revision vmNameToClean clients = do
  _ <- forkIO $ do
    let root = fromSqlKey (acRootTaskId ctx)
        cleanSavedState nac = do
          scoped <- NOA.vmOperationClient nac [(vmId, revision, Nothing)]
          case scoped of
            Left err -> pure (Left err)
            Right client -> NOA.deleteSavedState client vmNameToClean
        loop = do
          current <- atomically $ TaskScope.vmResetRevisionMatches (ssTasks state) root vmId revision
          when current $ do
            results <- mapConcurrently (\nac -> NOA.vmDrainStatus nac vmId revision) clients
            case sequence results of
              Right drained | and drained -> do
                atomically $ TaskScope.waitVmTasks (ssTasks state) root vmId
                -- Clean every participating host only after old transfers and
                -- migration workers have stopped writing saved state.
                TaskScope.withVmResetFinalization (ssTasks state) root vmId revision $ do
                  cleanup <- mapConcurrently cleanSavedState clients
                  case sequence cleanup of
                    Left err ->
                      runSqlPool
                        (updateWhere [VmId ==. toSqlKey vmId, VmLifecycleRevision ==. revision] [VmErrorMessage =. Just ("deleteSavedState: " <> T.pack (show err))])
                        (ssDbPool state)
                    Right _ -> do
                      -- Keep Stopping durable until both registries have drained.
                      releaseManagedTaps state vmId
                      completed <- runSqlPool (completeVmReset vmId revision) (ssDbPool state)
                      when completed $
                        atomically $
                          TaskScope.confirmVmRevisionDrain (ssTasks state) root vmId revision
              Right _ -> threadDelay 200000 >> loop
              Left _ -> pure ()
    loop
  pure ()

newtype VmPause = VmPause {vpVmId :: Int64}

instance Action VmPause where
  actionSubsystem _ = SubVm
  actionCommand _ = "pause"
  actionEntityId = Just . fromIntegral . vpVmId
  actionExecute ctx a = handleVmPause (acState ctx) (vpVmId a)

newtype VmSave = VmSave {vsaveVmId :: Int64}

instance Action VmSave where
  actionSubsystem _ = SubVm
  actionCommand _ = "save"
  actionEntityId = Just . fromIntegral . vsaveVmId
  actionValidate state a = do
    result <- handleVmSaveValidate state (vsaveVmId a)
    pure $ case result of
      Left errResp -> Just errResp
      Right _ -> Nothing
  actionExecute ctx a = handleVmSaveExecute (acState ctx) (vsaveVmId a)

newtype VmReset = VmReset {vrstVmId :: Int64}

instance Action VmReset where
  actionSubsystem _ = SubVm
  actionCommand _ = "reset"
  actionEntityId = Just . fromIntegral . vrstVmId
  actionEmergencyVm = Just . vrstVmId
  actionBypassLeases _ = True
  actionExecute ctx a = handleVmReset ctx (vrstVmId a)

-- Complex handlers with validate/execute split

newtype VmReboot = VmReboot {vrbVmId :: Int64}

instance Action VmReboot where
  actionSubsystem _ = SubVm
  actionCommand _ = "reboot"
  actionEntityId = Just . fromIntegral . vrbVmId
  actionValidate state action = do
    result <- runSqlPool (getVmWithStatus (vrbVmId action)) (ssDbPool state)
    pure $ case result of
      Nothing -> Just RespVmNotFound
      Just (_, VmRunning) -> Nothing
      Just (_, status) -> Just $ RespInvalidTransition status "VM must be running to reboot"
  actionExecute ctx action = runServerLogging (acState ctx) $ do
    let state = acState ctx
        vmId = vrbVmId action
    current <- liftIO $ runSqlPool (getVmWithStatus vmId) (ssDbPool state)
    case current of
      Nothing -> pure RespVmNotFound
      Just (_, status)
        | status /= VmRunning ->
            pure $ RespInvalidTransition status "VM must be running to reboot"
      Just (vm, _) -> do
        result <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmReboot nac vmId (vmGuestAgent vm)
        pure $ case result of
          Left err -> RespError err
          Right (Left err) -> RespError (T.pack (show err))
          Right (Right ()) -> RespVmStateChanged VmRunning

data VmStop = VmStop
  { vstpVmId :: Int64
  , vstpTimeout :: Word32
  , vstpForceOnTimeout :: Bool
  }

instance Action VmStop where
  actionSubsystem _ = SubVm
  actionCommand _ = "stop"
  actionEntityId = Just . fromIntegral . vstpVmId
  actionValidate state a = do
    result <- handleVmStopValidate state (vstpVmId a)
    pure $ case result of
      Left errResp -> Just errResp
      Right _ -> Nothing
  actionExecute ctx a = handleVmStopExecute ctx (vstpVmId a) (vstpTimeout a) (vstpForceOnTimeout a)

-- (Phase 3 refactor: the @StartVirtiofsd@ and @LaunchQemu@
-- subtask actions are gone — virtiofsd is implicit in 'VmSpec',
-- and QEMU spawning happens inline via 'NOA.vmStart' in
