{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.Vm.Lifecycle
  ( VmStop (..)
  , VmPause (..)
  , VmSave (..)
  , VmReset (..)
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugN, logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Vm.Db
import Corvus.Handlers.Vm.Monitor (releaseManagedTaps)
import Corvus.Model (VmStatus (..))
import Corvus.Model hiding (DriveFormat, VmStatus)
import Corvus.Model.VmState (VmAction (..), validateTransition)
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol
import Corvus.Types
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Word (Word32)
import Database.Persist
import Database.Persist.Sql (runSqlPool)

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
    Just (_, currentStatus) ->
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
                liftIO $ runSqlPool (setVmStatus vmId VmPaused) (ssDbPool state)
                pure $ RespVmStateChanged VmPaused

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
-- The per-from-state cancel side-effect dispatch lives here:
--
--   * 'VmRunning' / 'VmStarting' / 'VmStopping' / 'VmPaused' /
--     'VmLoading' — SIGTERM-then-SIGKILL QEMU via 'NOA.vmStopHard'.
--   * 'VmSaved' — drop the state file via 'NOA.deleteSavedState';
--     no QEMU to kill.
--   * 'VmSaving' — kill QEMU (in-flight QMP @migrate@ aborts when
--     the process dies). The save executor observes the agent RPC
--     failure and skips its own status flip (the row is already
--     VmStopped by the time the executor returns).
--   * 'VmMigrating' — kill QEMU on the source if any (the agent's
--     vmStopHard is idempotent), drop the state file. Partial
--     destination-side artifacts get cleaned up on the next
--     migrate attempt; doing it now would require cross-node
--     orchestration that the FSM lock no longer holds.
--   * 'VmStopped' / 'VmError' — DB write only; idempotent.
--
-- The status is committed first so the monitor thread observes
-- 'VmStopped' and skips its own reconciliation.
handleVmReset :: ServerState -> Int64 -> IO Response
handleVmReset state vmId = runServerLogging state $ do
  mVm <- liftIO $ runSqlPool (get (toSqlKey vmId :: VmId)) (ssDbPool state)
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm -> case validateTransition (vmStatus vm) ActionReset of
      Left errMsg -> pure $ RespInvalidTransition (vmStatus vm) errMsg
      Right _ -> do
        mClaim <- liftIO $ runSqlPool (claimVmReset vmId) (ssDbPool state)
        case mClaim of
          Nothing -> pure $ RespInvalidTransition (vmStatus vm) "VM lifecycle changed while reset was being admitted"
          Just (revision, generation) -> do
            let fromStatus = vmStatus vm
                failReset message = do
                  _ <- liftIO $ runSqlPool (completeVmStop vmId revision generation (Just message)) (ssDbPool state)
                  pure $ RespError message
            -- First fence the nodeagent, including the no-live-process case:
            -- this is what invalidates a start still between helper spawn and
            -- ledger publication. Do not claim stopped on transport failure.
            outer <- liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmStopHard nac vmId (Just revision)
            case outer of
              Left err -> failReset ("nodeagent unavailable during reset: " <> err)
              Right (Left e) -> failReset ("vmStopHard: " <> T.pack (show e))
              Right (Right res)
                | NOA.vsrKind res `elem` [NOA.VmStopStopped, NOA.VmStopAlreadyStopped] -> do
                    cleanup <-
                      if fromStatus `elem` [VmSaved, VmMigrating]
                        then liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.deleteSavedState nac (vmName vm)
                        else pure (Right (Right ()))
                    case cleanup of
                      Left err -> failReset ("deleteSavedState: " <> err)
                      Right (Left err) -> failReset ("deleteSavedState: " <> T.pack (show err))
                      Right (Right ()) -> do
                        completed <- liftIO $ runSqlPool (completeVmReset vmId revision) (ssDbPool state)
                        if completed
                          then do
                            liftIO $ releaseManagedTaps state vmId
                            pure $ RespVmStateChanged VmStopped
                          else do
                            mCurrent <- liftIO $ runSqlPool (getVmWithStatus vmId) (ssDbPool state)
                            pure $ case mCurrent of
                              Nothing -> RespVmNotFound
                              Just (_, status) -> RespInvalidTransition status "VM lifecycle operation was superseded"
                | otherwise -> failReset ("vmStopHard: " <> NOA.vsrMessage res)

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
  actionExecute ctx a = handleVmReset (acState ctx) (vrstVmId a)

-- Complex handlers with validate/execute split

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
