{-# LANGUAGE OverloadedStrings #-}

-- | @snapshot rollback --auto-stop@ orchestration.
--
-- QEMU has no online snapshot-rollback command — the underlying
-- @qemu-img snapshot -a@ requires the qcow2 file not be open for
-- writes, which collides with a running VM's exclusive lock. The
-- daemon-side workaround is to graceful-stop every running VM
-- that has the disk attached, run the offline rollback, then
-- restart every VM it stopped.
--
-- The handler lives in its own module (rather than alongside the
-- offline rollback in "Corvus.Handlers.Disk.Snapshot") to break a
-- module cycle: @Handlers.Vm@ imports @Handlers.Disk@, which
-- re-exports @Handlers.Disk.Snapshot@; putting the @VmStop@ /
-- @VmStart@ usage here keeps the snapshot handler from importing
-- Vm directly.
module Corvus.Handlers.Disk.SnapshotAutoStop
  ( SnapshotRollbackAutoStop (..)
  , handleSnapshotRollbackAutoStop
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logErrorN, logInfoN)
import Corvus.Action
import Corvus.Handlers.Disk.Db (getRunningAttachedVms)
import Corvus.Handlers.Disk.Snapshot (handleSnapshotRollback)
import Corvus.Handlers.Vm (VmStart (..), VmStop (..))
import Corvus.Model
import Corvus.Protocol
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Postgresql (runSqlPool)

-- | Stop every running/paused VM attached to @diskId@, delegate
-- to the offline rollback, then restart every VM that was
-- stopped. If a stop fails the rollback is NOT attempted (we
-- don't want to mutate the disk while another VM might still
-- hold it open). If a restart fails after a successful rollback
-- the failure is surfaced — operator likely needs to investigate
-- the VM by hand.
handleSnapshotRollbackAutoStop :: ActionContext -> Int64 -> Ref -> IO Response
handleSnapshotRollbackAutoStop ctx diskId snapRef = runServerLogging state $ do
  vmIds <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
  case vmIds of
    [] -> do
      -- Nothing running; defer to the offline path verbatim.
      liftIO $ handleSnapshotRollback state diskId snapRef
    _ -> do
      logInfoN $
        "autoStop rollback: stopping "
          <> T.pack (show (length vmIds))
          <> " VM(s) before offline rollback"
      stopResults <- liftIO $ mapM (stopVmAsSubtask ctx) vmIds
      case firstFailure (zip vmIds stopResults) of
        Just (vid, err) -> do
          logErrorN $
            "autoStop rollback: VM " <> T.pack (show vid) <> " failed to stop: " <> err
          pure $
            RespError
              ("autoStop: stopping VM " <> T.pack (show vid) <> ": " <> err)
        Nothing -> do
          rollback <- liftIO $ handleSnapshotRollback state diskId snapRef
          restartResults <- liftIO $ mapM (startVmAsSubtask ctx) vmIds
          case firstFailure (zip vmIds restartResults) of
            Nothing -> pure rollback
            Just (vid, err) -> do
              logErrorN $
                "autoStop rollback: VM "
                  <> T.pack (show vid)
                  <> " failed to restart: "
                  <> err
              pure $
                RespError
                  ( "autoStop: VM "
                      <> T.pack (show vid)
                      <> " failed to restart post-rollback: "
                      <> err
                  )
  where
    state = acState ctx

    firstFailure :: [(a, Either e ())] -> Maybe (a, e)
    firstFailure [] = Nothing
    firstFailure ((_, Right ()) : rest) = firstFailure rest
    firstFailure ((x, Left e) : _) = Just (x, e)

-- | Subtask helpers: thread the autoStop action's own task id
-- through as the parent so the @task.parent@ FK is satisfied
-- (and operators see the stop/start subtasks attached to the
-- rollback in the history). The context's client-name is reused
-- verbatim — both subtasks should attribute to the same caller.
-- 'handleVmStopExecute' / 'handleVmStartExecute' signal success
-- with 'RespVmStateChanged' carrying the new state — they do NOT
-- return 'RespOk'. We treat any "running/paused → stopped"
-- transition as a successful stop, and any "stopped →
-- starting/running" transition as a successful start.
stopVmAsSubtask :: ActionContext -> Int64 -> IO (Either Text ())
stopVmAsSubtask ctx vmId = do
  resp <- runActionAsSubtask ctx (VmStop vmId)
  pure $ case resp of
    RespOk -> Right ()
    RespVmStateChanged VmStopped -> Right ()
    RespVmStateChanged VmStopping -> Right ()
    RespError msg -> Left msg
    other -> Left ("VmStop unexpected response: " <> T.pack (show other))

startVmAsSubtask :: ActionContext -> Int64 -> IO (Either Text ())
startVmAsSubtask ctx vmId = do
  resp <- runActionAsSubtask ctx (VmStart vmId)
  pure $ case resp of
    RespOk -> Right ()
    RespVmStateChanged VmRunning -> Right ()
    RespVmStateChanged VmStarting -> Right ()
    RespVmStateChanged VmLoading -> Right ()
    RespError msg -> Left msg
    other -> Left ("VmStart unexpected response: " <> T.pack (show other))

-- ---------------------------------------------------------------------------
-- Action wrapper for the autoStop variant. The plain
-- 'Corvus.Handlers.Disk.Snapshot.SnapshotRollback' Action stays
-- as-is; this wrapper records its own task row when called
-- directly (e.g. via 'runActionAsSubtask' from the RPC layer).

data SnapshotRollbackAutoStop = SnapshotRollbackAutoStop
  { srasDiskId :: Int64
  , srasSnapRef :: Ref
  }

instance Action SnapshotRollbackAutoStop where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "rollback"
  actionEntityId = Just . fromIntegral . srasDiskId
  actionExecute ctx a =
    handleSnapshotRollbackAutoStop ctx (srasDiskId a) (srasSnapRef a)
