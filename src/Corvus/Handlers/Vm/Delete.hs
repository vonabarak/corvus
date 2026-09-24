{-# LANGUAGE OverloadedStrings #-}

module Corvus.Handlers.Vm.Delete
  ( VmDelete (..)
  , deleteVm
  , getEphemeralAttachedDisks
  , deleteTpmStateForVm
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Disk (DiskDelete (..))
import Corvus.Handlers.Disk.Db (diskImageNodeFilePathFor)
import Corvus.Handlers.Resolve (ResolveError (..), resolveErrorMessage, resolveNode, validateName)
import Corvus.Handlers.Scheduler (pickNodeForVm)
import Corvus.Handlers.Vm.CloudInit (ensureCloudInitIso)
import Corvus.Handlers.Vm.Console (generateSpicePassword)
import Corvus.Handlers.Vm.Db
import Corvus.Handlers.Vm.Lifecycle (VmReset (..))
import Corvus.Handlers.Vm.Monitor (attachVmMonitor, releaseManagedTaps)
import Corvus.Model (DriveFormat (..), VmStatus (..))
import Corvus.Model hiding (DriveFormat, VmStatus)
import qualified Corvus.Model as M
import Corvus.Model.VmState (VmAction (..), validateTransition)
import Corvus.Node.SpicePort (withAllocatedSpicePort)
import Corvus.Node.VsockCid (withAllocatedVsockCid)
import qualified Corvus.NodeAgentClient as NOA
import qualified Corvus.NodeAgentClient.Spec as NSpec
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol
import Corvus.Qemu (QemuConfig, getGuestAgentSocket, getMonitorSocket, getSerialSocket)
import Corvus.Types
import Data.Int (Int64)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Word (Word32)
import Database.Persist
import Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlPool)
import System.FilePath ((</>))

handleVmDelete :: ActionContext -> Int64 -> Bool -> Bool -> IO Response
handleVmDelete ctx vmId keepDisks force = do
  resetResponse <-
    if force
      then runActionAsSubtask ctx (VmReset vmId)
      else pure (RespVmStateChanged VmStopped)
  case resetResponse of
    RespVmStateChanged VmStopped -> deleteStoppedVm ctx vmId keepDisks
    _ -> pure resetResponse

-- | Delete a VM that is known to be stopped. Re-fetching after a forced
-- reset also prevents a concurrent start from deleting a live VM.
deleteStoppedVm :: ActionContext -> Int64 -> Bool -> IO Response
deleteStoppedVm ctx vmId keepDisks = do
  let state = acState ctx
  result <- runSqlPool (getVmWithStatus vmId) (ssDbPool state)
  case result of
    Nothing -> pure RespVmNotFound
    Just (vm, status) ->
      if status
        `elem` [ VmRunning
               , VmStarting
               , VmStopping
               , VmPaused
               , VmSaving
               , VmLoading
               , VmMigrating
               ]
        then pure RespVmMustBeStopped
        else do
          -- TPM state belongs to an enabled TPM VM. Its deletion is
          -- strict: if the nodeagent cannot remove it, leave the VM
          -- and all of its associations intact so the operator can
          -- retry without orphaning persistent security state.
          tpmDeleteResult <-
            if vmTpm vm
              then deleteTpmStateForVm state vmId (vmName vm)
              else pure (Right ())
          case tpmDeleteResult of
            Left err -> pure (RespError err)
            Right () -> do
              -- For a saved VM, ask the agent to drop the per-VM
              -- state file before we tear the row down. Best-effort
              -- (the call is idempotent on the agent) so an
              -- unreachable agent doesn't block the delete.
              when (status == VmSaved) $
                runServerLogging state $ do
                  outerDel <-
                    liftIO $
                      withVmNodeAgent state vmId $ \nac ->
                        NOA.deleteSavedState nac (vmName vm)
                  case outerDel of
                    Left err ->
                      logWarnN $
                        "nodeagent unavailable; saved-state file may persist for deleted VM "
                          <> T.pack (show vmId)
                          <> ": "
                          <> err
                    Right (Left e) ->
                      logWarnN $
                        "deleteSavedState during delete failed for VM "
                          <> T.pack (show vmId)
                          <> ": "
                          <> T.pack (show e)
                    Right (Right ()) -> pure ()
              disksToDelete <-
                if keepDisks
                  then pure []
                  else runSqlPool (getEphemeralAttachedDisks vmId) (ssDbPool state)
              -- Delete VM and its associations (drives, netifs, etc.)
              runSqlPool (deleteVm vmId) (ssDbPool state)
              -- Reap each ephemeral disk as a subtask.
              mapM_ (runActionAsSubtask ctx . DiskDelete) disksToDelete
              pure RespVmDeleted

-- | Strictly remove persistent TPM state through the VM's nodeagent.
-- A missing state directory is success on the agent. Routing or RPC
-- failures are returned to the caller so it can leave the database
-- flag/row unchanged.
deleteTpmStateForVm :: ServerState -> Int64 -> Text -> IO (Either Text ())
deleteTpmStateForVm state vmId name = do
  outer <- withVmNodeAgent state vmId $ \nac -> NOA.deleteTpmState nac name
  pure $ case outer of
    Left err -> Left ("nodeagent unavailable; TPM state was not deleted: " <> err)
    Right (Left err) -> Left ("failed to delete TPM state: " <> T.pack (show err))
    Right (Right ()) -> Right ()

-- | Pick the right 'ActionStart' variant for a VM in its current
-- state. The validator returns the correct intermediate status
-- (VmRunning / VmStarting / VmLoading) per the chosen variant, so
-- this is the only place in the codebase that has to know "what
getEphemeralAttachedDisks :: Int64 -> SqlPersistT IO [Int64]
getEphemeralAttachedDisks vmId = do
  let key = toSqlKey vmId :: VmId
  drives <- selectList [M.DriveVmId ==. key] []
  let diskKeys = mapMaybe (driveDiskImageId . entityVal) drives
  ephemKeys <- filterM isEphemeral diskKeys
  let ephemIds = map fromSqlKey ephemKeys
  filterM (fmap not . isSharedDisk vmId) ephemIds
  where
    isEphemeral :: DiskImageId -> SqlPersistT IO Bool
    isEphemeral dk = do
      mDisk <- get dk
      pure $ maybe False diskImageEphemeral mDisk

    isSharedDisk :: Int64 -> Int64 -> SqlPersistT IO Bool
    isSharedDisk thisVmId diskId = do
      otherDrives <- selectList [M.DriveDiskImageId ==. Just (toSqlKey diskId), M.DriveVmId !=. toSqlKey thisVmId] [LimitTo 1]
      pure $ not (null otherDrives)

-- | Delete a VM and all associated resources
deleteVm :: Int64 -> SqlPersistT IO ()
deleteVm vmId = do
  let key = toSqlKey vmId :: VmId
  -- Drop any build-cache rows that pin this VM as the chain owner.
  -- Cascade is enforced at the application layer (no OnDelete in the
  -- schema); cache rows for a deleted bake VM would dangle otherwise.
  deleteWhere [M.BuildCacheEntryVmId ==. key]
  -- Delete cloud-init config
  deleteBy (M.UniqueCloudInitVm key)
  -- Delete SSH key associations
  deleteWhere [M.VmSshKeyVmId ==. key]
  -- Delete drives
  deleteWhere [M.DriveVmId ==. key]
  -- Delete network interfaces
  deleteWhere [M.NetworkInterfaceVmId ==. key]
  -- Delete shared directories
  deleteWhere [M.SharedDirVmId ==. key]
  -- Delete VM
  delete key

data VmDelete = VmDelete
  { vdelVmId :: Int64
  , vdelKeepDisks :: Bool
  -- ^ When 'False' (default), delete every ephemeral disk attached
  -- to the VM (cloud-init ISOs, template-instantiated disks). When
  -- 'True', leave all attached disks in place — including ephemeral
  -- ones — so the operator can debug or re-use them.
  , vdelForce :: Bool
  -- ^ Hard-reset the VM before deleting it. This discards unsaved guest
  -- state and deletion proceeds only when the reset reports 'VmStopped'.
  }

instance Action VmDelete where
  actionSubsystem _ = SubVm
  actionCommand _ = "delete"
  actionEntityId = Just . fromIntegral . vdelVmId
  actionExecute ctx a = handleVmDelete ctx (vdelVmId a) (vdelKeepDisks a) (vdelForce a)
