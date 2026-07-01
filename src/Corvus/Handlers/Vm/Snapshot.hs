{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | VM-scoped full-machine snapshot handlers.
--
-- Wraps the disk-scoped vmstate primitives in
-- "Corvus.Handlers.Disk.Snapshot" so operators can refer to a
-- snapshot by @(vm, name)@ rather than picking a carrier disk by
-- hand. Three mutating Actions:
--
-- * 'VmSnapshotCreate' — auto-picks the carrier (the writable
--   qcow2 drive with the lowest 'DriveId') and delegates to the
--   existing full-machine 'SnapshotCreate' Action.
-- * 'VmSnapshotRollback' — handles both the running-VM case
--   (delegates to 'handleVmstateRollback') and the stopped-VM
--   case (paused-start + load + cont).
-- * 'VmSnapshotDelete' — drives the agent's
--   'snapshotDeleteWithVmstate' directly, then deletes the rows.
--
-- The read-only listing helper 'handleVmSnapshotList' lives here
-- but is dispatched directly from "Corvus.Handlers" (no task
-- record for list ops, per the project convention).
module Corvus.Handlers.Vm.Snapshot
  ( VmSnapshotCreate (..)
  , VmSnapshotRollback (..)
  , VmSnapshotDelete (..)
  , handleVmSnapshotList
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Disk.Agent
  ( deleteSnapshotViaAgentWithVmstate
  , guestSetTimeViaAgent
  , loadSnapshotViaAgentWithVmstate
  )
import Corvus.Handlers.Disk.Path (resolveDiskPath)
import Corvus.Handlers.Disk.Snapshot
  ( SnapshotCreate (..)
  , handleVmstateRollback
  , listVmstateSiblingDrives
  )
import Corvus.Handlers.Vm (hasNetdMediatedNetIf, setVmError, setVmStatus)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Node.Image (ImageResult (..))
import qualified Corvus.Node.VmSpec as VS
import qualified Corvus.NodeAgentClient as NOA
import qualified Corvus.NodeAgentClient.Spec as NSpec
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol
import Corvus.Types
  ( ServerState (..)
  , lookupNetAgentMaybe
  , runServerLogging
  )
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (SqlBackend, SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)

--------------------------------------------------------------------------------
-- Action types
--------------------------------------------------------------------------------

data VmSnapshotCreate = VmSnapshotCreate
  { vscVmId :: Int64
  , vscName :: Text
  }

instance Action VmSnapshotCreate where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "vm-snapshot-create"
  actionEntityId = Just . fromIntegral . vscVmId
  actionEntityName = Just . vscName
  actionExecute ctx a = handleVmSnapshotCreate ctx (vscVmId a) (vscName a)

data VmSnapshotRollback = VmSnapshotRollback
  { vsrVmId :: Int64
  , vsrName :: Text
  }

instance Action VmSnapshotRollback where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "vm-snapshot-rollback"
  actionEntityId = Just . fromIntegral . vsrVmId
  actionEntityName = Just . vsrName
  actionExecute ctx a = handleVmSnapshotRollback (acState ctx) (vsrVmId a) (vsrName a)

data VmSnapshotDelete = VmSnapshotDelete
  { vsdVmId :: Int64
  , vsdName :: Text
  }

instance Action VmSnapshotDelete where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "vm-snapshot-delete"
  actionEntityId = Just . fromIntegral . vsdVmId
  actionEntityName = Just . vsdName
  actionExecute ctx a = handleVmSnapshotDelete (acState ctx) (vsdVmId a) (vsdName a)

--------------------------------------------------------------------------------
-- Create
--------------------------------------------------------------------------------

-- | Create a VM-scoped full-machine snapshot. Auto-picks the carrier
-- disk (the writable qcow2 drive with the lowest 'DriveId') and
-- delegates to the existing 'SnapshotCreate' Action with
-- @scrFullMachine = True@.
handleVmSnapshotCreate :: ActionContext -> Int64 -> Text -> IO Response
handleVmSnapshotCreate ctx vmId name = runServerLogging (acState ctx) $ do
  let state = acState ctx
      pool = ssDbPool state
  mVm <- liftIO $ runSqlPool (get (toSqlKey vmId :: VmId)) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm
      | vmStatus vm /= VmRunning ->
          pure $
            RespError $
              "VM "
                <> vmName vm
                <> " is not running (status: "
                <> enumToText (vmStatus vm)
                <> "); full-machine snapshots require a live QEMU process"
      | otherwise -> do
          eSiblings <-
            liftIO $
              runSqlPool
                (listVmstateSiblingDrives (toSqlKey vmId :: VmId))
                pool
          case eSiblings of
            Left err -> pure $ RespError err
            Right [] ->
              pure $
                RespError
                  "VM has no writable qcow2 drives; nothing to snapshot"
            Right (carrierId : _rest) -> do
              let diskIds = carrierId : _rest
              clash <-
                liftIO $ runSqlPool (snapshotNameClashes diskIds name) pool
              if clash
                then
                  pure $
                    RespError $
                      "Snapshot '" <> name <> "' already exists for VM " <> vmName vm
                else do
                  logInfoN $
                    "VM-scoped snapshot create: vm="
                      <> vmName vm
                      <> " name="
                      <> name
                      <> " carrier-disk-id="
                      <> T.pack (show (fromSqlKey carrierId))
                      <> " sibling-count="
                      <> T.pack (show (length diskIds))
                  resp <-
                    liftIO $
                      runActionAsSubtask
                        ctx
                        SnapshotCreate
                          { scrDiskId = fromSqlKey carrierId
                          , scrName = name
                          , scrQuiesce = NOA.QuiesceAuto
                          , scrFullMachine = True
                          }
                  case resp of
                    RespSnapshotCreated _ -> do
                      mInfo <-
                        liftIO $
                          runSqlPool (buildVmSnapshotInfo vmId name) pool
                      case mInfo of
                        Just info -> pure $ RespVmSnapshotCreated info
                        Nothing ->
                          pure $
                            RespError $
                              "Snapshot created but row lookup failed for VM "
                                <> vmName vm
                                <> ", name="
                                <> name
                    other -> pure other

-- | Does ANY of the supplied disks already carry a snapshot with
-- this name?
snapshotNameClashes :: [DiskImageId] -> Text -> SqlPersistT IO Bool
snapshotNameClashes diskIds name = do
  rows <-
    selectList
      [SnapshotDiskImageId <-. diskIds, SnapshotName ==. name]
      [LimitTo 1]
  pure (not (null rows))

--------------------------------------------------------------------------------
-- Rollback
--------------------------------------------------------------------------------

-- | Roll a VM back to a named full-machine snapshot. Handles both
-- the running-VM path (QMP @stop@ -> @snapshot-load@ -> @cont@) and
-- the stopped-VM path (launch paused, load, resume).
handleVmSnapshotRollback :: ServerState -> Int64 -> Text -> IO Response
handleVmSnapshotRollback state vmId name = runServerLogging state $ do
  let pool = ssDbPool state
  mVm <- liftIO $ runSqlPool (get (toSqlKey vmId :: VmId)) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm -> do
      mCarrier <- liftIO $ runSqlPool (findCarrierSnapshot vmId name) pool
      case mCarrier of
        Nothing -> pure RespSnapshotNotFound
        Just (carrierDiskId, carrierSnap) ->
          case vmStatus vm of
            VmStopped ->
              rollbackFromStopped state vmId vm carrierDiskId carrierSnap
            VmRunning ->
              handleVmstateRollback state (fromSqlKey carrierDiskId) carrierSnap
            VmPaused ->
              handleVmstateRollback state (fromSqlKey carrierDiskId) carrierSnap
            other ->
              pure $
                RespError $
                  "VM "
                    <> vmName vm
                    <> " is in transitional state "
                    <> enumToText other
                    <> "; wait for it to settle, then re-issue the rollback"

-- | Locate the carrier @Snapshot@ row for @(vmId, name)@. Returns
-- the carrier's disk id alongside the row so the caller can
-- resolve the device path without re-joining.
findCarrierSnapshot
  :: Int64
  -> Text
  -> SqlPersistT IO (Maybe (DiskImageId, Snapshot))
findCarrierSnapshot vmId name = do
  drives <- selectList [DriveVmId ==. (toSqlKey vmId :: VmId)] [Asc DriveId]
  let diskIds = map (driveDiskImageId . entityVal) drives
  rows <-
    selectList
      [ SnapshotDiskImageId <-. diskIds
      , SnapshotName ==. name
      , SnapshotHasVmstate ==. True
      ]
      [LimitTo 1]
  pure $ case rows of
    (Entity _ s : _) -> Just (snapshotDiskImageId s, s)
    [] -> Nothing

-- | Rollback path for a stopped VM. Launches QEMU paused, drives
-- @snapshot-load@, then @cont@s — leaving the VM running at the
-- captured state. Mirrors the build-cache resume choreography in
-- 'Corvus.Handlers.Build.resumeMemoryCacheBakeVm'.
rollbackFromStopped
  :: ServerState
  -> Int64
  -> Vm
  -> DiskImageId
  -> Snapshot
  -> LoggingT IO Response
rollbackFromStopped state vmId vm carrierDiskId carrierSnap = do
  let pool = ssDbPool state
      cfg = ssQemuConfig state
      tag = snapshotName carrierSnap
      nodeId = M.vmNodeId vm
  eSiblings <-
    liftIO $
      runSqlPool
        (listVmstateSiblingDrives (toSqlKey vmId :: VmId))
        pool
  case eSiblings of
    Left err -> pure $ RespError err
    Right diskIds -> do
      paths <-
        liftIO $ mapM (\d -> resolveDiskPath pool cfg d nodeId) diskIds
      carrierPath <-
        liftIO $ resolveDiskPath pool cfg carrierDiskId nodeId
      liftIO $ runSqlPool (setVmStatus vmId VmStarting) pool
      needsNetd <- liftIO $ runSqlPool (hasNetdMediatedNetIf vmId) pool
      mNetAgent <- liftIO $ lookupNetAgentMaybe state nodeId
      let netAgentForSpec = if needsNetd then mNetAgent else Nothing
          waitMs = if vmGuestAgent vm then 90000 else 0
      mSpec <-
        liftIO $
          NSpec.assembleVmSpec pool cfg netAgentForSpec vmId waitMs
      case mSpec of
        Left err -> do
          let msg = "vm-snapshot-rollback (stopped): assembleVmSpec: " <> err
          liftIO $ runSqlPool (setVmError vmId msg) pool
          pure $ RespError msg
        Right baseSpec -> do
          let spec = baseSpec {VS.vsStartPaused = True}
          logInfoN $
            "vm-snapshot-rollback (stopped): starting VM "
              <> vmName vm
              <> " paused for snapshot-load tag="
              <> tag
          startR <-
            liftIO $ withVmNodeAgent state vmId $ \nac -> NOA.vmStart nac spec
          case startR of
            Left err -> finishStartFailure pool vmId ("vmStart paused: " <> err)
            Right (Left e) ->
              finishStartFailure
                pool
                vmId
                ("vmStart paused: " <> T.pack (show e))
            Right (Right _runtime) -> do
              logInfoN $
                "vm-snapshot-rollback (stopped): loading vmstate tag=" <> tag
              loadRes <-
                liftIO $
                  loadSnapshotViaAgentWithVmstate
                    state
                    nodeId
                    carrierPath
                    paths
                    tag
                    vmId
              case loadRes of
                ImageSuccess -> do
                  resumeR <-
                    liftIO $ withVmNodeAgent state vmId $ \nac ->
                      NOA.vmResume nac vmId
                  case resumeR of
                    Left e -> finishStartFailure pool vmId ("cont: " <> e)
                    Right (Left e) ->
                      finishStartFailure
                        pool
                        vmId
                        ("cont: " <> T.pack (show e))
                    Right (Right ()) -> do
                      _ <- liftIO $ guestSetTimeViaAgent state nodeId vmId
                      liftIO $ runSqlPool (setVmStatus vmId VmRunning) pool
                      logInfoN $
                        "vm-snapshot-rollback (stopped): VM "
                          <> vmName vm
                          <> " resumed at snapshot state"
                      pure RespSnapshotOk
                ImageFormatNotSupported msg ->
                  finishStartFailure pool vmId ("snapshot-load: " <> msg)
                ImageError e ->
                  finishStartFailure pool vmId ("snapshot-load: " <> e)
                ImageNotFound ->
                  finishStartFailure
                    pool
                    vmId
                    "snapshot-load: snapshot not found on agent"

-- | Record an error on the VM row and surface it. The QEMU process
-- may be left running paused if the failure was post-vmStart; the
-- operator can clean it up via @crv vm stop@.
finishStartFailure
  :: Pool SqlBackend -> Int64 -> Text -> LoggingT IO Response
finishStartFailure pool vmId msg = do
  logWarnN msg
  liftIO $ runSqlPool (setVmError vmId msg) pool
  pure $ RespError msg

--------------------------------------------------------------------------------
-- Delete
--------------------------------------------------------------------------------

-- | Delete a VM-scoped snapshot. Removes vmstate + every sibling
-- block snapshot in one atomic QMP @snapshot-delete@, then deletes
-- the matching rows from the @snapshot@ table.
handleVmSnapshotDelete :: ServerState -> Int64 -> Text -> IO Response
handleVmSnapshotDelete state vmId name = runServerLogging state $ do
  let pool = ssDbPool state
      cfg = ssQemuConfig state
  mVm <- liftIO $ runSqlPool (get (toSqlKey vmId :: VmId)) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm
      | vmStatus vm /= VmRunning ->
          pure $
            RespError $
              "VM "
                <> vmName vm
                <> " is not running; vmstate snapshot delete needs a live "
                <> "QEMU process (QMP snapshot-delete)"
      | otherwise -> do
          mCarrier <- liftIO $ runSqlPool (findCarrierSnapshot vmId name) pool
          case mCarrier of
            Nothing -> pure RespSnapshotNotFound
            Just (_carrierDiskId, carrierSnap) -> do
              eSiblings <-
                liftIO $
                  runSqlPool
                    (listVmstateSiblingDrives (toSqlKey vmId :: VmId))
                    pool
              case eSiblings of
                Left err -> pure $ RespError err
                Right diskIds -> do
                  rows <-
                    liftIO $
                      runSqlPool
                        ( selectList
                            [ SnapshotDiskImageId <-. diskIds
                            , SnapshotName ==. snapshotName carrierSnap
                            ]
                            []
                        )
                        pool
                  let snapIds :: [SnapshotId]
                      snapIds = map entityKey rows
                  paths <-
                    liftIO $
                      mapM
                        (\d -> resolveDiskPath pool cfg d (M.vmNodeId vm))
                        diskIds
                  logInfoN $
                    "vm-snapshot-delete: vm="
                      <> vmName vm
                      <> " name="
                      <> snapshotName carrierSnap
                      <> " disks="
                      <> T.pack (show (length diskIds))
                  result <-
                    liftIO $
                      deleteSnapshotViaAgentWithVmstate
                        state
                        (M.vmNodeId vm)
                        paths
                        (snapshotName carrierSnap)
                        vmId
                  case result of
                    ImageSuccess -> do
                      liftIO $
                        runSqlPool (mapM_ delete snapIds) pool
                      logInfoN "vm-snapshot-delete OK"
                      pure RespSnapshotOk
                    ImageFormatNotSupported msg ->
                      pure $ RespFormatNotSupported msg
                    ImageError e -> pure $ RespError e
                    ImageNotFound -> pure RespSnapshotNotFound

--------------------------------------------------------------------------------
-- List
--------------------------------------------------------------------------------

-- | List the VM's full-machine snapshots, one row per @(vmId, name)@.
-- Aggregates sibling sizes; reports the carrier disk identity.
handleVmSnapshotList :: ServerState -> Int64 -> IO Response
handleVmSnapshotList state vmId = do
  let pool = ssDbPool state
  mVm <- runSqlPool (get (toSqlKey vmId :: VmId)) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just _ -> do
      infos <- runSqlPool (collectVmSnapshotInfos vmId) pool
      pure $ RespVmSnapshotList infos

collectVmSnapshotInfos :: Int64 -> SqlPersistT IO [VmSnapshotInfo]
collectVmSnapshotInfos vmId = do
  drives <- selectList [DriveVmId ==. (toSqlKey vmId :: VmId)] [Asc DriveId]
  let diskIds = map (driveDiskImageId . entityVal) drives
  rows <-
    selectList
      [SnapshotDiskImageId <-. diskIds]
      [Asc SnapshotCreatedAt]
  let carriers = [s | Entity _ s <- rows, snapshotHasVmstate s]
  mapM (buildOne rows) carriers
  where
    buildOne rows carrier = do
      let nm = snapshotName carrier
          siblings = [s | Entity _ s <- rows, snapshotName s == nm]
          total =
            fromIntegral
              ( sum
                  [ fromMaybe 0 (snapshotSizeMb s)
                  | s <- siblings
                  ]
              )
      mVm <- get (toSqlKey vmId :: VmId)
      mCarrierDisk <- get (snapshotDiskImageId carrier)
      pure
        VmSnapshotInfo
          { vsiName = nm
          , vsiCreatedAt = snapshotCreatedAt carrier
          , vsiVm =
              NamedRef
                { nrId = fromIntegral vmId
                , nrName = maybe "" vmName mVm
                }
          , vsiCarrierDisk =
              NamedRef
                { nrId = fromIntegral (fromSqlKey (snapshotDiskImageId carrier))
                , nrName = maybe "" diskImageName mCarrierDisk
                }
          , vsiDiskCount = length siblings
          , vsiTotalSizeMb = total
          }

--------------------------------------------------------------------------------
-- Helper used by VmSnapshotCreate
--------------------------------------------------------------------------------

-- | Build a single 'VmSnapshotInfo' for the freshly-created
-- (vmId, name) tuple. Returns 'Nothing' if the carrier row can't
-- be found (a "shouldn't happen" race after a successful create).
buildVmSnapshotInfo
  :: Int64 -> Text -> SqlPersistT IO (Maybe VmSnapshotInfo)
buildVmSnapshotInfo vmId name = do
  drives <- selectList [DriveVmId ==. (toSqlKey vmId :: VmId)] [Asc DriveId]
  let diskIds = map (driveDiskImageId . entityVal) drives
  rows <-
    selectList
      [SnapshotDiskImageId <-. diskIds, SnapshotName ==. name]
      []
  let siblings = map entityVal rows
      carriers = [s | s <- siblings, snapshotHasVmstate s]
  case carriers of
    (c : _) -> do
      mVm <- get (toSqlKey vmId :: VmId)
      mCarrierDisk <- get (snapshotDiskImageId c)
      let total =
            fromIntegral
              ( sum
                  [ fromMaybe 0 (snapshotSizeMb s)
                  | s <- siblings
                  ]
              )
      pure $
        Just
          VmSnapshotInfo
            { vsiName = name
            , vsiCreatedAt = snapshotCreatedAt c
            , vsiVm =
                NamedRef
                  { nrId = fromIntegral vmId
                  , nrName = maybe "" vmName mVm
                  }
            , vsiCarrierDisk =
                NamedRef
                  { nrId = fromIntegral (fromSqlKey (snapshotDiskImageId c))
                  , nrName = maybe "" diskImageName mCarrierDisk
                  }
            , vsiDiskCount = length siblings
            , vsiTotalSizeMb = total
            }
    [] -> pure Nothing
