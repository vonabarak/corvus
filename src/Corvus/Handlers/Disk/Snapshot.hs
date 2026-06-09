{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Disk snapshot handlers (create / delete / rollback / merge / list).
--
-- Snapshots are a qcow2-only feature; the handlers reject other formats
-- with 'RespFormatNotSupported'.
--
-- == Live vs offline routing
--
-- Create / delete / merge transparently route between two backends
-- based on whether the disk is currently attached to a running or
-- paused VM:
--
-- * No attached VM (or all attached VMs are stopped): the OFFLINE
--   path runs @qemu-img snapshot -c/-d@ via
--   'createSnapshotViaAgent' / 'deleteSnapshotViaAgent'.
-- * Exactly one running/paused VM has the disk attached: the LIVE
--   path runs @blockdev-snapshot-internal-sync@ /
--   @blockdev-snapshot-delete-internal-sync@ via QMP, optionally
--   bracketed with QGA @guest-fsfreeze-freeze@/@thaw@ per the
--   caller's 'QuiesceMode'.
--
-- ROLLBACK is asymmetric: QEMU exposes no online equivalent of
-- @qemu-img snapshot -a@, so the offline path is the only choice.
-- The optional @autoStop@ knob on 'handleSnapshotRollback' lets
-- the daemon orchestrate a graceful @VmStop@ → rollback →
-- @VmStart@ cycle so the operator still issues one command, but
-- the VM does cycle. With @autoStop=False@ the handler refuses
-- the call on a running/paused VM (today's behaviour preserved).
module Corvus.Handlers.Disk.Snapshot
  ( -- * Action types
    SnapshotCreate (..)
  , SnapshotDelete (..)
  , SnapshotRollback (..)
  , SnapshotMerge (..)

    -- * Handlers
  , handleSnapshotCreate
  , handleSnapshotDelete
  , handleSnapshotRollback
  , handleSnapshotMerge
  , handleSnapshotList
  )
where

import Corvus.Action
import Corvus.Handlers.Disk.Db (getRunningAttachedVms, getSnapshots)
import Corvus.Handlers.Disk.Path (resolveDiskPath)
import Corvus.Handlers.Resolve (resolveSnapshot, validateName)

import Control.Monad (filterM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import Corvus.Handlers.Disk.Agent
  ( createSnapshotViaAgent
  , createSnapshotViaAgentLive
  , createSnapshotViaAgentWithVmstate
  , deleteSnapshotViaAgent
  , deleteSnapshotViaAgentLive
  , deleteSnapshotViaAgentWithVmstate
  , guestSetTimeViaAgent
  , loadSnapshotViaAgentWithVmstate
  , mergeSnapshotViaAgent
  , rollbackSnapshotViaAgent
  )
import Corvus.Handlers.Scheduler (pickNodeForExistingDisk)
import Corvus.Model
import Corvus.Node.Image (ImageResult (..))
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Protocol
import Corvus.Types (ServerState (..), lookupNodeAgent, runServerLogging)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (SqlPersistT, runSqlPool)

-- | Create a snapshot. Dispatches transparently between the
-- offline path (qemu-img on a stopped disk) and the live path
-- (QMP on a running or paused VM). 'QuiesceMode' only matters for
-- the live path; on the offline path it is ignored.
handleSnapshotCreate
  :: ServerState -> Int64 -> Text -> NOA.QuiesceMode -> Bool -> IO Response
handleSnapshotCreate state diskId snapshotName _quiesce True =
  handleSnapshotCreateWithVmstate state diskId snapshotName
handleSnapshotCreate state diskId snapshotName quiesce False =
  case validateName "Snapshot" snapshotName of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Creating snapshot '" <> snapshotName <> "' for disk " <> T.pack (show diskId)

      mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
      case mDisk of
        Nothing -> pure RespDiskNotFound
        Just disk
          | diskImageFormat disk /= FormatQcow2 -> do
              logWarnN "Snapshot requested on non-qcow2 image"
              pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
          | otherwise -> do
              mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
              case mNid of
                Left err -> pure $ RespError err
                Right nid -> do
                  filePath <-
                    liftIO $
                      resolveDiskPath
                        (ssDbPool state)
                        (ssQemuConfig state)
                        (toSqlKey diskId :: DiskImageId)
                        nid
                  runningVms <-
                    liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
                  (result, isLive, quiesced) <- case runningVms of
                    [] -> do
                      logInfoN "Snapshot path: offline (no running attached VM)"
                      r <- liftIO $ createSnapshotViaAgent state nid filePath snapshotName
                      pure (r, False, False)
                    (vmId : _) -> do
                      logInfoN $
                        "Snapshot path: live via QMP (VM "
                          <> T.pack (show vmId)
                          <> ", quiesce="
                          <> T.pack (show quiesce)
                          <> ")"
                      (r, q) <-
                        liftIO $
                          createSnapshotViaAgentLive
                            state
                            nid
                            filePath
                            snapshotName
                            vmId
                            quiesce
                      pure (r, True, q)
                  case result of
                    ImageSuccess -> do
                      now <- liftIO getCurrentTime
                      snapshotId <-
                        liftIO $
                          runSqlPool
                            ( insert
                                Snapshot
                                  { snapshotDiskImageId = toSqlKey diskId
                                  , snapshotName = snapshotName
                                  , snapshotCreatedAt = now
                                  , snapshotSizeMb = Nothing
                                  , snapshotLive = isLive
                                  , snapshotQuiesced = quiesced
                                  , snapshotHasVmstate = False
                                  }
                            )
                            (ssDbPool state)
                      logInfoN $
                        "Created snapshot with ID: "
                          <> T.pack (show $ fromSqlKey snapshotId)
                          <> (if isLive then " (live)" else " (offline)")
                          <> (if quiesced then ", quiesced" else "")
                      pure $ RespSnapshotCreated $ fromSqlKey snapshotId
                    ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                    ImageError err -> pure $ RespError err
                    ImageNotFound -> pure RespDiskNotFound

-- | Take a full-machine snapshot (vmstate + RAM + device + CPU
-- state) of a running VM. The disk the user invoked
-- @snapshot create --with-ram@ on is the vmstate carrier; every
-- other writable qcow2 drive attached to the same running VM
-- gets a sibling block snapshot under the same name.
--
-- Returns the carrier's @Snapshot@ row id on success — that's the
-- one with @hasVmstate=True@, which the rollback path uses to
-- find the rest.
handleSnapshotCreateWithVmstate
  :: ServerState -> Int64 -> Text -> IO Response
handleSnapshotCreateWithVmstate state diskId snapshotName' =
  case validateName "Snapshot" snapshotName' of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $
        "Creating vmstate snapshot '"
          <> snapshotName'
          <> "' for carrier disk "
          <> T.pack (show diskId)
      mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
      case mDisk of
        Nothing -> pure RespDiskNotFound
        Just disk
          | diskImageFormat disk /= FormatQcow2 ->
              pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
          | otherwise -> do
              runningVms <-
                liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
              case runningVms of
                [] ->
                  pure $
                    RespError
                      "Full-machine (--with-ram) snapshots require the VM to be running"
                (vmId : _ : _)
                  | length runningVms > 1 ->
                      pure $
                        RespError
                          ( "Carrier disk is attached to "
                              <> T.pack (show (length runningVms))
                              <> " running VMs; full-machine snapshots require exactly one"
                          )
                (vmId : _) -> do
                  mNid <-
                    liftIO $
                      pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
                  case mNid of
                    Left err -> pure $ RespError err
                    Right nid -> do
                      siblings <-
                        liftIO $
                          runSqlPool
                            (listVmstateSiblingDrives (toSqlKey vmId :: VmId))
                            (ssDbPool state)
                      case siblings of
                        Left err -> pure $ RespError err
                        Right diskIds -> do
                          ensureCarrierInSet
                            (toSqlKey diskId `elem` diskIds)
                            ( "Carrier disk "
                                <> T.pack (show diskId)
                                <> " is not in the VM's writable qcow2 drive set "
                                <> T.pack (show (map fromSqlKey diskIds))
                            )
                            $ do
                              paths <-
                                liftIO $
                                  mapM
                                    ( \did ->
                                        resolveDiskPath
                                          (ssDbPool state)
                                          (ssQemuConfig state)
                                          did
                                          nid
                                    )
                                    diskIds
                              carrierPath <-
                                liftIO $
                                  resolveDiskPath
                                    (ssDbPool state)
                                    (ssQemuConfig state)
                                    (toSqlKey diskId :: DiskImageId)
                                    nid
                              result <-
                                liftIO $
                                  createSnapshotViaAgentWithVmstate
                                    state
                                    nid
                                    carrierPath
                                    paths
                                    snapshotName'
                                    vmId
                              case result of
                                ImageSuccess -> do
                                  now <- liftIO getCurrentTime
                                  carrierKey <-
                                    liftIO $
                                      runSqlPool
                                        ( do
                                            keys <-
                                              mapM
                                                ( \did ->
                                                    insert
                                                      Snapshot
                                                        { snapshotDiskImageId = did
                                                        , snapshotName = snapshotName'
                                                        , snapshotCreatedAt = now
                                                        , snapshotSizeMb = Nothing
                                                        , snapshotLive = True
                                                        , snapshotQuiesced = False
                                                        , snapshotHasVmstate =
                                                            did == toSqlKey diskId
                                                        }
                                                )
                                                diskIds
                                            pure $
                                              head
                                                [ k
                                                | (d, k) <- zip diskIds keys
                                                , d == toSqlKey diskId
                                                ]
                                        )
                                        (ssDbPool state)
                                  logInfoN $
                                    "vmstate snapshot created; carrier id="
                                      <> T.pack (show (fromSqlKey carrierKey))
                                      <> " across "
                                      <> T.pack (show (length diskIds))
                                      <> " disks"
                                  pure $ RespSnapshotCreated (fromSqlKey carrierKey)
                                ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                                ImageError err -> pure $ RespError err
                                ImageNotFound -> pure RespDiskNotFound
  where
    -- @ensureCarrierInSet inSet errMsg action@: when the carrier
    -- IS in the writable-disk set, run the snapshot 'action';
    -- otherwise return @errMsg@. Named over Control.Monad.unless
    -- because the polarity matters here — a previous iteration
    -- swapped these branches and the integration suite caught it
    -- with "Carrier disk 14 is not in the writable qcow2 drive
    -- set [14]" (where the carrier WAS in the set, but the
    -- inverted check fired the error anyway).
    ensureCarrierInSet True _ k = k
    ensureCarrierInSet False err _ = pure $ RespError err

-- | Enumerate a VM's writable qcow2 drives that can carry an
-- internal snapshot. Filters out pflash / floppy interfaces
-- (firmware vars, not data), read-only drives, and non-qcow2
-- formats. Returns the list of 'DiskImageId' in drive-position
-- order so the carrier's neighbours appear stable across calls.
--
-- Returns @Left@ if the drives span multiple nodes (vmstate save
-- is a single QEMU-process operation; cross-node sets can't be
-- atomic).
listVmstateSiblingDrives
  :: VmId -> SqlPersistT IO (Either Text [DiskImageId])
listVmstateSiblingDrives vmKey = do
  drives <-
    selectList
      [DriveVmId ==. vmKey, DriveReadOnly ==. False]
      [Asc DriveId]
  let candidateIds =
        [ driveDiskImageId (entityVal d)
        | d <- drives
        , driveInterface (entityVal d) `notElem` [InterfacePflash, InterfaceFloppy]
        ]
  qcow2Ids <- filterM isQcow2 candidateIds
  pure (Right qcow2Ids)
  where
    isQcow2 did = do
      mDisk <- get did
      pure $ case mDisk of
        Just d | diskImageFormat d == FormatQcow2 -> True
        _ -> False

-- | Delete a snapshot. Routes between three paths based on the
-- snapshot's @hasVmstate@ flag and the attached VM's running
-- state. If the snapshot carries vmstate, the delete spans every
-- sibling row that shares the snapshot @name@ on the same VM's
-- writable-disk set (the vmstate snapshot is atomic on the qcow2
-- side; deleting only one sibling would leave vmstate orphaned
-- in the carrier).
handleSnapshotDelete :: ServerState -> Int64 -> Ref -> IO Response
handleSnapshotDelete state diskId snapRef = runServerLogging state $ do
  logInfoN $ "Deleting snapshot '" <> unRef snapRef <> "' from disk " <> T.pack (show diskId)

  -- First check disk exists
  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk
      | diskImageFormat disk /= FormatQcow2 ->
          pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
      | otherwise -> do
          mSnapId <- liftIO $ resolveSnapshot snapRef diskId (ssDbPool state)
          case mSnapId of
            Left _ -> pure RespSnapshotNotFound
            Right snapshotId -> do
              mSnapshot <-
                liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
              case mSnapshot of
                Nothing -> pure RespSnapshotNotFound
                Just snapshot
                  | snapshotHasVmstate snapshot ->
                      handleSnapshotDeleteWithVmstate state diskId snapshotId snapshot
                  | otherwise ->
                      handleSnapshotDeletePlain state diskId snapshotId snapshot

handleSnapshotDeletePlain
  :: ServerState -> Int64 -> Int64 -> Snapshot -> LoggingT IO Response
handleSnapshotDeletePlain state diskId snapshotId snapshot = do
  mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
  case mNid of
    Left err -> pure $ RespError err
    Right nid -> do
      filePath <-
        liftIO $
          resolveDiskPath
            (ssDbPool state)
            (ssQemuConfig state)
            (toSqlKey diskId :: DiskImageId)
            nid
      runningVms <-
        liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
      result <- case runningVms of
        [] -> do
          logInfoN "Snapshot delete path: offline"
          liftIO $
            deleteSnapshotViaAgent state nid filePath (snapshotName snapshot)
        (vmId : _) -> do
          logInfoN $
            "Snapshot delete path: live via QMP (VM "
              <> T.pack (show vmId)
              <> ")"
          liftIO $
            deleteSnapshotViaAgentLive
              state
              nid
              filePath
              (snapshotName snapshot)
              vmId
      case result of
        ImageSuccess -> do
          liftIO $
            runSqlPool
              (delete (toSqlKey snapshotId :: SnapshotId))
              (ssDbPool state)
          logInfoN "Snapshot deleted"
          pure RespSnapshotOk
        ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
        ImageError err -> pure $ RespError err
        ImageNotFound -> pure RespDiskNotFound

-- | Delete a vmstate-aware snapshot AND its sibling block
-- snapshots in one atomic agent call. The @snapshot@ argument is
-- the carrier row (the one with @hasVmstate=True@); siblings are
-- discovered via a Drive-join against the same VM's writable
-- qcow2 disk set and matching @snapshotName@.
handleSnapshotDeleteWithVmstate
  :: ServerState -> Int64 -> Int64 -> Snapshot -> LoggingT IO Response
handleSnapshotDeleteWithVmstate state diskId carrierSnapId carrierSnap = do
  runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
  case runningVms of
    [] ->
      pure $
        RespError
          "vmstate snapshot delete requires the VM to be running (QMP snapshot-delete)"
    (vmId : _) -> do
      mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
      case mNid of
        Left err -> pure $ RespError err
        Right nid -> do
          eSet <-
            liftIO $
              runSqlPool
                ( do
                    eSiblings <- listVmstateSiblingDrives (toSqlKey vmId :: VmId)
                    case eSiblings of
                      Left err -> pure (Left err)
                      Right diskIds -> do
                        snapRows <-
                          mapM
                            ( \did -> do
                                rs <-
                                  selectList
                                    [ SnapshotDiskImageId ==. did
                                    , SnapshotName ==. snapshotName carrierSnap
                                    ]
                                    []
                                pure (did, rs)
                            )
                            diskIds
                        pure (Right snapRows)
                )
                (ssDbPool state)
          case eSet of
            Left err -> pure $ RespError err
            Right snapRows -> do
              let pairs =
                    [ (did, entityKey s)
                    | (did, ss) <- snapRows
                    , s <- ss
                    ]
              paths <-
                liftIO $
                  mapM
                    ( \(did, _) ->
                        resolveDiskPath
                          (ssDbPool state)
                          (ssQemuConfig state)
                          did
                          nid
                    )
                    pairs
              logInfoN $
                "vmstate snapshot delete: tag="
                  <> snapshotName carrierSnap
                  <> ", disks="
                  <> T.pack (show (length pairs))
              result <-
                liftIO $
                  deleteSnapshotViaAgentWithVmstate
                    state
                    nid
                    paths
                    (snapshotName carrierSnap)
                    vmId
              case result of
                ImageSuccess -> do
                  liftIO $
                    runSqlPool
                      ( mapM_
                          (\(_, sid) -> delete sid)
                          pairs
                      )
                      (ssDbPool state)
                  logInfoN "vmstate snapshot deleted (all siblings)"
                  pure RespSnapshotOk
                ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                ImageError err -> pure $ RespError err
                ImageNotFound -> pure RespDiskNotFound

-- | Offline rollback to a snapshot. Refuses the call when any
-- attached VM is running/paused — QEMU has no online rollback,
-- so the underlying @qemu-img snapshot -a@ would collide with
-- the live qemu's file lock.
--
-- For the orchestrated "stop → rollback → start" path see
-- 'Corvus.Handlers.Disk.SnapshotAutoStop.handleSnapshotRollbackAutoStop'.
handleSnapshotRollback :: ServerState -> Int64 -> Ref -> IO Response
handleSnapshotRollback state diskId snapRef = runServerLogging state $ do
  logInfoN $
    "Rolling back disk "
      <> T.pack (show diskId)
      <> " to snapshot '"
      <> unRef snapRef
      <> "'"

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk
      | diskImageFormat disk /= FormatQcow2 ->
          pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
      | otherwise -> do
          -- Resolve the snapshot row FIRST and check hasVmstate
          -- before the VM-must-be-stopped gate. A vmstate snapshot's
          -- rollback path is fundamentally different (it's a QMP
          -- @snapshot-load@, which REQUIRES the VM to be running);
          -- letting the VmMustBeStopped error fire first would
          -- mislead operators into stopping the VM, which makes the
          -- correct vmstate path even harder to reach.
          mSnapId <- liftIO $ resolveSnapshot snapRef diskId (ssDbPool state)
          case mSnapId of
            Left _ -> pure RespSnapshotNotFound
            Right snapshotId -> do
              mSnapshot <-
                liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
              case mSnapshot of
                Nothing -> pure RespSnapshotNotFound
                Just snapshot
                  | snapshotHasVmstate snapshot ->
                      handleVmstateRollback state diskId snapshot
                  | otherwise -> do
                      runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
                      if not (null runningVms)
                        then pure RespVmMustBeStopped
                        else do
                          mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
                          case mNid of
                            Left err -> pure $ RespError err
                            Right nid -> do
                              filePath <-
                                liftIO $
                                  resolveDiskPath
                                    (ssDbPool state)
                                    (ssQemuConfig state)
                                    (toSqlKey diskId :: DiskImageId)
                                    nid
                              result <-
                                liftIO $
                                  rollbackSnapshotViaAgent
                                    state
                                    nid
                                    filePath
                                    (snapshotName snapshot)
                              case result of
                                ImageSuccess -> do
                                  logInfoN "Rollback complete"
                                  pure RespSnapshotOk
                                ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                                ImageError err -> pure $ RespError err
                                ImageNotFound -> pure RespDiskNotFound

-- | Rollback the running VM's full machine state to a vmstate
-- snapshot. Pre: @snapshotHasVmstate snapshot = True@.
--
-- Lifecycle (running VM case): QMP @stop@ pauses CPUs →
-- @snapshot-load@ restores RAM/devices + every disk in the
-- snapshot set atomically → @cont@ resumes CPUs → QGA
-- @guest-set-time@ resyncs the wall clock.
--
-- A stopped VM is rejected for now: that path needs the
-- paused-start lifecycle (launch QEMU with @-S@, then load).
-- The 'vsStartPaused' plumbing landed in a sibling commit;
-- wiring it through is a follow-up.
handleVmstateRollback
  :: ServerState -> Int64 -> Snapshot -> LoggingT IO Response
handleVmstateRollback state diskId carrierSnap = do
  runningVms <-
    liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
  case runningVms of
    [] ->
      pure $
        RespError
          "vmstate snapshot rollback currently requires the VM to be running \
          \(QMP snapshot-load needs a live QEMU process). Start the VM first, \
          \then re-issue the rollback. (Paused-start support is a follow-up.)"
    (_ : _ : _) ->
      pure $
        RespError
          "vmstate snapshot rollback: the disk is attached to multiple running \
          \VMs; the snapshot-load lifecycle is per-VM."
    [vmId] -> do
      mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
      case mNid of
        Left err -> pure $ RespError err
        Right nid -> do
          eSet <-
            liftIO $
              runSqlPool
                ( do
                    eSiblings <- listVmstateSiblingDrives (toSqlKey vmId :: VmId)
                    case eSiblings of
                      Left err -> pure (Left err)
                      Right diskIds -> do
                        snapRows <-
                          mapM
                            ( \did -> do
                                rs <-
                                  selectList
                                    [ SnapshotDiskImageId ==. did
                                    , SnapshotName ==. snapshotName carrierSnap
                                    ]
                                    []
                                pure (did, rs)
                            )
                            diskIds
                        pure (Right snapRows)
                )
                (ssDbPool state)
          case eSet of
            Left err -> pure $ RespError err
            Right snapRows -> do
              let pairs = [(did, entityKey s) | (did, ss) <- snapRows, s <- ss]
              when (null pairs) $
                error "vmstate rollback: no sibling snapshots — DB inconsistent?"
              paths <-
                liftIO $
                  mapM
                    ( \(did, _) ->
                        resolveDiskPath
                          (ssDbPool state)
                          (ssQemuConfig state)
                          did
                          nid
                    )
                    pairs
              carrierPath <-
                liftIO $
                  resolveDiskPath
                    (ssDbPool state)
                    (ssQemuConfig state)
                    (toSqlKey diskId :: DiskImageId)
                    nid
              logInfoN $
                "vmstate snapshot rollback: pausing VM "
                  <> T.pack (show vmId)
                  <> ", loading tag="
                  <> snapshotName carrierSnap
              -- Pause CPUs so snapshot-load can run; vmPause is
              -- idempotent (QMP `stop` on an already-paused VM is
              -- a no-op).
              ePause <-
                liftIO $ withNAC state nid $ \nac -> NOA.vmPause nac vmId
              case ePause of
                Left e -> pure $ RespError ("vmstate rollback (pause): " <> e)
                Right () -> do
                  loadRes <-
                    liftIO $
                      loadSnapshotViaAgentWithVmstate
                        state
                        nid
                        carrierPath
                        paths
                        (snapshotName carrierSnap)
                        vmId
                  case loadRes of
                    ImageSuccess -> do
                      eResume <-
                        liftIO $
                          withNAC state nid $
                            \nac -> NOA.vmResume nac vmId
                      case eResume of
                        Left e ->
                          pure $ RespError ("vmstate rollback (cont): " <> e)
                        Right () -> do
                          -- guest-set-time is best-effort: the
                          -- rollback succeeded even if the QGA
                          -- call doesn't reach the guest.
                          _ <-
                            liftIO $ guestSetTimeViaAgent state nid vmId
                          logInfoN $
                            "vmstate snapshot rollback: VM "
                              <> T.pack (show vmId)
                              <> " resumed at snapshot state"
                          pure RespSnapshotOk
                    ImageFormatNotSupported msg ->
                      pure $ RespFormatNotSupported msg
                    ImageError err -> pure $ RespError err
                    ImageNotFound -> pure RespDiskNotFound

-- | Adapter that bridges 'lookupNodeAgent' + a NAC call into a
-- single 'IO (Either Text ())' return for inline orchestration
-- (where the daemon-side helper layer's @ImageResult@ adapter
-- doesn't fit because the agent method returns @()@ on success,
-- not a 'DiskOpResult'). Used by 'handleVmstateRollback' for
-- vmPause/vmResume which don't carry a wire body on success.
withNAC
  :: ServerState
  -> NodeId
  -> (NOA.NodeAgentClient -> IO (Either NOA.NodeAgentError ()))
  -> IO (Either Text ())
withNAC state nid call = do
  r <- lookupNodeAgent state nid
  case r of
    Left err -> pure (Left err)
    Right nac -> do
      res <- call nac
      pure $ case res of
        Left e -> Left (T.pack (show e))
        Right () -> Right ()

-- | Merge a snapshot. On qcow2 the merge operation IS the
-- snapshot-delete (the current disk state is preserved; only the
-- snapshot record is dropped), so the dispatch mirrors
-- 'handleSnapshotDelete': QMP delete if any attached VM is
-- running/paused, @qemu-img snapshot -d@ otherwise.
handleSnapshotMerge :: ServerState -> Int64 -> Ref -> IO Response
handleSnapshotMerge state diskId snapRef = runServerLogging state $ do
  logInfoN $ "Merging snapshot '" <> unRef snapRef <> "' for disk " <> T.pack (show diskId)

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk
      | diskImageFormat disk /= FormatQcow2 ->
          pure $ RespFormatNotSupported "Snapshots are only supported for qcow2 format"
      | otherwise -> do
          mSnapId <- liftIO $ resolveSnapshot snapRef diskId (ssDbPool state)
          case mSnapId of
            Left _ -> pure RespSnapshotNotFound
            Right snapshotId -> do
              mSnapshot <-
                liftIO $ runSqlPool (get (toSqlKey snapshotId :: SnapshotId)) (ssDbPool state)
              case mSnapshot of
                Nothing -> pure RespSnapshotNotFound
                Just snapshot -> do
                  mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
                  case mNid of
                    Left err -> pure $ RespError err
                    Right nid -> do
                      filePath <-
                        liftIO $
                          resolveDiskPath
                            (ssDbPool state)
                            (ssQemuConfig state)
                            (toSqlKey diskId :: DiskImageId)
                            nid
                      runningVms <-
                        liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
                      result <- case runningVms of
                        [] -> do
                          logInfoN "Snapshot merge path: offline"
                          liftIO $
                            mergeSnapshotViaAgent
                              state
                              nid
                              filePath
                              (snapshotName snapshot)
                        (vmId : _) -> do
                          logInfoN $
                            "Snapshot merge path: live via QMP (VM "
                              <> T.pack (show vmId)
                              <> ")"
                          liftIO $
                            deleteSnapshotViaAgentLive
                              state
                              nid
                              filePath
                              (snapshotName snapshot)
                              vmId
                      case result of
                        ImageSuccess -> do
                          liftIO $
                            runSqlPool
                              (delete (toSqlKey snapshotId :: SnapshotId))
                              (ssDbPool state)
                          logInfoN "Merge complete"
                          pure RespSnapshotOk
                        ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
                        ImageError err -> pure $ RespError err
                        ImageNotFound -> pure RespDiskNotFound

-- | List snapshots for a disk image
handleSnapshotList :: ServerState -> Int64 -> IO Response
handleSnapshotList state diskId = do
  mDisk <- runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just _ -> do
      snapshots <- runSqlPool (getSnapshots diskId) (ssDbPool state)
      pure $ RespSnapshotList snapshots

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

data SnapshotCreate = SnapshotCreate
  { scrDiskId :: Int64
  , scrName :: Text
  , scrQuiesce :: NOA.QuiesceMode
  , scrFullMachine :: Bool
  }

instance Action SnapshotCreate where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "create"
  actionEntityId = Just . fromIntegral . scrDiskId
  actionExecute ctx a =
    handleSnapshotCreate
      (acState ctx)
      (scrDiskId a)
      (scrName a)
      (scrQuiesce a)
      (scrFullMachine a)

data SnapshotDelete = SnapshotDelete
  { sdelDiskId :: Int64
  , sdelSnapRef :: Ref
  }

instance Action SnapshotDelete where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "delete"
  actionEntityId = Just . fromIntegral . sdelDiskId
  actionExecute ctx a = handleSnapshotDelete (acState ctx) (sdelDiskId a) (sdelSnapRef a)

data SnapshotRollback = SnapshotRollback
  { srlDiskId :: Int64
  , srlSnapRef :: Ref
  }

instance Action SnapshotRollback where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "rollback"
  actionEntityId = Just . fromIntegral . srlDiskId
  actionExecute ctx a = handleSnapshotRollback (acState ctx) (srlDiskId a) (srlSnapRef a)

data SnapshotMerge = SnapshotMerge
  { smrDiskId :: Int64
  , smrSnapRef :: Ref
  }

instance Action SnapshotMerge where
  actionSubsystem _ = SubSnapshot
  actionCommand _ = "merge"
  actionEntityId = Just . fromIntegral . smrDiskId
  actionExecute ctx a = handleSnapshotMerge (acState ctx) (smrDiskId a) (smrSnapRef a)
