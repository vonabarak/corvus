{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | DiskManager + Disk + Snapshot cap implementations.
--
-- Phase 4 lands the synchronous lifecycle (create / register /
-- delete / resize). The async / streaming ones (import-url,
-- create-overlay, clone, rebase) are deferred to Phase 6 where
-- they'll be wired up alongside task progress sinks.
module Corvus.Rpc.Disk
  ( DiskManagerCap (..)
  , DiskCap (..)
  , SnapshotCap (..)
  , newDiskManagerCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Disk as CGDisk
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer, methodUnimplemented)
import Corvus.Action (runAction, runActionAsyncWithId)
import Corvus.Handlers.Disk
  ( DiskClone (..)
  , DiskCopy (..)
  , DiskCreate (..)
  , DiskCreateOverlay (..)
  , DiskDelete (..)
  , DiskImportAction (..)
  , DiskImportUrl (..)
  , DiskMove (..)
  , DiskRebase (..)
  , DiskRefresh (..)
  , DiskRegister (..)
  , DiskResize (..)
  , handleDiskList
  , handleDiskShow
  )
import Corvus.Handlers.Disk.Snapshot
  ( SnapshotCreate (..)
  , SnapshotDelete (..)
  , SnapshotMerge (..)
  , SnapshotRollback (..)
  , handleSnapshotList
  )
import Corvus.Handlers.Resolve (resolveDisk, resolveNode, resolveSnapshot)
import Corvus.Protocol (Response (..))
import qualified Corvus.Protocol as P
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft, handleParsed)
import Corvus.Types (ServerState (..))
import Corvus.Wire.Disk (toCapnpDiskImageInfo, toCapnpSnapshotInfo)
import Corvus.Wire.Enums (fromCapnpDriveFormat)
import Data.Int (Int64)
import qualified Data.Text as T
import Supervisors (Supervisor)

-- ---------------------------------------------------------------------
-- Manager
-- ---------------------------------------------------------------------

data DiskManagerCap = DiskManagerCap
  { dmState :: !ServerState
  , dmSup :: !Supervisor
  , dmClientName :: !T.Text
  }

newDiskManagerCap :: ServerState -> Supervisor -> T.Text -> IO DiskManagerCap
newDiskManagerCap st sup cn = pure (DiskManagerCap st sup cn)

instance SomeServer DiskManagerCap

instance CGDisk.DiskManager'server_ DiskManagerCap where
  diskManager'list (DiskManagerCap st _ cn) = handleParsed $ \_ -> do
    resp <- handleDiskList st
    case resp of
      RespDiskList disks ->
        pure CGDisk.DiskManager'list'results {CGDisk.disks = map toCapnpDiskImageInfo disks}
      RespError msg -> throwFailed msg
      _ -> throwFailed "diskManager'list: unexpected response"

  diskManager'get (DiskManagerCap st sup cn) =
    handleParsed $ \CGDisk.DiskManager'get'params {..} -> do
      ref' <- capnpRefToRef ref
      eid <- failOnLeft =<< resolveDisk ref' (ssDbPool st)
      client <- export @CGDisk.Disk sup (DiskCap st sup eid cn)
      pure CGDisk.DiskManager'get'results {CGDisk.disk = client}

  diskManager'create (DiskManagerCap st sup cn) =
    handleParsed $ \CGDisk.DiskManager'create'params {params = CGDisk.DiskCreateParams {..}} -> do
      fmt <- enumOrThrow (fromCapnpDriveFormat format)
      let act =
            DiskCreate
              { dcrName = name
              , dcrFormat = fmt
              , dcrSizeMb = sizeMb
              , dcrPath = Nothing
              , dcrEphemeral = ephemeral
              }
      resp <- runAction st cn act
      case resp of
        RespDiskCreated newId -> do
          client <- export @CGDisk.Disk sup (DiskCap st sup newId cn)
          pure CGDisk.DiskManager'create'results {CGDisk.disk = client}
        RespError msg -> throwFailed msg
        _ -> throwFailed (T.pack ("diskManager'create: unexpected response: " <> show resp))

  diskManager'register (DiskManagerCap st sup cn) =
    handleParsed $ \CGDisk.DiskManager'register'params {params = CGDisk.DiskRegisterParams {..}} -> do
      fmt <- enumOrThrow (fromCapnpDriveFormat format)
      let act =
            DiskRegister
              { drgName = name
              , drgPath = filePath
              , drgFormat = Just fmt
              , drgBackingDiskId = Nothing
              , drgEphemeral = ephemeral
              }
      resp <- runAction st cn act
      case resp of
        RespDiskCreated newId -> do
          client <- export @CGDisk.Disk sup (DiskCap st sup newId cn)
          pure CGDisk.DiskManager'register'results {CGDisk.disk = client}
        RespError msg -> throwFailed msg
        _ -> throwFailed "diskManager'register: unexpected response"

  diskManager'createOverlay (DiskManagerCap st sup cn) =
    handleParsed $ \CGDisk.DiskManager'createOverlay'params {params = CGDisk.DiskCreateOverlayParams {..}} -> do
      baseRef' <- capnpRefToRef backingDiskRef
      baseId <- failOnLeft =<< resolveDisk baseRef' (ssDbPool st)
      let act =
            DiskCreateOverlay
              { dcoName = name
              , dcoBaseDiskId = baseId
              , dcoResizeMb = Nothing
              , dcoPath = Nothing
              , dcoEphemeral = ephemeral
              }
      resp <- runAction st cn act
      case resp of
        RespDiskCreated newId -> do
          client <- export @CGDisk.Disk sup (DiskCap st sup newId cn)
          pure CGDisk.DiskManager'createOverlay'results {CGDisk.disk = client}
        RespError msg -> throwFailed msg
        _ -> throwFailed "diskManager'createOverlay: unexpected response"

  diskManager'clone (DiskManagerCap st sup cn) =
    handleParsed $ \CGDisk.DiskManager'clone'params {params = CGDisk.DiskCloneParams {..}} -> do
      srcRef' <- capnpRefToRef sourceRef
      srcId <- failOnLeft =<< resolveDisk srcRef' (ssDbPool st)
      -- Empty `path` means "let the daemon pick the default
      -- location"; non-empty is forwarded verbatim (the handler
      -- accepts both relative-to-basePath and absolute paths).
      let mPath = if T.null path then Nothing else Just path
          act =
            DiskClone
              { dclName = newName
              , dclBaseDiskId = srcId
              , dclResizeMb = Nothing
              , dclPath = mPath
              , dclEphemeral = ephemeral
              }
      resp <- runAction st cn act
      case resp of
        RespDiskCreated newId -> do
          client <- export @CGDisk.Disk sup (DiskCap st sup newId cn)
          pure CGDisk.DiskManager'clone'results {CGDisk.disk = client}
        RespVmMustBeStopped -> throwFailed "VM must be stopped"
        RespError msg -> throwFailed msg
        _ -> throwFailed "diskManager'clone: unexpected response"

  diskManager'rebase (DiskManagerCap st _ cn) =
    handleParsed $ \CGDisk.DiskManager'rebase'params {params = CGDisk.DiskRebaseParams {..}} -> do
      diskRef' <- capnpRefToRef diskRef
      diskId' <- failOnLeft =<< resolveDisk diskRef' (ssDbPool st)
      backingRef' <- capnpRefToRef newBackingDiskRef
      backingId <- failOnLeft =<< resolveDisk backingRef' (ssDbPool st)
      let act =
            DiskRebase
              { drbDiskId = diskId'
              , drbNewBackingId = Just backingId
              , drbUnsafe = False
              }
      resp <- runAction st cn act
      case resp of
        RespDiskOk -> pure CGDisk.DiskManager'rebase'results
        RespVmMustBeStopped -> throwFailed "VM must be stopped"
        RespError msg -> throwFailed msg
        _ -> throwFailed "diskManager'rebase: unexpected response"

  diskManager'flatten (DiskManagerCap st _ cn) =
    handleParsed $ \CGDisk.DiskManager'flatten'params {..} -> do
      diskRef' <- capnpRefToRef diskRef
      diskId' <- failOnLeft =<< resolveDisk diskRef' (ssDbPool st)
      -- @drbNewBackingId = Nothing@ is the flatten signal in the
      -- daemon's @DiskRebase@ action (`Handlers/Disk/Rebase.hs`).
      let act =
            DiskRebase
              { drbDiskId = diskId'
              , drbNewBackingId = Nothing
              , drbUnsafe = False
              }
      resp <- runAction st cn act
      case resp of
        RespDiskOk -> pure CGDisk.DiskManager'flatten'results
        RespVmMustBeStopped -> throwFailed "VM must be stopped"
        RespError msg -> throwFailed msg
        _ -> throwFailed "diskManager'flatten: unexpected response"

  diskManager'importUrl (DiskManagerCap st _ cn) =
    handleParsed $ \CGDisk.DiskManager'importUrl'params {params = CGDisk.DiskImportUrlParams {..}} -> do
      let act =
            DiskImportUrl
              { diuName = name
              , diuUrl = url
              , diuFormat = Nothing
              , diuEphemeral = ephemeral
              }
      resp <- runActionAsyncWithId st cn act RespDiskImportStarted
      case resp of
        RespDiskImportStarted tid ->
          pure CGDisk.DiskManager'importUrl'results {CGDisk.taskId = tid}
        RespError msg -> throwFailed msg
        _ -> throwFailed "diskManager'importUrl: unexpected response"

  diskManager'import_ (DiskManagerCap st sup cn) =
    handleParsed $ \CGDisk.DiskManager'import'params {params = CGDisk.DiskImportParams {..}} -> do
      let act =
            DiskImportAction
              { diaName = name
              , diaSource = srcPath
              , diaDestPath = Nothing
              , diaFormat = Nothing
              , diaMd5 = Nothing
              , diaEphemeral = ephemeral
              }
      resp <- runAction st cn act
      case resp of
        RespDiskCreated newId -> do
          client <- export @CGDisk.Disk sup (DiskCap st sup newId cn)
          pure CGDisk.DiskManager'import'results {CGDisk.disk = client}
        RespError msg -> throwFailed msg
        _ -> throwFailed "diskManager'import: unexpected response"

  diskManager'copy (DiskManagerCap st _ cn) =
    handleParsed $ \CGDisk.DiskManager'copy'params {params = CGDisk.DiskCopyParams {..}} -> do
      dr <- capnpRefToRef diskRef
      diskId <- failOnLeft =<< resolveDisk dr (ssDbPool st)
      nr <- capnpRefToRef toNodeRef
      nodeId <- failOnLeft =<< resolveNode nr (ssDbPool st)
      let act = DiskCopy {dcpDiskId = diskId, dcpDestNodeId = nodeId}
      resp <- runActionAsyncWithId st cn act RespDiskTransferStarted
      case resp of
        RespDiskTransferStarted tid ->
          pure CGDisk.DiskManager'copy'results {CGDisk.taskId = tid}
        RespError msg -> throwFailed msg
        _ -> throwFailed "diskManager'copy: unexpected response"

  diskManager'move (DiskManagerCap st _ cn) =
    handleParsed $ \CGDisk.DiskManager'move'params {params = CGDisk.DiskMoveParams {..}} -> do
      dr <- capnpRefToRef diskRef
      diskId <- failOnLeft =<< resolveDisk dr (ssDbPool st)
      nr <- capnpRefToRef toNodeRef
      nodeId <- failOnLeft =<< resolveNode nr (ssDbPool st)
      let act = DiskMove {dmvDiskId = diskId, dmvDestNodeId = nodeId}
      resp <- runActionAsyncWithId st cn act RespDiskTransferStarted
      case resp of
        RespDiskTransferStarted tid ->
          pure CGDisk.DiskManager'move'results {CGDisk.taskId = tid}
        RespError msg -> throwFailed msg
        _ -> throwFailed "diskManager'move: unexpected response"

-- ---------------------------------------------------------------------
-- Disk resource cap
-- ---------------------------------------------------------------------

data DiskCap = DiskCap
  { dskState :: !ServerState
  , dskSup :: !Supervisor
  , dskId :: !Int64
  , dskClientName :: !T.Text
  }

instance SomeServer DiskCap

instance CGDisk.Disk'server_ DiskCap where
  disk'show (DiskCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleDiskShow st eid
    case resp of
      RespDiskInfo info ->
        pure CGDisk.Disk'show'results {CGDisk.info = toCapnpDiskImageInfo info}
      RespDiskNotFound -> throwFailed "Disk not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "disk'show: unexpected response"

  disk'delete (DiskCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (DiskDelete eid)
    case resp of
      RespDiskOk -> pure CGDisk.Disk'delete'results
      RespDiskNotFound -> throwFailed "Disk not found"
      RespDiskInUse _ -> throwFailed "Disk in use"
      RespDiskHasOverlays _ -> throwFailed "Disk has overlays"
      RespError msg -> throwFailed msg
      _ -> throwFailed "disk'delete: unexpected response"

  disk'resize (DiskCap st _ eid cn) = handleParsed $ \CGDisk.Disk'resize'params {..} -> do
    resp <- runAction st cn (DiskResize {drzDiskId = eid, drzNewSizeMb = newSizeMb})
    case resp of
      RespDiskOk -> pure CGDisk.Disk'resize'results
      RespDiskNotFound -> throwFailed "Disk not found"
      RespVmMustBeStopped -> throwFailed "VM must be stopped"
      RespError msg -> throwFailed msg
      _ -> throwFailed "disk'resize: unexpected response"

  disk'snapshotCreate (DiskCap st sup eid cn) =
    handleParsed $ \CGDisk.Disk'snapshotCreate'params {..} -> do
      resp <- runAction st cn (SnapshotCreate {scrDiskId = eid, scrName = name})
      case resp of
        RespSnapshotCreated sid -> do
          client <- export @CGDisk.Snapshot sup (SnapshotCap st eid sid cn)
          pure CGDisk.Disk'snapshotCreate'results {CGDisk.snapshot = client}
        RespDiskNotFound -> throwFailed "Disk not found"
        RespVmMustBeStopped -> throwFailed "VM must be stopped"
        RespError msg -> throwFailed msg
        _ -> throwFailed "disk'snapshotCreate: unexpected response"

  disk'snapshotList (DiskCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleSnapshotList st eid
    case resp of
      RespSnapshotList snaps ->
        pure CGDisk.Disk'snapshotList'results {CGDisk.snapshots = map toCapnpSnapshotInfo snaps}
      RespDiskNotFound -> throwFailed "Disk not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "disk'snapshotList: unexpected response"

  disk'snapshotGet (DiskCap st sup eid cn) =
    handleParsed $ \CGDisk.Disk'snapshotGet'params {..} -> do
      ref' <- capnpRefToRef ref
      sid <- failOnLeft =<< resolveSnapshot ref' eid (ssDbPool st)
      client <- export @CGDisk.Snapshot sup (SnapshotCap st eid sid cn)
      pure CGDisk.Disk'snapshotGet'results {CGDisk.snapshot = client}

  disk'refresh (DiskCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (DiskRefresh eid)
    case resp of
      RespDiskInfo info ->
        pure CGDisk.Disk'refresh'results {CGDisk.info = toCapnpDiskImageInfo info}
      RespDiskOk -> do
        -- some refresh paths return RespDiskOk; re-fetch
        info <- handleDiskShow st eid
        case info of
          RespDiskInfo i ->
            pure CGDisk.Disk'refresh'results {CGDisk.info = toCapnpDiskImageInfo i}
          _ -> throwFailed "disk'refresh: unable to fetch info"
      RespDiskNotFound -> throwFailed "Disk not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "disk'refresh: unexpected response"

-- ---------------------------------------------------------------------
-- Snapshot resource cap
-- ---------------------------------------------------------------------

data SnapshotCap = SnapshotCap
  { _snState :: !ServerState
  , _snDiskId :: !Int64
  , _snId :: !Int64
  , _snClientName :: !T.Text
  }

instance SomeServer SnapshotCap

instance CGDisk.Snapshot'server_ SnapshotCap where
  snapshot'show (SnapshotCap st diskId sid cn) = handleParsed $ \_ -> do
    resp <- handleSnapshotList st diskId
    case resp of
      RespSnapshotList snaps ->
        case filter ((== sid) . P.sniId) snaps of
          (s : _) ->
            pure CGDisk.Snapshot'show'results {CGDisk.info = toCapnpSnapshotInfo s}
          [] -> throwFailed "Snapshot not found"
      RespDiskNotFound -> throwFailed "Disk not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "snapshot'show: unexpected response"
  snapshot'delete (SnapshotCap st diskId sid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (SnapshotDelete {sdelDiskId = diskId, sdelSnapRef = P.Ref (T.pack (show sid))})
    case resp of
      RespSnapshotOk -> pure CGDisk.Snapshot'delete'results
      RespSnapshotNotFound -> throwFailed "Snapshot not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "snapshot'delete: unexpected response"
  snapshot'rollback (SnapshotCap st diskId sid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (SnapshotRollback {srlDiskId = diskId, srlSnapRef = P.Ref (T.pack (show sid))})
    case resp of
      RespSnapshotOk -> pure CGDisk.Snapshot'rollback'results
      RespSnapshotNotFound -> throwFailed "Snapshot not found"
      RespVmMustBeStopped -> throwFailed "VM must be stopped"
      RespError msg -> throwFailed msg
      _ -> throwFailed "snapshot'rollback: unexpected response"
  snapshot'merge (SnapshotCap st diskId sid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (SnapshotMerge {smrDiskId = diskId, smrSnapRef = P.Ref (T.pack (show sid))})
    case resp of
      RespSnapshotOk -> pure CGDisk.Snapshot'merge'results
      RespSnapshotNotFound -> throwFailed "Snapshot not found"
      RespVmMustBeStopped -> throwFailed "VM must be stopped"
      RespError msg -> throwFailed msg
      _ -> throwFailed "snapshot'merge: unexpected response"

-- ---------------------------------------------------------------------
-- Helper
-- ---------------------------------------------------------------------

enumOrThrow :: Either e a -> IO a
enumOrThrow (Right a) = pure a
enumOrThrow (Left _) = throwFailed "unknown enum tag in request"
