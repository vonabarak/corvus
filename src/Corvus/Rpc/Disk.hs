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
import Capnp.Rpc.Server (SomeServer, handleParsed, methodUnimplemented)
import Corvus.Action (runAction)
import Corvus.Handlers.Disk
  ( DiskCreate (..)
  , DiskDelete (..)
  , DiskRegister (..)
  , DiskResize (..)
  , handleDiskList
  , handleDiskShow
  )
import Corvus.Handlers.Disk.Snapshot (SnapshotCreate (..), SnapshotDelete (..), handleSnapshotList)
import Corvus.Handlers.Resolve (resolveDisk, resolveSnapshot)
import Corvus.Protocol (Response (..))
import qualified Corvus.Protocol as P
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft)
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
  }

newDiskManagerCap :: ServerState -> Supervisor -> IO DiskManagerCap
newDiskManagerCap st sup = pure (DiskManagerCap st sup)

instance SomeServer DiskManagerCap

instance CGDisk.DiskManager'server_ DiskManagerCap where
  diskManager'list (DiskManagerCap st _) = handleParsed $ \_ -> do
    resp <- handleDiskList st
    case resp of
      RespDiskList disks ->
        pure CGDisk.DiskManager'list'results {CGDisk.disks = map toCapnpDiskImageInfo disks}
      RespError msg -> throwFailed msg
      _ -> throwFailed "diskManager'list: unexpected response"

  diskManager'get (DiskManagerCap st sup) =
    handleParsed $ \CGDisk.DiskManager'get'params {..} -> do
      ref' <- capnpRefToRef ref
      eid <- failOnLeft =<< resolveDisk ref' (ssDbPool st)
      client <- export @CGDisk.Disk sup (DiskCap st sup eid)
      pure CGDisk.DiskManager'get'results {CGDisk.disk = client}

  diskManager'create (DiskManagerCap st sup) =
    handleParsed $ \CGDisk.DiskManager'create'params {params = CGDisk.DiskCreateParams {..}} -> do
      fmt <- enumOrThrow (fromCapnpDriveFormat format)
      let act =
            DiskCreate
              { dcrName = name
              , dcrFormat = fmt
              , dcrSizeMb = sizeMb
              , dcrPath = Nothing
              }
      resp <- runAction st act
      case resp of
        RespDiskCreated newId -> do
          client <- export @CGDisk.Disk sup (DiskCap st sup newId)
          pure CGDisk.DiskManager'create'results {CGDisk.disk = client}
        RespError msg -> throwFailed msg
        _ -> throwFailed (T.pack ("diskManager'create: unexpected response: " <> show resp))

  diskManager'register (DiskManagerCap st sup) =
    handleParsed $ \CGDisk.DiskManager'register'params {params = CGDisk.DiskRegisterParams {..}} -> do
      fmt <- enumOrThrow (fromCapnpDriveFormat format)
      let act =
            DiskRegister
              { drgName = name
              , drgPath = filePath
              , drgFormat = Just fmt
              , drgBackingDiskId = Nothing
              }
      resp <- runAction st act
      case resp of
        RespDiskCreated newId -> do
          client <- export @CGDisk.Disk sup (DiskCap st sup newId)
          pure CGDisk.DiskManager'register'results {CGDisk.disk = client}
        RespError msg -> throwFailed msg
        _ -> throwFailed "diskManager'register: unexpected response"

  -- Async / overlay / clone / rebase land in Phase 6.
  diskManager'createOverlay _ = methodUnimplemented
  diskManager'clone _ = methodUnimplemented
  diskManager'rebase _ = methodUnimplemented
  diskManager'importUrl _ = methodUnimplemented
  diskManager'import_ _ = methodUnimplemented

-- ---------------------------------------------------------------------
-- Disk resource cap
-- ---------------------------------------------------------------------

data DiskCap = DiskCap
  { dskState :: !ServerState
  , dskSup :: !Supervisor
  , dskId :: !Int64
  }

instance SomeServer DiskCap

instance CGDisk.Disk'server_ DiskCap where
  disk'show (DiskCap st _ eid) = handleParsed $ \_ -> do
    resp <- handleDiskShow st eid
    case resp of
      RespDiskInfo info ->
        pure CGDisk.Disk'show'results {CGDisk.info = toCapnpDiskImageInfo info}
      RespDiskNotFound -> throwFailed "Disk not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "disk'show: unexpected response"

  disk'delete (DiskCap st _ eid) = handleParsed $ \_ -> do
    resp <- runAction st (DiskDelete eid)
    case resp of
      RespDiskOk -> pure CGDisk.Disk'delete'results
      RespDiskNotFound -> throwFailed "Disk not found"
      RespDiskInUse _ -> throwFailed "Disk in use"
      RespDiskHasOverlays _ -> throwFailed "Disk has overlays"
      RespError msg -> throwFailed msg
      _ -> throwFailed "disk'delete: unexpected response"

  disk'resize (DiskCap st _ eid) = handleParsed $ \CGDisk.Disk'resize'params {..} -> do
    resp <- runAction st (DiskResize {drzDiskId = eid, drzNewSizeMb = newSizeMb})
    case resp of
      RespDiskOk -> pure CGDisk.Disk'resize'results
      RespDiskNotFound -> throwFailed "Disk not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "disk'resize: unexpected response"

  disk'snapshotCreate (DiskCap st sup eid) =
    handleParsed $ \CGDisk.Disk'snapshotCreate'params {..} -> do
      resp <- runAction st (SnapshotCreate {scrDiskId = eid, scrName = name})
      case resp of
        RespSnapshotCreated sid -> do
          client <- export @CGDisk.Snapshot sup (SnapshotCap st eid sid)
          pure CGDisk.Disk'snapshotCreate'results {CGDisk.snapshot = client}
        RespDiskNotFound -> throwFailed "Disk not found"
        RespError msg -> throwFailed msg
        _ -> throwFailed "disk'snapshotCreate: unexpected response"

  disk'snapshotList (DiskCap st _ eid) = handleParsed $ \_ -> do
    resp <- handleSnapshotList st eid
    case resp of
      RespSnapshotList snaps ->
        pure CGDisk.Disk'snapshotList'results {CGDisk.snapshots = map toCapnpSnapshotInfo snaps}
      RespDiskNotFound -> throwFailed "Disk not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "disk'snapshotList: unexpected response"

  disk'snapshotGet (DiskCap st _ eid) =
    handleParsed $ \CGDisk.Disk'snapshotGet'params {..} -> do
      ref' <- capnpRefToRef ref
      sid <- failOnLeft =<< resolveSnapshot ref' eid (ssDbPool st)
      -- Snapshots don't have a manager pool, so we hand back a
      -- cap directly. The cap stays alive for the duration of the
      -- client session.
      throwFailed (T.pack ("disk'snapshotGet stub returns id " <> show sid))

  disk'refresh _ = methodUnimplemented

-- ---------------------------------------------------------------------
-- Snapshot resource cap
-- ---------------------------------------------------------------------

data SnapshotCap = SnapshotCap
  { _snState :: !ServerState
  , _snDiskId :: !Int64
  , _snId :: !Int64
  }

instance SomeServer SnapshotCap

instance CGDisk.Snapshot'server_ SnapshotCap where
  snapshot'show _ = methodUnimplemented
  snapshot'delete (SnapshotCap st diskId sid) = handleParsed $ \_ -> do
    resp <- runAction st (SnapshotDelete {sdelDiskId = diskId, sdelSnapRef = P.Ref (T.pack (show sid))})
    case resp of
      RespSnapshotOk -> pure CGDisk.Snapshot'delete'results
      RespSnapshotNotFound -> throwFailed "Snapshot not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "snapshot'delete: unexpected response"
  snapshot'rollback _ = methodUnimplemented
  snapshot'merge _ = methodUnimplemented

-- ---------------------------------------------------------------------
-- Helper
-- ---------------------------------------------------------------------

enumOrThrow :: Either e a -> IO a
enumOrThrow (Right a) = pure a
enumOrThrow (Left _) = throwFailed "unknown enum tag in request"
