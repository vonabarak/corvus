{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Disk-related RPC wrappers extracted from "Corvus.Client.Capnp.Rpc".
module Corvus.Client.Capnp.Rpc.Disk
  ( -- * Disk read methods
    rpcDiskList
  , rpcDiskShow

    -- * Disk lifecycle
  , rpcDiskCreate
  , rpcDiskDelete
  , rpcDiskResize

    -- * Disk additional wrappers
  , rpcDiskCreateOverlay
  , rpcDiskRegister
  , rpcDiskRefresh
  , rpcDiskImport
  , rpcDiskUpload
  , rpcDiskClone
  , rpcDiskRebase
  , rpcDiskFlatten
  , rpcDiskCopy
  , rpcDiskMove
  , rpcDiskAttach
  , rpcDiskDetach
  , rpcDiskDetachByDisk
  , rpcDiskMediaEject
  , rpcDiskMediaChange

    -- * Snapshot operations (per-disk)
  , rpcSnapshotList
  , rpcSnapshotCreate
  , rpcSnapshotDelete
  , rpcSnapshotRollback
  , rpcSnapshotMerge

    -- * Helpers
  , getDiskClient
  , capnpDriveFormat
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Common as CGCommon
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Disk as CGDisk
import qualified Capnp.Gen.Enums as CGE
import qualified Capnp.Gen.Vm as CGVm
import Control.Exception (SomeException, try)
import qualified Control.Monad
import Corvus.Client.Capnp.Connection (CapnpConnection (..))
import Corvus.Client.Capnp.Rpc.Vm (getVmClient, rpcVmShow)
import Corvus.Model
  ( CacheType
  , DriveFormat
  , DriveInterface
  , DriveMedia (..)
  )
import qualified Corvus.Protocol.Disk as PD
import Corvus.Protocol.NamedRef (nrId, nrName)
import qualified Corvus.Protocol.Vm as PV
import Corvus.Wire.Common (EntityRef, entityRefFromText, toCapnpEntityRef)
import qualified Corvus.Wire.Common as WC
import qualified Corvus.Wire.Disk as WDisk
import Corvus.Wire.Enums
  ( toCapnpCacheType
  , toCapnpDriveFormat
  , toCapnpDriveInterface
  , toCapnpDriveMedia
  )
import Corvus.Wire.Errors (WireError, showWireError)
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.Int (Int64)
import qualified Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (IOMode (ReadMode), withBinaryFile)

-- | Call a method on a cap and return its parsed results struct.
callOn
  :: ( C.IsCap iface
     , C.IsStruct params
     , C.IsStruct results
     , C.Parse params (C.Parsed params)
     , C.Parse results (C.Parsed results)
     )
  => C.Method iface params results
  -> C.Parsed params
  -> C.Client iface
  -> IO (C.Parsed results)
callOn method p client = do
  raw <- (client & C.callP method p) >>= C.waitPipeline
  C.evalLimitT C.defaultLimit (C.parse raw)

failOnWire :: Either WireError a -> IO a
failOnWire (Right a) = pure a
failOnWire (Left e) = fail ("wire decode error: " <> show (showWireError e))

-- ---------------------------------------------------------------------
-- Disk read methods
-- ---------------------------------------------------------------------

rpcDiskList :: CapnpConnection -> IO [PD.DiskImageInfo]
rpcDiskList conn = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  CGDisk.DiskManager'list'results {CGDisk.disks = ds} <-
    callOn #list CGDisk.DiskManager'list'params mgr
  traverse (failOnWire . WDisk.fromCapnpDiskImageInfo) ds

rpcDiskShow :: CapnpConnection -> EntityRef -> IO PD.DiskImageInfo
rpcDiskShow conn refIn = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  CGDisk.DiskManager'get'results {CGDisk.disk = dClient} <-
    callOn #get CGDisk.DiskManager'get'params {CGDisk.ref = toCapnpEntityRef refIn} mgr
  CGDisk.Disk'show'results {CGDisk.info = info} <-
    callOn #show CGDisk.Disk'show'params dClient
  failOnWire (WDisk.fromCapnpDiskImageInfo info)

-- ---------------------------------------------------------------------
-- Disk lifecycle
-- ---------------------------------------------------------------------

rpcDiskCreate
  :: CapnpConnection
  -> Text
  -- ^ name
  -> Int64
  -- ^ size (MB)
  -> CGE.DriveFormat
  -- ^ format (wire-side enum)
  -> Maybe Text
  -- ^ optional destination path
  -> Bool
  -- ^ ephemeral
  -> EntityRef
  -- ^ target node (unset = scheduler picks)
  -> IO Int64
rpcDiskCreate conn name sizeMb fmt mPath ephemeral nodeRef = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let inner =
        CGDisk.DiskCreateParams
          { CGDisk.name = name
          , CGDisk.sizeMb = sizeMb
          , CGDisk.format = fmt
          , CGDisk.ephemeral = ephemeral
          , CGDisk.node = toCapnpEntityRef nodeRef
          , CGDisk.path = Data.Maybe.fromMaybe "" mPath
          }
  CGDisk.DiskManager'create'results {CGDisk.disk = diskClient} <-
    callOn #create CGDisk.DiskManager'create'params {CGDisk.params = inner} mgr
  CGDisk.Disk'show'results {CGDisk.info = info} <-
    callOn #show CGDisk.Disk'show'params diskClient
  case info of CGDisk.DiskImageInfo {CGDisk.id = did} -> pure did

rpcDiskDelete :: CapnpConnection -> EntityRef -> IO ()
rpcDiskDelete conn ref = do
  diskClient <- getDiskClient conn ref
  _ <- callOn #delete CGDisk.Disk'delete'params diskClient
  pure ()

rpcDiskResize :: CapnpConnection -> EntityRef -> Int64 -> IO ()
rpcDiskResize conn ref newSizeMb = do
  diskClient <- getDiskClient conn ref
  _ <- callOn #resize CGDisk.Disk'resize'params {CGDisk.newSizeMb = newSizeMb} diskClient
  pure ()

-- ---------------------------------------------------------------------
-- Disk additional wrappers
-- ---------------------------------------------------------------------

rpcDiskCreateOverlay :: CapnpConnection -> Text -> EntityRef -> Maybe Text -> Bool -> IO Int64
rpcDiskCreateOverlay conn name baseRef mPath ephemeral = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let inner =
        CGDisk.DiskCreateOverlayParams
          { CGDisk.name = name
          , CGDisk.backingDiskRef = toCapnpEntityRef baseRef
          , CGDisk.ephemeral = ephemeral
          , CGDisk.path = Data.Maybe.fromMaybe "" mPath
          }
  CGDisk.DiskManager'createOverlay'results {CGDisk.disk = dClient} <-
    callOn #createOverlay CGDisk.DiskManager'createOverlay'params {CGDisk.params = inner} mgr
  CGDisk.Disk'show'results {CGDisk.info = info} <-
    callOn #show CGDisk.Disk'show'params dClient
  case info of CGDisk.DiskImageInfo {CGDisk.id = did} -> pure did

rpcDiskRegister
  :: CapnpConnection
  -> Text
  -> Text
  -> Maybe DriveFormat
  -> Maybe EntityRef
  -> Bool
  -> EntityRef
  -> IO Int64
rpcDiskRegister conn name filePath mFormat mBackingRef ephemeral nodeRef = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let p =
        CGDisk.DiskRegisterParams
          { CGDisk.name = name
          , CGDisk.filePath = filePath
          , CGDisk.format = maybe CGE.DriveFormat'qcow2 capnpDriveFormat mFormat
          , CGDisk.ephemeral = ephemeral
          , CGDisk.node = toCapnpEntityRef nodeRef
          , CGDisk.formatProvided = Data.Maybe.isJust mFormat
          , CGDisk.backingDiskRef = maybe emptyCapnpEntityRef toCapnpEntityRef mBackingRef
          , CGDisk.backingProvided = Data.Maybe.isJust mBackingRef
          }
  CGDisk.DiskManager'register'results {CGDisk.disk = dClient} <-
    callOn #register CGDisk.DiskManager'register'params {CGDisk.params = p} mgr
  CGDisk.Disk'show'results {CGDisk.info = info} <-
    callOn #show CGDisk.Disk'show'params dClient
  case info of CGDisk.DiskImageInfo {CGDisk.id = did} -> pure did

rpcDiskRefresh :: CapnpConnection -> EntityRef -> IO PD.DiskImageInfo
rpcDiskRefresh conn ref = do
  dClient <- getDiskClient conn ref
  CGDisk.Disk'refresh'results {CGDisk.info = info} <-
    callOn #refresh CGDisk.Disk'refresh'params dClient
  failOnWire (WDisk.fromCapnpDiskImageInfo info)

rpcDiskImport
  :: CapnpConnection
  -> Text
  -> Text
  -> Maybe Text
  -> Maybe DriveFormat
  -> Bool
  -> EntityRef
  -> IO Int64
rpcDiskImport conn name srcPath mDestPath mFormat ephemeral nodeRef = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let p =
        CGDisk.DiskImportParams
          { CGDisk.name = name
          , CGDisk.srcPath = srcPath
          , CGDisk.format = maybe CGE.DriveFormat'qcow2 capnpDriveFormat mFormat
          , CGDisk.ephemeral = ephemeral
          , CGDisk.node = toCapnpEntityRef nodeRef
          , CGDisk.destPath = Data.Maybe.fromMaybe "" mDestPath
          , CGDisk.formatProvided = Data.Maybe.isJust mFormat
          }
  CGDisk.DiskManager'import'results {CGDisk.taskId = tid} <-
    callOn #import_ CGDisk.DiskManager'import'params {CGDisk.params = p} mgr
  pure tid

rpcDiskUpload
  :: CapnpConnection
  -> Text
  -> FilePath
  -> DriveFormat
  -> Maybe Text
  -> Bool
  -> EntityRef
  -> Bool
  -> IO Int64
rpcDiskUpload conn name source fmt mPath ephemeral nodeRef overwrite = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let params =
        CGDisk.DiskUploadParams
          { CGDisk.name = name
          , CGDisk.format = toCapnpDriveFormat fmt
          , CGDisk.path = Data.Maybe.fromMaybe "" mPath
          , CGDisk.ephemeral = ephemeral
          , CGDisk.node = toCapnpEntityRef nodeRef
          , CGDisk.overwrite = overwrite
          }
  CGDisk.DiskManager'beginUpload'results {CGDisk.upload = upload} <-
    callOn #beginUpload CGDisk.DiskManager'beginUpload'params {CGDisk.params = params} mgr
  let send h = do
        chunk <- BS.hGet h (1024 * 1024)
        if BS.null chunk
          then pure ()
          else do
            _ <- callOn #write CGDisk.DiskUpload'write'params {CGDisk.chunk = chunk} upload
            send h
  uploadResult <- try @SomeException $ withBinaryFile source ReadMode send
  case uploadResult of
    Left err -> do
      _ <- try @SomeException (callOn #abort CGDisk.DiskUpload'abort'params upload)
      fail (show err)
    Right () -> do
      CGDisk.DiskUpload'finish'results {CGDisk.disk = disk} <-
        callOn #finish CGDisk.DiskUpload'finish'params upload
      CGDisk.Disk'show'results {CGDisk.info = info} <-
        callOn #show CGDisk.Disk'show'params disk
      case info of CGDisk.DiskImageInfo {CGDisk.id = did} -> pure did

rpcDiskClone :: CapnpConnection -> EntityRef -> Text -> Maybe Text -> Bool -> IO Int64
rpcDiskClone conn srcRef newName mPath ephemeral = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let p =
        CGDisk.DiskCloneParams
          { CGDisk.sourceRef = toCapnpEntityRef srcRef
          , CGDisk.newName = newName
          , CGDisk.path = Data.Maybe.fromMaybe "" mPath
          , CGDisk.ephemeral = ephemeral
          }
  CGDisk.DiskManager'clone'results {CGDisk.disk = dClient} <-
    callOn #clone CGDisk.DiskManager'clone'params {CGDisk.params = p} mgr
  CGDisk.Disk'show'results {CGDisk.info = info} <-
    callOn #show CGDisk.Disk'show'params dClient
  case info of CGDisk.DiskImageInfo {CGDisk.id = did} -> pure did

rpcDiskRebase :: CapnpConnection -> EntityRef -> EntityRef -> Bool -> IO ()
rpcDiskRebase conn diskRef newBackingRef unsafe = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let p =
        CGDisk.DiskRebaseParams
          { CGDisk.diskRef = toCapnpEntityRef diskRef
          , CGDisk.newBackingDiskRef = toCapnpEntityRef newBackingRef
          , CGDisk.newBackingProvided = True
          , CGDisk.unsafe = unsafe
          }
  _ <- callOn #rebase CGDisk.DiskManager'rebase'params {CGDisk.params = p} mgr
  pure ()

rpcDiskFlatten :: CapnpConnection -> EntityRef -> IO ()
rpcDiskFlatten conn diskRef = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  _ <- callOn #flatten CGDisk.DiskManager'flatten'params {CGDisk.diskRef = toCapnpEntityRef diskRef} mgr
  pure ()

rpcDiskCopy
  :: CapnpConnection
  -> EntityRef
  -> EntityRef
  -> Maybe Text
  -> Bool
  -> IO Int64
rpcDiskCopy conn diskRef toNodeRef mToPath withBackingChain = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let p =
        CGDisk.DiskCopyParams
          { CGDisk.diskRef = toCapnpEntityRef diskRef
          , CGDisk.toNodeRef = toCapnpEntityRef toNodeRef
          , CGDisk.toPath = Data.Maybe.fromMaybe T.empty mToPath
          , CGDisk.withBackingChain = withBackingChain
          }
  CGDisk.DiskManager'copy'results {CGDisk.taskId = tid} <-
    callOn #copy CGDisk.DiskManager'copy'params {CGDisk.params = p} mgr
  pure tid

rpcDiskMove
  :: CapnpConnection
  -> EntityRef
  -> EntityRef
  -> Maybe Text
  -> Bool
  -> IO Int64
rpcDiskMove conn diskRef toNodeRef mToPath withBackingChain = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let p =
        CGDisk.DiskMoveParams
          { CGDisk.diskRef = toCapnpEntityRef diskRef
          , CGDisk.toNodeRef = toCapnpEntityRef toNodeRef
          , CGDisk.toPath = Data.Maybe.fromMaybe T.empty mToPath
          , CGDisk.withBackingChain = withBackingChain
          }
  CGDisk.DiskManager'move'results {CGDisk.taskId = tid} <-
    callOn #move CGDisk.DiskManager'move'params {CGDisk.params = p} mgr
  pure tid

rpcDiskAttach
  :: CapnpConnection
  -> EntityRef
  -> EntityRef
  -> DriveInterface
  -> Maybe DriveMedia
  -> Bool
  -> Bool
  -> CacheType
  -> IO Int64
rpcDiskAttach conn vmRef diskRef iface mMedia readOnly discard cache = do
  vmClient <- getVmClient conn vmRef
  let mediaWire = toCapnpDriveMedia (Data.Maybe.fromMaybe MediaDisk mMedia)
      p =
        CGVm.DriveAttachParams
          { CGVm.diskRef = toCapnpEntityRef diskRef
          , CGVm.interface = toCapnpDriveInterface iface
          , CGVm.media = mediaWire
          , CGVm.readOnly = readOnly
          , CGVm.cacheType = toCapnpCacheType cache
          , CGVm.discard = discard
          }
  CGVm.Vm'attachDisk'results {CGVm.driveId = did} <-
    callOn #attachDisk CGVm.Vm'attachDisk'params {CGVm.params = p} vmClient
  pure did

rpcDiskDetach :: CapnpConnection -> EntityRef -> Int64 -> IO ()
rpcDiskDetach conn vmRef driveId = do
  vmClient <- getVmClient conn vmRef
  _ <- callOn #detachDisk CGVm.Vm'detachDisk'params {CGVm.driveId = driveId} vmClient
  pure ()

rpcDiskDetachByDisk :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcDiskDetachByDisk conn vmRef diskRef = do
  details <- rpcVmShow conn vmRef
  let drives = PV.vdDrives details
      driveIds = map PV.diId drives
      matches d = case (PV.diDiskImage d, diskRef) of
        (Just di, WC.RefById did) -> nrId di == did
        (Just di, WC.RefByName name) -> nrName di == name
        (Nothing, _) -> False
  case (diskRef, driveIds) of
    (WC.RefById did, ids)
      | did `elem` ids -> rpcDiskDetach conn vmRef did
      | otherwise ->
          case filter matches drives of
            (drive : _) -> rpcDiskDetach conn vmRef (PV.diId drive)
            [] -> fail ("no drive on VM with disk " <> show diskRef)
    _ ->
      case filter matches drives of
        (drive : _) -> rpcDiskDetach conn vmRef (PV.diId drive)
        [] -> fail ("no drive on VM with disk " <> show diskRef)

rpcDiskMediaEject :: CapnpConnection -> Int64 -> IO ()
rpcDiskMediaEject conn driveId = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  _ <- callOn #mediaEject CGDisk.DiskManager'mediaEject'params {CGDisk.driveId = driveId} mgr
  pure ()

rpcDiskMediaChange :: CapnpConnection -> Int64 -> EntityRef -> IO ()
rpcDiskMediaChange conn driveId newDiskRef = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  _ <-
    callOn
      #mediaChange
      CGDisk.DiskManager'mediaChange'params
        { CGDisk.driveId = driveId
        , CGDisk.newDiskRef = toCapnpEntityRef newDiskRef
        }
      mgr
  pure ()

-- ---------------------------------------------------------------------
-- Snapshot wrappers (per-disk)
-- ---------------------------------------------------------------------

rpcSnapshotList :: CapnpConnection -> EntityRef -> IO [PD.SnapshotInfo]
rpcSnapshotList conn diskRef = do
  dClient <- getDiskClient conn diskRef
  CGDisk.Disk'snapshotList'results {CGDisk.snapshots = ss} <-
    callOn #snapshotList CGDisk.Disk'snapshotList'params dClient
  pure (map WDisk.fromCapnpSnapshotInfo ss)

rpcSnapshotCreate
  :: CapnpConnection -> EntityRef -> Text -> CGE.QuiesceMode -> Bool -> IO Int64
rpcSnapshotCreate conn diskRef name quiesce fullMachine = do
  dClient <- getDiskClient conn diskRef
  CGDisk.Disk'snapshotCreate'results {CGDisk.snapshot = sClient, CGDisk.snapshotId = sid} <-
    callOn
      #snapshotCreate
      CGDisk.Disk'snapshotCreate'params
        { CGDisk.name = name
        , CGDisk.quiesce = quiesce
        , CGDisk.fullMachine = fullMachine
        }
      dClient
  _ <- pure sClient
  pure sid

rpcSnapshotDelete :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcSnapshotDelete conn diskRef snapRef = do
  sClient <- getSnapshotClient conn diskRef snapRef
  _ <- callOn #delete CGDisk.Snapshot'delete'params sClient
  pure ()

rpcSnapshotRollback :: CapnpConnection -> EntityRef -> EntityRef -> Bool -> IO ()
rpcSnapshotRollback conn diskRef snapRef autoStop = do
  sClient <- getSnapshotClient conn diskRef snapRef
  _ <-
    callOn
      #rollback
      CGDisk.Snapshot'rollback'params {CGDisk.autoStop = autoStop}
      sClient
  pure ()

rpcSnapshotMerge :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcSnapshotMerge conn diskRef snapRef = do
  sClient <- getSnapshotClient conn diskRef snapRef
  _ <- callOn #merge CGDisk.Snapshot'merge'params sClient
  pure ()

getSnapshotClient :: CapnpConnection -> EntityRef -> EntityRef -> IO (C.Client CGDisk.Snapshot)
getSnapshotClient conn diskRef snapRef = do
  dClient <- getDiskClient conn diskRef
  CGDisk.Disk'snapshotGet'results {CGDisk.snapshot = sClient} <-
    callOn #snapshotGet CGDisk.Disk'snapshotGet'params {CGDisk.ref = toCapnpEntityRef snapRef} dClient
  pure sClient

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

getDiskClient :: CapnpConnection -> EntityRef -> IO (C.Client CGDisk.Disk)
getDiskClient conn ref = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  CGDisk.DiskManager'get'results {CGDisk.disk = dClient} <-
    callOn #get CGDisk.DiskManager'get'params {CGDisk.ref = toCapnpEntityRef ref} mgr
  pure dClient

capnpDriveFormat :: DriveFormat -> CGE.DriveFormat
capnpDriveFormat = toCapnpDriveFormat

emptyCapnpEntityRef :: C.Parsed CGCommon.EntityRef
emptyCapnpEntityRef =
  CGCommon.EntityRef {CGCommon.union' = CGCommon.EntityRef'id 0}
