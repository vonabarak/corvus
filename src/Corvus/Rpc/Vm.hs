{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | VmManager + Vm cap implementations.
--
-- Phase 4 lands the full non-streaming Vm surface:
--   * Manager: list / get / create
--   * Resource: show / lifecycle (start/stop/pause/reset/edit/delete)
--   * Sub-resources: disk attach/detach, net-if add/remove/list,
--     shared-dir add/remove/list, ssh-key attach/detach/list
--   * Helpers: cloudInit / viewGrant / guestExec / sendCtrlAltDel
--
-- Streaming methods (serial console, HMP monitor, guest-agent
-- subscription) and VM-scoped snapshot ops stay stubbed with
-- 'methodUnimplemented' until Phase 6 wires the sink machinery.
module Corvus.Rpc.Vm
  ( VmManagerCap (..)
  , VmCap (..)
  , newVmManagerCap
  )
where

import Capnp (export)
import qualified Capnp as C
import qualified Capnp.Gen.Common as CGCommon
import qualified Capnp.Gen.Enums as CGE
import qualified Capnp.Gen.Streams as CGS
import qualified Capnp.Gen.Vm as CGVm
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer, methodUnimplemented)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Corvus.Action (runAction, runActionAsync, runActionAsyncWithId)
import Corvus.Handlers.Disk.Attach (DiskAttach (..), DiskDetachByDisk (..))
import Corvus.Handlers.GuestExec (GuestExec (..))
import Corvus.Handlers.NetIf (NetIfAdd (..), NetIfRemove (..), handleNetIfList)
import Corvus.Handlers.Resolve (resolveDisk, resolveNetwork, resolveNode, resolveSshKey, resolveVm)
import Corvus.Handlers.SharedDir (SharedDirAdd (..), SharedDirRemove (..), handleSharedDirList)
import Corvus.Handlers.SshKey (SshKeyAttach (..), SshKeyDetach (..), handleSshKeyListForVm)
import Corvus.Handlers.Vm
  ( VmCreate (..)
  , VmDelete (..)
  , VmEdit (..)
  , VmPause (..)
  , VmReset (..)
  , VmSave (..)
  , VmStart (..)
  , VmStop (..)
  , handleHmpMonitor
  , handleHmpMonitorFlush
  , handleSerialConsole
  , handleSerialConsoleFlush
  , handleVmCloudInit
  , handleVmList
  , handleVmSendCtrlAltDel
  , handleVmShow
  , handleVmViewGrant
  )
import Corvus.Handlers.Vm.Migrate (VmMigrate (..))
import Corvus.Handlers.Vm.Snapshot
  ( VmSnapshotCreate (..)
  , VmSnapshotDelete (..)
  , VmSnapshotRollback (..)
  , handleVmSnapshotList
  )
import qualified Corvus.Model as M
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol (Response (..))
import qualified Corvus.Protocol as P
import qualified Corvus.Protocol.CloudInit as PCI
import Corvus.Rpc.Common (capnpRefToRef, handleParsed, resolveOrThrow, throwError, throwWireError)
import Corvus.Rpc.Streams (EmptyHandle (..), runByteSinkRelay)
import Corvus.Types (ServerState (..))
import Corvus.Wire.CloudInit (toCapnpCloudInitInfo)
import Corvus.Wire.Common (ViewGrant (..), toCapnpViewGrant)
import Corvus.Wire.Enums
  ( fromCapnpCacheType
  , fromCapnpDriveInterface
  , fromCapnpDriveMedia
  , fromCapnpNetInterfaceType
  , fromCapnpSharedDirCache
  , toCapnpVmStatus
  )
import Corvus.Wire.Error (ErrorCode (..))
import Corvus.Wire.SharedDir (toCapnpSharedDirInfo)
import Corvus.Wire.SshKey (toCapnpSshKeyInfo)
import Corvus.Wire.Vm (toCapnpNetIfInfo, toCapnpVmDetails, toCapnpVmInfo, toCapnpVmSnapshotInfo, zeroVmStats)
import Data.Foldable (toList)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Database.Persist (delete, get)
import Database.Persist.Sql (fromSqlKey, runSqlPool, toSqlKey)
import Supervisors (Supervisor)

-- ---------------------------------------------------------------------
-- Manager
-- ---------------------------------------------------------------------

data VmManagerCap = VmManagerCap
  { vmgState :: !ServerState
  , vmgSup :: !Supervisor
  , vmgClientName :: !T.Text
  }

newVmManagerCap :: ServerState -> Supervisor -> T.Text -> IO VmManagerCap
newVmManagerCap st sup cn = pure (VmManagerCap st sup cn)

instance SomeServer VmManagerCap

instance CGVm.VmManager'server_ VmManagerCap where
  vmManager'list (VmManagerCap st _ _) = handleParsed $ \_ -> do
    resp <- handleVmList st
    case resp of
      RespVmList vms -> pure CGVm.VmManager'list'results {CGVm.vms = map toCapnpVmInfo vms}
      _ -> throwError resp

  vmManager'get (VmManagerCap st sup cn) = handleParsed $ \CGVm.VmManager'get'params {..} -> do
    ref' <- capnpRefToRef ref
    eid <- resolveOrThrow =<< resolveVm ref' (ssDbPool st)
    client <- export @CGVm.Vm sup (VmCap st sup eid cn)
    pure CGVm.VmManager'get'results {CGVm.vm = client}

  vmManager'create (VmManagerCap st sup cn) =
    handleParsed $ \CGVm.VmManager'create'params {params = CGVm.VmCreateParams {..}} -> do
      nodeRef' <- capnpRefToRef node
      let act =
            VmCreate
              { vcrName = name
              , vcrNodeRef = P.unRef nodeRef'
              , vcrCpuCount = fromIntegral cpuCount
              , vcrRamMb = fromIntegral ramMb
              , vcrDescription = if description == "" then Nothing else Just description
              , vcrHeadless = headless
              , vcrGuestAgent = guestAgent
              , vcrTpm = tpm
              , vcrCloudInit = cloudInit
              , vcrAutostart = autostart
              , vcrRebootQuirk = rebootQuirk
              , vcrCpuModel = cpuModel
              }
      resp <- runAction st cn act
      case resp of
        RespVmCreated newId -> do
          client <- export @CGVm.Vm sup (VmCap st sup newId cn)
          pure CGVm.VmManager'create'results {CGVm.vm = client}
        _ -> throwError resp

-- ---------------------------------------------------------------------
-- Vm resource cap
-- ---------------------------------------------------------------------

data VmCap = VmCap
  { vmState :: !ServerState
  , _vmSup :: !Supervisor
  , vmId :: !Int64
  , vmClientName :: !T.Text
  }

instance SomeServer VmCap

instance CGVm.Vm'server_ VmCap where
  vm'show (VmCap st _ eid cn) = handleParsed $ \_ -> do
    detResp <- handleVmShow st eid
    case detResp of
      RespVmDetails det -> do
        sharedResp <- handleSharedDirList st eid
        let sds = case sharedResp of
              RespSharedDirList xs -> xs
              _ -> []
        stats <- latestVmStats st eid
        pure
          CGVm.Vm'show'results
            { CGVm.details = toCapnpVmDetails det sds stats
            }
      _ -> throwError detResp

  vm'start (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'start'params {wait = wait'} -> do
    resp <-
      if wait'
        then runAction st cn (VmStart eid)
        else runActionAsync st cn (VmStart eid) (RespVmStateChanged M.VmStarting)
    status <- statusOrThrow resp
    pure CGVm.Vm'start'results {CGVm.status = status}

  vm'stop (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'stop'params {wait = wait', timeoutSec = tmo} -> do
    let action = VmStop eid tmo
    resp <-
      if wait'
        then runAction st cn action
        else runActionAsync st cn action (RespVmStateChanged M.VmStopping)
    status <- statusOrThrow resp
    pure CGVm.Vm'stop'results {CGVm.status = status}

  vm'pause (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (VmPause eid)
    status <- statusOrThrow resp
    pure CGVm.Vm'pause'results {CGVm.status = status}

  vm'reset (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (VmReset eid)
    status <- statusOrThrow resp
    pure CGVm.Vm'reset'results {CGVm.status = status}

  vm'save (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'save'params {wait = wait'} -> do
    resp <-
      if wait'
        then runAction st cn (VmSave eid)
        else runActionAsync st cn (VmSave eid) (RespVmStateChanged M.VmSaving)
    status <- statusOrThrow resp
    pure CGVm.Vm'save'results {CGVm.status = status}

  vm'edit (VmCap st _ eid cn) =
    handleParsed $ \CGVm.Vm'edit'params {params = CGVm.VmEditParams {..}} -> do
      let act =
            VmEdit
              { vedVmId = eid
              , vedCpus = if hasCpuCount then Just (fromIntegral cpuCount) else Nothing
              , vedRam = if hasRamMb then Just (fromIntegral ramMb) else Nothing
              , vedDesc = if hasDescription then Just description else Nothing
              , vedHeadless = if hasHeadless then Just headless else Nothing
              , vedGuestAgent = if hasGuestAgent then Just guestAgent else Nothing
              , vedTpm = if hasTpm then Just tpm else Nothing
              , vedCloudInit = if hasCloudInit then Just cloudInit else Nothing
              , vedAutostart = if hasAutostart then Just autostart else Nothing
              , vedRebootQuirk = if hasRebootQuirk then Just rebootQuirk else Nothing
              , vedCpuModel = if hasCpuModel then Just cpuModel else Nothing
              }
      resp <- runAction st cn act
      case resp of
        RespVmEdited -> pure CGVm.Vm'edit'results
        _ -> throwError resp

  vm'delete (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'delete'params {..} -> do
    resp <- runAction st cn (VmDelete {vdelVmId = eid, vdelKeepDisks = keepDisks})
    case resp of
      RespVmDeleted -> pure CGVm.Vm'delete'results
      _ -> throwError resp

  -- -------------------------------------------------------------------
  -- Misc read / one-shot
  -- -------------------------------------------------------------------

  vm'cloudInit (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleVmCloudInit st cn eid
    let emptyInfo =
          PCI.CloudInitInfo
            { PCI.ciiUserData = Nothing
            , PCI.ciiNetworkConfig = Nothing
            , PCI.ciiInjectSshKeys = False
            }
    case resp of
      RespCloudInitConfig mInfo ->
        let cfg = toCapnpCloudInitInfo (Data.Maybe.fromMaybe emptyInfo mInfo)
         in pure CGVm.Vm'cloudInit'results {CGVm.config = cfg}
      -- @handleVmCloudInit@ returns @RespVmEdited@ after a
      -- successful regenerate; the schema's @config@ field is
      -- not load-bearing for this call (the CLI only checks the
      -- exit code), so an empty payload is fine.
      RespVmEdited ->
        pure CGVm.Vm'cloudInit'results {CGVm.config = toCapnpCloudInitInfo emptyInfo}
      _ -> throwError resp

  vm'viewGrant (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleVmViewGrant st eid
    case resp of
      RespVmViewGrant host port password ttl ->
        pure
          CGVm.Vm'viewGrant'results
            { CGVm.grant =
                toCapnpViewGrant
                  ViewGrant
                    { vgHost = host
                    , vgPort = port
                    , vgPassword = password
                    , vgTtlSeconds = ttl
                    }
            }
      _ -> throwError resp

  vm'guestExec (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'guestExec'params {..} -> do
    resp <- runAction st cn (GuestExec {geVmId = eid, geCommand = command})
    case resp of
      RespGuestExecResult code outT errT ->
        pure
          CGVm.Vm'guestExec'results
            { CGVm.result =
                CGVm.GuestExecResult
                  { CGVm.exitCode = fromIntegral code
                  , CGVm.stdout = outT
                  , CGVm.stderr = errT
                  }
            }
      -- VM is mid-transition (e.g. an in-flight reboot_quirk
      -- re-spawn, or a reset/stop in progress). Surface the
      -- actual state so callers can decide whether to back off
      -- and retry.
      RespInvalidTransition status msg ->
        throwWireError VmNotRunning ("VM is " <> M.enumToText status <> "; " <> msg)
      _ -> throwError resp

  vm'sendCtrlAltDel (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleVmSendCtrlAltDel st eid
    case resp of
      RespOk -> pure CGVm.Vm'sendCtrlAltDel'results
      _ -> throwError resp

  -- -------------------------------------------------------------------
  -- Streaming methods
  -- -------------------------------------------------------------------

  -- Bidirectional serial-console relay. The client passes its
  -- 'ByteSink' (where QEMU's output goes); the daemon pushes the
  -- ring-buffer contents through it and returns an input
  -- 'ByteSink' the client can write into to forward bytes to
  -- QEMU.
  vm'serialConsole (VmCap st _sup eid _cn) =
    handleParsed $ \CGVm.Vm'serialConsole'params {CGVm.sink = sinkClient} -> do
      -- Validator first; rejections throw their structured wire
      -- error before anything touches the agent.
      resp <- handleSerialConsole st eid
      case resp of
        RespSerialConsoleOk -> do
          r <-
            withVmNodeAgent st eid $ \nac ->
              NOA.openSerialConsole nac eid sinkClient
          case r of
            Left err -> throwFailed err
            Right (Left e) -> throwFailed (T.pack (show e))
            Right (Right inp) -> pure CGVm.Vm'serialConsole'results {CGVm.input = inp}
        _ -> throwError resp

  -- HMP monitor: identical shape to serialConsole but rides the
  -- per-VM monitor buffer. Same validator-first dispatch.
  vm'hmpMonitor (VmCap st _sup eid _cn) =
    handleParsed $ \CGVm.Vm'hmpMonitor'params {CGVm.sink = sinkClient} -> do
      resp <- handleHmpMonitor st eid
      case resp of
        RespHmpMonitorOk -> do
          r <-
            withVmNodeAgent st eid $ \nac ->
              NOA.openHmpMonitor nac eid sinkClient
          case r of
            Left err -> throwFailed err
            Right (Left e) -> throwFailed (T.pack (show e))
            Right (Right inp) -> pure CGVm.Vm'hmpMonitor'results {CGVm.input = inp}
        _ -> throwError resp

  -- Register a 'GuestAgentStatusSink' against the per-VM
  -- subscriber list. Returns an empty 'Handle' cap; when the
  -- client drops it (or the sink itself), the next push attempt
  -- raises and the subscriber is pruned from the list.
  vm'subscribeGuestAgent (VmCap st sup eid _cn) =
    handleParsed $ \CGVm.Vm'subscribeGuestAgent'params {CGVm.sink = sinkClient} -> do
      atomically $
        modifyTVar' (ssGuestAgentSubs st) $
          Map.insertWith (++) eid [sinkClient]
      handle <- export @CGS.Handle sup EmptyHandle
      pure CGVm.Vm'subscribeGuestAgent'results {CGVm.handle = handle}

  vm'serialConsoleFlush (VmCap st _ eid cn) = handleParsed $ \_ -> do
    -- Validator first; proxy to the agent on success.
    _ <- handleSerialConsoleFlush st eid
    r <- withVmNodeAgent st eid $ \nac -> NOA.flushSerialConsole nac eid
    case r of
      Left err -> throwFailed err
      Right (Left e) -> throwFailed (T.pack (show e))
      Right (Right ()) -> pure CGVm.Vm'serialConsoleFlush'results

  vm'hmpMonitorFlush (VmCap st _ eid cn) = handleParsed $ \_ -> do
    _ <- handleHmpMonitorFlush st eid
    r <- withVmNodeAgent st eid $ \nac -> NOA.flushHmpMonitor nac eid
    case r of
      Left err -> throwFailed err
      Right (Left e) -> throwFailed (T.pack (show e))
      Right (Right ()) -> pure CGVm.Vm'hmpMonitorFlush'results

  -- -------------------------------------------------------------------
  -- Disk attach / detach
  -- -------------------------------------------------------------------

  vm'attachDisk (VmCap st _ eid cn) =
    handleParsed $ \CGVm.Vm'attachDisk'params {params = CGVm.DriveAttachParams {..}} -> do
      diskRef' <- capnpRefToRef diskRef
      diskId <- resolveOrThrow =<< resolveDisk diskRef' (ssDbPool st)
      iface <- enumOrThrow (fromCapnpDriveInterface interface)
      med <- enumOrThrow (fromCapnpDriveMedia media)
      cache <- enumOrThrow (fromCapnpCacheType cacheType)
      let act =
            DiskAttach
              { datVmId = eid
              , datDiskId = diskId
              , datInterface = iface
              , datMedia = Just med
              , datReadOnly = readOnly
              , datDiscard = discard
              , datCache = cache
              }
      resp <- runAction st cn act
      case resp of
        RespDiskAttached driveId ->
          pure CGVm.Vm'attachDisk'results {CGVm.driveId = driveId}
        _ -> throwError resp

  vm'detachDisk (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'detachDisk'params {..} -> do
    -- The schema's driveId is the row id of the Drive table
    -- (i.e. a specific attachment instance). The existing
    -- DiskDetachByDisk Action operates on the disk-image id; look
    -- up the Drive row to bridge between the two representations.
    mDrive <- runSqlPool (get (toSqlKey driveId :: M.DriveId)) (ssDbPool st)
    case mDrive of
      Just drv | M.driveVmId drv == toSqlKey eid -> case M.driveDiskImageId drv of
        Just diskImageKey -> do
          let diskImageId = fromSqlKey diskImageKey
          resp <- runAction st cn (DiskDetachByDisk {ddbVmId = eid, ddbDiskId = diskImageId})
          case resp of
            RespOk -> pure CGVm.Vm'detachDisk'results
            RespDiskOk -> pure CGVm.Vm'detachDisk'results
            _ -> throwError resp
        Nothing -> do
          -- Ejected media drive: no image attached, so the
          -- image-based action cannot bridge to it. CD-ROM drives
          -- are not hot-pluggable, so require a stopped VM and
          -- drop the row directly.
          mVm <- runSqlPool (get (M.driveVmId drv)) (ssDbPool st)
          case mVm of
            Nothing -> throwError RespVmNotFound
            Just vm
              | M.vmStatus vm `elem` [M.VmStarting, M.VmRunning, M.VmPaused] ->
                  throwWireError
                    VmMustBeStopped
                    ( "Drive "
                        <> T.pack (show driveId)
                        <> " has no media attached; CD-ROM drives cannot be detached while the VM is active - stop the VM first"
                    )
            Just _ -> do
              runSqlPool (delete (toSqlKey driveId :: M.DriveId)) (ssDbPool st)
              pure CGVm.Vm'detachDisk'results
      _ -> throwError RespDriveNotFound

  -- -------------------------------------------------------------------
  -- Net interfaces
  -- -------------------------------------------------------------------

  vm'addNetIf (VmCap st _ eid cn) =
    handleParsed $ \CGVm.Vm'addNetIf'params {params = CGVm.NetIfAddParams {..}} -> do
      iface <- enumOrThrow (fromCapnpNetInterfaceType type_)
      mNetId <- case fromCapnpRefMaybe networkRef of
        Just r -> Just <$> (resolveOrThrow =<< resolveNetwork r (ssDbPool st))
        Nothing -> pure Nothing
      let act =
            NetIfAdd
              { niaVmId = eid
              , niaType = iface
              , niaHostDevice = hostDevice
              , niaMacAddress = if macAddress == "" then Nothing else Just macAddress
              , niaNetworkId = mNetId
              }
      resp <- runAction st cn act
      case resp of
        RespNetIfAdded nid -> pure CGVm.Vm'addNetIf'results {CGVm.netIfId = nid}
        _ -> throwError resp

  vm'removeNetIf (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'removeNetIf'params {..} -> do
    resp <- runAction st cn (NetIfRemove {nirVmId = eid, nirNetIfId = netIfId})
    case resp of
      RespOk -> pure CGVm.Vm'removeNetIf'results
      _ -> throwError resp

  vm'listNetIfs (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleNetIfList st eid
    case resp of
      RespNetIfList nis ->
        pure CGVm.Vm'listNetIfs'results {CGVm.netIfs = map toCapnpNetIfInfo nis}
      _ -> throwError resp

  -- -------------------------------------------------------------------
  -- Shared directories
  -- -------------------------------------------------------------------

  vm'addSharedDir (VmCap st _ eid cn) =
    handleParsed $ \CGVm.Vm'addSharedDir'params {params = CGVm.SharedDirAddParams {..}} -> do
      cacheVal <- enumOrThrow (fromCapnpSharedDirCache cache)
      let act =
            SharedDirAdd
              { sdaVmId = eid
              , sdaPath = path
              , sdaTag = tag
              , sdaCache = cacheVal
              , sdaReadOnly = readOnly
              }
      resp <- runAction st cn act
      case resp of
        RespSharedDirAdded sid ->
          pure CGVm.Vm'addSharedDir'results {CGVm.sharedDirId = sid}
        _ -> throwError resp

  vm'removeSharedDir (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'removeSharedDir'params {..} -> do
    resp <- runAction st cn (SharedDirRemove {sdrVmId = eid, sdrDirId = sharedDirId})
    case resp of
      RespOk -> pure CGVm.Vm'removeSharedDir'results
      RespSharedDirOk -> pure CGVm.Vm'removeSharedDir'results
      _ -> throwError resp

  vm'listSharedDirs (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleSharedDirList st eid
    case resp of
      RespSharedDirList sds ->
        pure CGVm.Vm'listSharedDirs'results {CGVm.sharedDirs = map toCapnpSharedDirInfo sds}
      _ -> throwError resp

  -- -------------------------------------------------------------------
  -- SSH keys attached to the VM
  -- -------------------------------------------------------------------

  vm'attachSshKey (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'attachSshKey'params {..} -> do
    keyRef' <- capnpRefToRef keyRef
    keyId <- resolveOrThrow =<< resolveSshKey keyRef' (ssDbPool st)
    resp <- runAction st cn (SshKeyAttach {skaVmId = eid, skaKeyId = keyId})
    case resp of
      RespSshKeyOk -> pure CGVm.Vm'attachSshKey'results
      RespOk -> pure CGVm.Vm'attachSshKey'results
      _ -> throwError resp

  vm'detachSshKey (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'detachSshKey'params {..} -> do
    keyRef' <- capnpRefToRef keyRef
    keyId <- resolveOrThrow =<< resolveSshKey keyRef' (ssDbPool st)
    resp <- runAction st cn (SshKeyDetach {skdetVmId = eid, skdetKeyId = keyId})
    case resp of
      RespSshKeyOk -> pure CGVm.Vm'detachSshKey'results
      RespOk -> pure CGVm.Vm'detachSshKey'results
      _ -> throwError resp

  vm'listSshKeys (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleSshKeyListForVm st eid
    case resp of
      RespSshKeyList keys ->
        pure CGVm.Vm'listSshKeys'results {CGVm.keys = map toCapnpSshKeyInfo keys}
      _ -> throwError resp

  vm'snapshotCreate (VmCap st _ eid cn) =
    handleParsed $ \CGVm.Vm'snapshotCreate'params {..} -> do
      resp <- runAction st cn (VmSnapshotCreate {vscVmId = eid, vscName = name})
      case resp of
        RespVmSnapshotCreated info ->
          pure CGVm.Vm'snapshotCreate'results {CGVm.info = toCapnpVmSnapshotInfo info}
        _ -> throwError resp

  vm'snapshotList (VmCap st _ eid _) = handleParsed $ \_ -> do
    resp <- handleVmSnapshotList st eid
    case resp of
      RespVmSnapshotList snaps ->
        pure
          CGVm.Vm'snapshotList'results
            { CGVm.snapshots = map toCapnpVmSnapshotInfo snaps
            }
      _ -> throwError resp

  vm'snapshotRollback (VmCap st _ eid cn) =
    handleParsed $ \CGVm.Vm'snapshotRollback'params {..} -> do
      resp <- runAction st cn (VmSnapshotRollback {vsrVmId = eid, vsrName = name})
      case resp of
        RespSnapshotOk -> pure CGVm.Vm'snapshotRollback'results
        RespOk -> pure CGVm.Vm'snapshotRollback'results
        _ -> throwError resp

  vm'snapshotDelete (VmCap st _ eid cn) =
    handleParsed $ \CGVm.Vm'snapshotDelete'params {..} -> do
      resp <- runAction st cn (VmSnapshotDelete {vsdVmId = eid, vsdName = name})
      case resp of
        RespSnapshotOk -> pure CGVm.Vm'snapshotDelete'results
        RespOk -> pure CGVm.Vm'snapshotDelete'results
        _ -> throwError resp

  vm'migrate (VmCap st _ eid cn) =
    handleParsed $ \CGVm.Vm'migrate'params {params = CGVm.VmMigrateParams {..}} -> do
      nr <- capnpRefToRef toNodeRef
      destNode <- resolveOrThrow =<< resolveNode nr (ssDbPool st)
      let act = VmMigrate {vmiVmId = eid, vmiDestNodeId = destNode}
      resp <- runActionAsyncWithId st cn act RespDiskTransferStarted
      case resp of
        RespDiskTransferStarted tid ->
          pure CGVm.Vm'migrate'results {CGVm.taskId = tid}
        _ -> throwError resp

  -- Per-VM resource stats subscribe + history fetch. The agent's
  -- StatusPoller pushes one VmStats sample per VM every ~10 s up
  -- the existing VmStatusSink channel; the daemon's
  -- DaemonVmStatusSink (Corvus.Handlers.VmStatusSink) memoises
  -- the most recent 60 samples per VM in 'ssVmStatsRing' and
  -- fans the latest out to every subscriber in 'ssVmStatsSubs'.
  vm'getStatsHistory (VmCap st _ eid _) = handleParsed $ \_ -> do
    ring <- readVmStatsRing st eid
    pure CGVm.Vm'getStatsHistory'results {CGVm.samples = ring}

  vm'subscribeStats (VmCap st sup eid _) =
    handleParsed $ \CGVm.Vm'subscribeStats'params {sink = sink'} -> do
      atomically $
        modifyTVar' (ssVmStatsSubs st) $
          Map.insertWith (++) eid [sink']
      h <- export @CGS.Handle sup EmptyHandle
      pure CGVm.Vm'subscribeStats'results {CGVm.handle = h}

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

-- | Map a VM-lifecycle action response to either the new status
-- (returned to the client on success) or a typed RPC exception.
--
-- The old @toStatusOrThrow@ silently mapped any unknown response
-- to 'VmStatus'error', which the CLI's @rpcVm{Start,Stop,…}@
-- wrapper interpreted as success — operators would see "OK"
-- after a failed start when the real cause was, e.g., a
-- not-running managed network. Every known failure shape now
-- throws so the wire reply has @type_ = failed@ and the CLI
-- surfaces the message verbatim.
statusOrThrow :: Response -> IO CGE.VmStatus
statusOrThrow resp = case resp of
  RespVmStateChanged s -> pure (toCapnpVmStatus s)
  RespVmRunning -> pure (toCapnpVmStatus M.VmRunning)
  _ -> throwError resp

enumOrThrow :: Either e a -> IO a
enumOrThrow (Right a) = pure a
enumOrThrow (Left _) = throwWireError ProtocolError "unknown enum tag in request"

-- | Treat an EntityRef as 'Nothing' if both branches are
-- absent-equivalent (id == 0 / name == ""). Used for the optional
-- 'networkRef' field on NetIfAddParams.
fromCapnpRefMaybe :: CGCommon.Parsed CGCommon.EntityRef -> Maybe P.Ref
fromCapnpRefMaybe r =
  case CGCommon.union' r of
    CGCommon.EntityRef'id 0 -> Nothing
    CGCommon.EntityRef'name "" -> Nothing
    CGCommon.EntityRef'id n -> Just (P.Ref (T.pack (show n)))
    CGCommon.EntityRef'name t -> Just (P.Ref t)
    CGCommon.EntityRef'unknown' _ -> Nothing

-- | Look up the most recent cached 'VmStats' sample for a VM, or
-- 'zeroVmStats' when the daemon hasn't seen one yet.
latestVmStats :: ServerState -> Int64 -> IO (C.Parsed CGVm.VmStats)
latestVmStats st vmId = do
  ring <- readTVarIO (ssVmStatsRing st)
  pure $ case Map.lookup vmId ring of
    Just s | not (Seq.null s) -> Seq.index s (Seq.length s - 1)
    _ -> zeroVmStats

-- | Whole ring buffer for a VM, oldest first. Empty list when
-- the VM was never sampled or has transitioned out of running.
readVmStatsRing :: ServerState -> Int64 -> IO [C.Parsed CGVm.VmStats]
readVmStatsRing st vmId = do
  ring <- readTVarIO (ssVmStatsRing st)
  pure (maybe [] toList (Map.lookup vmId ring))
