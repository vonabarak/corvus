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
import qualified Capnp.Gen.Common as CGCommon
import qualified Capnp.Gen.Enums as CGE
import qualified Capnp.Gen.Streams as CGS
import qualified Capnp.Gen.Vm as CGVm
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer, methodUnimplemented)
import Control.Concurrent.STM (atomically, modifyTVar')
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
import qualified Corvus.Model as M
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol (Response (..))
import qualified Corvus.Protocol as P
import qualified Corvus.Protocol.CloudInit as PCI
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft, handleParsed)
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
import Corvus.Wire.SharedDir (toCapnpSharedDirInfo)
import Corvus.Wire.SshKey (toCapnpSshKeyInfo)
import Corvus.Wire.Vm (toCapnpNetIfInfo, toCapnpVmDetails, toCapnpVmInfo)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe
import qualified Data.Text as T
import Database.Persist (get)
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
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
      RespError msg -> throwFailed msg
      _ -> throwFailed "vmManager'list: unexpected response"

  vmManager'get (VmManagerCap st sup cn) = handleParsed $ \CGVm.VmManager'get'params {..} -> do
    ref' <- capnpRefToRef ref
    eid <- failOnLeft =<< resolveVm ref' (ssDbPool st)
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
              , vcrCloudInit = cloudInit
              , vcrAutostart = autostart
              , vcrRebootQuirk = rebootQuirk
              }
      resp <- runAction st cn act
      case resp of
        RespVmCreated newId -> do
          client <- export @CGVm.Vm sup (VmCap st sup newId cn)
          pure CGVm.VmManager'create'results {CGVm.vm = client}
        RespError msg -> throwFailed msg
        _ -> throwFailed (T.pack ("vmManager'create: unexpected response: " <> show resp))

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
        pure CGVm.Vm'show'results {CGVm.details = toCapnpVmDetails det sds}
      RespVmNotFound -> throwFailed "VM not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'show: unexpected response"

  vm'start (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'start'params {wait = wait'} -> do
    resp <-
      if wait'
        then runAction st cn (VmStart eid)
        else runActionAsync st cn (VmStart eid) (RespVmStateChanged M.VmStarting)
    case resp of
      RespError msg -> throwFailed msg
      _ -> pure CGVm.Vm'start'results {CGVm.status = toStatusOrThrow resp}

  vm'stop (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'stop'params {wait = wait'} -> do
    resp <-
      if wait'
        then runAction st cn (VmStop eid)
        else runActionAsync st cn (VmStop eid) (RespVmStateChanged M.VmStopping)
    case resp of
      RespError msg -> throwFailed msg
      _ -> pure CGVm.Vm'stop'results {CGVm.status = toStatusOrThrow resp}

  vm'pause (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (VmPause eid)
    case resp of
      RespError msg -> throwFailed msg
      _ -> pure CGVm.Vm'pause'results {CGVm.status = toStatusOrThrow resp}

  vm'reset (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- runAction st cn (VmReset eid)
    case resp of
      RespError msg -> throwFailed msg
      _ -> pure CGVm.Vm'reset'results {CGVm.status = toStatusOrThrow resp}

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
              , vedCloudInit = if hasCloudInit then Just cloudInit else Nothing
              , vedAutostart = if hasAutostart then Just autostart else Nothing
              , vedRebootQuirk = if hasRebootQuirk then Just rebootQuirk else Nothing
              }
      resp <- runAction st cn act
      case resp of
        RespVmEdited -> pure CGVm.Vm'edit'results
        RespVmNotFound -> throwFailed "VM not found"
        RespError msg -> throwFailed msg
        _ -> throwFailed "vm'edit: unexpected response"

  vm'delete (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'delete'params {..} -> do
    resp <- runAction st cn (VmDelete {vdelVmId = eid, vdelKeepDisks = keepDisks})
    case resp of
      RespVmDeleted -> pure CGVm.Vm'delete'results
      RespVmNotFound -> throwFailed "VM not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'delete: unexpected response"

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
      RespVmNotFound -> throwFailed "VM not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'cloudInit: unexpected response"

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
      RespVmNotFound -> throwFailed "VM not found"
      RespVmNotRunning -> throwFailed "VM not running"
      RespVmHeadless -> throwFailed "VM has no SPICE display"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'viewGrant: unexpected response"

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
      RespGuestAgentNotEnabled -> throwFailed "Guest agent not enabled"
      RespGuestAgentError msg -> throwFailed msg
      -- VM is mid-transition (e.g. an in-flight reboot_quirk
      -- re-spawn, or a reset/stop in progress). Surface the
      -- actual state so callers can decide whether to back off
      -- and retry.
      RespInvalidTransition status msg ->
        throwFailed $ "VM is " <> M.enumToText status <> "; " <> msg
      RespVmNotFound -> throwFailed "VM not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'guestExec: unexpected response"

  vm'sendCtrlAltDel (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleVmSendCtrlAltDel st eid
    case resp of
      RespOk -> pure CGVm.Vm'sendCtrlAltDel'results
      RespVmNotFound -> throwFailed "VM not found"
      RespVmNotRunning -> throwFailed "VM not running"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'sendCtrlAltDel: unexpected response"

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
      -- Validator first, so rejections preserve the rich
      -- pre-Cap'n-Proto messages ("VM is not running ...",
      -- "VM is not headless ...").
      resp <- handleSerialConsole st eid
      case resp of
        RespVmNotFound -> throwFailed "VM not found"
        RespError msg -> throwFailed msg
        RespSerialConsoleOk -> do
          r <-
            withVmNodeAgent st eid $ \nac ->
              NOA.openSerialConsole nac eid sinkClient
          case r of
            Left err -> throwFailed err
            Right (Left e) -> throwFailed (T.pack (show e))
            Right (Right inp) -> pure CGVm.Vm'serialConsole'results {CGVm.input = inp}
        _ -> throwFailed "Unexpected serial console response"

  -- HMP monitor: identical shape to serialConsole but rides the
  -- per-VM monitor buffer. Same validator-first dispatch.
  vm'hmpMonitor (VmCap st _sup eid _cn) =
    handleParsed $ \CGVm.Vm'hmpMonitor'params {CGVm.sink = sinkClient} -> do
      resp <- handleHmpMonitor st eid
      case resp of
        RespVmNotFound -> throwFailed "VM not found"
        RespError msg -> throwFailed msg
        RespHmpMonitorOk -> do
          r <-
            withVmNodeAgent st eid $ \nac ->
              NOA.openHmpMonitor nac eid sinkClient
          case r of
            Left err -> throwFailed err
            Right (Left e) -> throwFailed (T.pack (show e))
            Right (Right inp) -> pure CGVm.Vm'hmpMonitor'results {CGVm.input = inp}
        _ -> throwFailed "Unexpected HMP monitor response"

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
      diskId <- failOnLeft =<< resolveDisk diskRef' (ssDbPool st)
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
        RespVmNotFound -> throwFailed "VM not found"
        RespDiskNotFound -> throwFailed "Disk not found"
        RespError msg -> throwFailed msg
        _ -> throwFailed "vm'attachDisk: unexpected response"

  vm'detachDisk (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'detachDisk'params {..} -> do
    -- The schema's driveId is the row id of the Drive table
    -- (i.e. a specific attachment instance). The existing
    -- DiskDetachByDisk Action operates on the disk-image id; look
    -- up the Drive row to bridge between the two representations.
    mDrive <- runSqlPool (get (toSqlKey driveId :: M.DriveId)) (ssDbPool st)
    case mDrive of
      Just drv | M.driveVmId drv == toSqlKey eid -> do
        let diskImageId = fromSqlKey (M.driveDiskImageId drv)
        resp <- runAction st cn (DiskDetachByDisk {ddbVmId = eid, ddbDiskId = diskImageId})
        case resp of
          RespOk -> pure CGVm.Vm'detachDisk'results
          RespDiskOk -> pure CGVm.Vm'detachDisk'results
          RespVmNotFound -> throwFailed "VM not found"
          RespDriveNotFound -> throwFailed "Drive not found"
          RespError msg -> throwFailed msg
          _ -> throwFailed "vm'detachDisk: unexpected response"
      _ -> throwFailed "Drive not found"

  -- -------------------------------------------------------------------
  -- Net interfaces
  -- -------------------------------------------------------------------

  vm'addNetIf (VmCap st _ eid cn) =
    handleParsed $ \CGVm.Vm'addNetIf'params {params = CGVm.NetIfAddParams {..}} -> do
      iface <- enumOrThrow (fromCapnpNetInterfaceType type_)
      mNetId <- case fromCapnpRefMaybe networkRef of
        Just r -> Just <$> (failOnLeft =<< resolveNetwork r (ssDbPool st))
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
        RespVmNotFound -> throwFailed "VM not found"
        RespError msg -> throwFailed msg
        _ -> throwFailed "vm'addNetIf: unexpected response"

  vm'removeNetIf (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'removeNetIf'params {..} -> do
    resp <- runAction st cn (NetIfRemove {nirVmId = eid, nirNetIfId = netIfId})
    case resp of
      RespOk -> pure CGVm.Vm'removeNetIf'results
      RespNetIfNotFound -> throwFailed "Net-if not found"
      RespVmNotFound -> throwFailed "VM not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'removeNetIf: unexpected response"

  vm'listNetIfs (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleNetIfList st eid
    case resp of
      RespNetIfList nis ->
        pure CGVm.Vm'listNetIfs'results {CGVm.netIfs = map toCapnpNetIfInfo nis}
      RespVmNotFound -> throwFailed "VM not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'listNetIfs: unexpected response"

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
        RespVmNotFound -> throwFailed "VM not found"
        RespError msg -> throwFailed msg
        _ -> throwFailed "vm'addSharedDir: unexpected response"

  vm'removeSharedDir (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'removeSharedDir'params {..} -> do
    resp <- runAction st cn (SharedDirRemove {sdrVmId = eid, sdrDirId = sharedDirId})
    case resp of
      RespOk -> pure CGVm.Vm'removeSharedDir'results
      RespSharedDirOk -> pure CGVm.Vm'removeSharedDir'results
      RespSharedDirNotFound -> throwFailed "Shared directory not found"
      RespVmNotFound -> throwFailed "VM not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'removeSharedDir: unexpected response"

  vm'listSharedDirs (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleSharedDirList st eid
    case resp of
      RespSharedDirList sds ->
        pure CGVm.Vm'listSharedDirs'results {CGVm.sharedDirs = map toCapnpSharedDirInfo sds}
      RespVmNotFound -> throwFailed "VM not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'listSharedDirs: unexpected response"

  -- -------------------------------------------------------------------
  -- SSH keys attached to the VM
  -- -------------------------------------------------------------------

  vm'attachSshKey (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'attachSshKey'params {..} -> do
    keyRef' <- capnpRefToRef keyRef
    keyId <- failOnLeft =<< resolveSshKey keyRef' (ssDbPool st)
    resp <- runAction st cn (SshKeyAttach {skaVmId = eid, skaKeyId = keyId})
    case resp of
      RespSshKeyOk -> pure CGVm.Vm'attachSshKey'results
      RespOk -> pure CGVm.Vm'attachSshKey'results
      RespVmNotFound -> throwFailed "VM not found"
      RespSshKeyNotFound -> throwFailed "SSH key not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'attachSshKey: unexpected response"

  vm'detachSshKey (VmCap st _ eid cn) = handleParsed $ \CGVm.Vm'detachSshKey'params {..} -> do
    keyRef' <- capnpRefToRef keyRef
    keyId <- failOnLeft =<< resolveSshKey keyRef' (ssDbPool st)
    resp <- runAction st cn (SshKeyDetach {skdetVmId = eid, skdetKeyId = keyId})
    case resp of
      RespSshKeyOk -> pure CGVm.Vm'detachSshKey'results
      RespOk -> pure CGVm.Vm'detachSshKey'results
      RespVmNotFound -> throwFailed "VM not found"
      RespSshKeyNotFound -> throwFailed "SSH key not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'detachSshKey: unexpected response"

  vm'listSshKeys (VmCap st _ eid cn) = handleParsed $ \_ -> do
    resp <- handleSshKeyListForVm st eid
    case resp of
      RespSshKeyList keys ->
        pure CGVm.Vm'listSshKeys'results {CGVm.keys = map toCapnpSshKeyInfo keys}
      RespVmNotFound -> throwFailed "VM not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "vm'listSshKeys: unexpected response"

  -- VM-scoped snapshot operations need cross-disk aggregation; that
  -- logic lives in Phase 6 alongside the disk snapshot caps.
  vm'snapshotCreate _ = methodUnimplemented
  vm'snapshotList _ = methodUnimplemented
  vm'snapshotGet _ = methodUnimplemented

  vm'migrate (VmCap st _ eid cn) =
    handleParsed $ \CGVm.Vm'migrate'params {params = CGVm.VmMigrateParams {..}} -> do
      nr <- capnpRefToRef toNodeRef
      destNode <- failOnLeft =<< resolveNode nr (ssDbPool st)
      let act = VmMigrate {vmiVmId = eid, vmiDestNodeId = destNode}
      resp <- runActionAsyncWithId st cn act RespDiskTransferStarted
      case resp of
        RespDiskTransferStarted tid ->
          pure CGVm.Vm'migrate'results {CGVm.taskId = tid}
        RespError msg -> throwFailed msg
        _ -> throwFailed "vm'migrate: unexpected response"

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

toStatusOrThrow :: Response -> CGE.VmStatus
toStatusOrThrow resp = case resp of
  RespVmStateChanged s -> toCapnpVmStatus s
  RespVmRunning -> toCapnpVmStatus M.VmRunning
  _ -> CGE.VmStatus'error

enumOrThrow :: Either e a -> IO a
enumOrThrow (Right a) = pure a
enumOrThrow (Left _) = throwFailed "unknown enum tag in request"

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
