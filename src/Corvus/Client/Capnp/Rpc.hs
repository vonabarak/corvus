{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Cap'n Proto-backed client RPC wrappers.
--
-- Each function below dials the Daemon cap held in
-- 'CapnpConnection', walks to the appropriate manager / resource
-- cap, invokes a method, and returns a Haskell-side type
-- (typically a 'Corvus.Protocol.*' info record, so CLI
-- "Corvus.Client.Commands.*" code keeps the same shape it has
-- today).
--
-- This is the Phase 4 / Phase 5 replacement for the legacy
-- "Corvus.Client.Rpc". The two coexist during the staged
-- migration; the legacy module is deleted in Phase 5.
--
-- Phase 4e (this commit) lands the read-only surface plus the
-- daemon-level operations (ping / status / shutdown). The
-- mutating wrappers ship in follow-up commits as their
-- corresponding cap methods get exercised by the test DSL and
-- CLI rewrites.
module Corvus.Client.Capnp.Rpc
  ( -- * Daemon-level
    rpcPing
  , rpcStatus
  , rpcShutdown
  , rpcApply
  , rpcBuild

    -- * VM streaming (Phase 6b)
  , rpcVmSerialConsole
  , rpcVmHmpMonitor

    -- * Guest-agent subscription (Phase 6d)
  , rpcVmSubscribeGuestAgent
  , GuestAgentStatusEvent (..)

    -- * Task progress subscription (Phase 6e)
  , rpcTaskSubscribe
  , TaskProgressEvent (..)

    -- * Resource read methods
  , rpcVmList
  , rpcVmShow
  , rpcDiskList
  , rpcDiskShow
  , rpcNetworkList
  , rpcNetworkShow
  , rpcNodeList
  , rpcNodeShow
  , rpcSshKeyList
  , rpcSshKeyListForVm
  , rpcTemplateList
  , rpcTemplateShow
  , rpcTaskList
  , rpcTaskShow
  , rpcTaskListChildren

    -- * VM lifecycle
  , rpcVmCreate
  , rpcVmStart
  , rpcVmStop
  , rpcVmPause
  , rpcVmReset
  , rpcVmSave
  , rpcVmDelete
  , rpcVmMigrate
  , rpcVmEdit
  , rpcVmCloudInit
  , rpcVmViewGrant
  , rpcVmSendCtrlAltDel
  , rpcVmSerialConsoleFlush
  , rpcVmHmpMonitorFlush
  , rpcGuestExec

    -- * Disk lifecycle
  , rpcDiskCreate
  , rpcDiskCreateOverlay
  , rpcDiskRegister
  , rpcDiskRefresh
  , rpcDiskImport
  , rpcDiskClone
  , rpcDiskRebase
  , rpcDiskCopy
  , rpcDiskMove
  , rpcDiskDelete
  , rpcDiskResize
  , rpcDiskAttach
  , rpcDiskDetachByDisk

    -- * Snapshot operations (per-disk)
  , rpcSnapshotList
  , rpcSnapshotCreate
  , rpcSnapshotDelete
  , rpcSnapshotRollback
  , rpcSnapshotMerge

    -- * Shared directory
  , rpcSharedDirAdd
  , rpcSharedDirRemove
  , rpcSharedDirList

    -- * Network interface (per-VM)
  , rpcNetIfAdd
  , rpcNetIfRemove
  , rpcNetIfList

    -- * Network lifecycle
  , rpcNetworkCreate
  , rpcNetworkStart
  , rpcNetworkAttachNode
  , rpcNetworkDetachNode
  , rpcNetworkStop
  , rpcNetworkDelete
  , rpcNetworkEdit

    -- * Node lifecycle
  , rpcNodeAdd
  , rpcNodeEdit
  , rpcNodeDrain
  , rpcNodeDelete

    -- * SSH key lifecycle
  , rpcSshKeyCreate
  , rpcSshKeyDelete
  , rpcSshKeyAttach
  , rpcSshKeyDetach

    -- * Template lifecycle
  , rpcTemplateCreate
  , rpcTemplateUpdate
  , rpcTemplateDelete
  , rpcTemplateInstantiate

    -- * Cloud-init
  , rpcCloudInitSet
  , rpcCloudInitGet
  , rpcCloudInitDelete
  )
where

import Capnp (export)
import qualified Capnp as C
import qualified Capnp.Gen.Cloudinit as CGCI
import qualified Capnp.Gen.Common as CGCommon
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Disk as CGDisk
import qualified Capnp.Gen.Enums as CGE
import qualified Capnp.Gen.Network as CGNet
import qualified Capnp.Gen.Node as CGNode
import qualified Capnp.Gen.Sshkey as CGSsh
import qualified Capnp.Gen.Streams as CGS
import qualified Capnp.Gen.Task as CGTask
import qualified Capnp.Gen.Template as CGTmpl
import qualified Capnp.Gen.Vm as CGVm
import Capnp.Rpc.Server (SomeServer, handleParsed)
import Control.Exception (SomeException, try)
import qualified Control.Monad
import Corvus.Client.Capnp.Connection (CapnpConnection (..))
import Corvus.Model
  ( CacheType
  , DriveFormat
  , DriveInterface
  , DriveMedia (..)
  , NetInterfaceType
  , NodeAdminState (..)
  , SharedDirCache
  )
import qualified Corvus.Protocol as P
import qualified Corvus.Protocol.Apply as PA
import Corvus.Protocol.Build (BuildEvent)
import qualified Corvus.Protocol.CloudInit as PCI
import qualified Corvus.Protocol.Disk as PD
import qualified Corvus.Protocol.Network as PN
import qualified Corvus.Protocol.Node as PNode
import qualified Corvus.Protocol.SharedDir as PSd
import qualified Corvus.Protocol.SshKey as PSk
import qualified Corvus.Protocol.Task as PT
import qualified Corvus.Protocol.Template as PTm
import qualified Corvus.Protocol.Vm as PV
import Corvus.Wire.Apply (fromCapnpApplyResult)
import Corvus.Wire.Build (fromCapnpBuildEvent)
import Corvus.Wire.CloudInit (fromCapnpCloudInitInfo, toCapnpCloudInitInfo)
import Corvus.Wire.Common (EntityRef, ViewGrant (..), entityRefFromText, fromCapnpStatusInfo, fromCapnpViewGrant, toCapnpEntityRef)
import qualified Corvus.Wire.Common as WC
import qualified Corvus.Wire.Disk as WDisk
import Corvus.Wire.Enums
  ( toCapnpCacheType
  , toCapnpDriveFormat
  , toCapnpDriveInterface
  , toCapnpDriveMedia
  , toCapnpNetInterfaceType
  , toCapnpNodeAdminState
  , toCapnpSharedDirCache
  )
import Corvus.Wire.Errors (WireError, showWireError)
import qualified Corvus.Wire.Network as WNet
import qualified Corvus.Wire.Node as WNode
import qualified Corvus.Wire.SharedDir as WSd
import qualified Corvus.Wire.SshKey as WSsh
import qualified Corvus.Wire.Task as WTask
import qualified Corvus.Wire.Template as WTmpl
import qualified Corvus.Wire.Vm as WVm
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.Int (Int64)
import qualified Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------

-- | Call a method on a cap and return its parsed results struct.
-- The method is normally given as an overloaded label
-- ('#ping', '#vms', ...) which resolves to a 'C.Method' via the
-- 'C.HasMethod' / 'IsLabel' instances emitted by capnpc-haskell.
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
-- Daemon-level
-- ---------------------------------------------------------------------

rpcPing :: CapnpConnection -> IO ()
rpcPing conn = do
  _ :: C.Parsed CGCorvus.Daemon'ping'results <-
    callOn #ping CGCorvus.Daemon'ping'params (ccDaemon conn)
  pure ()

rpcStatus :: CapnpConnection -> IO P.StatusInfo
rpcStatus conn = do
  CGCorvus.Daemon'status'results {CGCorvus.info = info} <-
    callOn #status CGCorvus.Daemon'status'params (ccDaemon conn)
  pure (fromCapnpStatusInfo info)

rpcShutdown :: CapnpConnection -> IO ()
rpcShutdown conn = do
  _ :: C.Parsed CGCorvus.Daemon'shutdown'results <-
    callOn #shutdown CGCorvus.Daemon'shutdown'params (ccDaemon conn)
  pure ()

-- ---------------------------------------------------------------------
-- VM
-- ---------------------------------------------------------------------

rpcVmList :: CapnpConnection -> IO [PV.VmInfo]
rpcVmList conn = do
  CGCorvus.Daemon'vms'results {CGCorvus.mgr = mgr} <-
    callOn #vms CGCorvus.Daemon'vms'params (ccDaemon conn)
  CGVm.VmManager'list'results {CGVm.vms = vms} <-
    callOn #list CGVm.VmManager'list'params mgr
  traverse (failOnWire . WVm.fromCapnpVmInfo) vms

rpcVmShow :: CapnpConnection -> EntityRef -> IO PV.VmDetails
rpcVmShow conn refIn = do
  CGCorvus.Daemon'vms'results {CGCorvus.mgr = mgr} <-
    callOn #vms CGCorvus.Daemon'vms'params (ccDaemon conn)
  CGVm.VmManager'get'results {CGVm.vm = vmClient} <-
    callOn #get CGVm.VmManager'get'params {CGVm.ref = toCapnpEntityRef refIn} mgr
  CGVm.Vm'show'results {CGVm.details = det} <-
    callOn #show CGVm.Vm'show'params vmClient
  (details, _sharedDirs) <- failOnWire (WVm.fromCapnpVmDetails det)
  pure details

-- ---------------------------------------------------------------------
-- Disk
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
-- Network
-- ---------------------------------------------------------------------

rpcNetworkList :: CapnpConnection -> IO [PN.NetworkInfo]
rpcNetworkList conn = do
  CGCorvus.Daemon'networks'results {CGCorvus.mgr = mgr} <-
    callOn #networks CGCorvus.Daemon'networks'params (ccDaemon conn)
  CGNet.NetworkManager'list'results {CGNet.networks = ns} <-
    callOn #list CGNet.NetworkManager'list'params mgr
  pure (map WNet.fromCapnpNetworkInfo ns)

rpcNetworkShow :: CapnpConnection -> EntityRef -> IO PN.NetworkInfo
rpcNetworkShow conn refIn = do
  CGCorvus.Daemon'networks'results {CGCorvus.mgr = mgr} <-
    callOn #networks CGCorvus.Daemon'networks'params (ccDaemon conn)
  CGNet.NetworkManager'get'results {CGNet.network = nClient} <-
    callOn #get CGNet.NetworkManager'get'params {CGNet.ref = toCapnpEntityRef refIn} mgr
  CGNet.Network'show'results {CGNet.info = info} <-
    callOn #show CGNet.Network'show'params nClient
  pure (WNet.fromCapnpNetworkInfo info)

-- ---------------------------------------------------------------------
-- SSH keys
-- ---------------------------------------------------------------------

rpcSshKeyList :: CapnpConnection -> IO [PSk.SshKeyInfo]
rpcSshKeyList conn = do
  CGCorvus.Daemon'sshKeys'results {CGCorvus.mgr = mgr} <-
    callOn #sshKeys CGCorvus.Daemon'sshKeys'params (ccDaemon conn)
  CGSsh.SshKeyManager'list'results {CGSsh.keys = ks} <-
    callOn #list CGSsh.SshKeyManager'list'params mgr
  pure (map WSsh.fromCapnpSshKeyInfo ks)

-- ---------------------------------------------------------------------
-- Templates
-- ---------------------------------------------------------------------

rpcTemplateList :: CapnpConnection -> IO [PTm.TemplateVmInfo]
rpcTemplateList conn = do
  CGCorvus.Daemon'templates'results {CGCorvus.mgr = mgr} <-
    callOn #templates CGCorvus.Daemon'templates'params (ccDaemon conn)
  CGTmpl.TemplateManager'list'results {CGTmpl.templates = ts} <-
    callOn #list CGTmpl.TemplateManager'list'params mgr
  pure (map WTmpl.fromCapnpTemplateVmInfo ts)

rpcTemplateShow :: CapnpConnection -> EntityRef -> IO PTm.TemplateDetails
rpcTemplateShow conn refIn = do
  CGCorvus.Daemon'templates'results {CGCorvus.mgr = mgr} <-
    callOn #templates CGCorvus.Daemon'templates'params (ccDaemon conn)
  CGTmpl.TemplateManager'get'results {CGTmpl.template = tClient} <-
    callOn #get CGTmpl.TemplateManager'get'params {CGTmpl.ref = toCapnpEntityRef refIn} mgr
  CGTmpl.Template'show'results {CGTmpl.details = det} <-
    callOn #show CGTmpl.Template'show'params tClient
  failOnWire (WTmpl.fromCapnpTemplateDetails det)

-- ---------------------------------------------------------------------
-- Tasks
-- ---------------------------------------------------------------------

rpcTaskList :: CapnpConnection -> Int -> IO [PT.TaskInfo]
rpcTaskList conn limit = do
  CGCorvus.Daemon'tasks'results {CGCorvus.mgr = mgr} <-
    callOn #tasks CGCorvus.Daemon'tasks'params (ccDaemon conn)
  let inner =
        CGTask.TaskListParams
          { CGTask.limit = fromIntegral limit
          , CGTask.subsystem = toEnum 0
          , CGTask.hasSubsystem = False
          , CGTask.entityId = 0
          , CGTask.result = toEnum 0
          , CGTask.hasResult = False
          }
  CGTask.TaskManager'list'results {CGTask.tasks = ts} <-
    callOn #list CGTask.TaskManager'list'params {CGTask.params = inner} mgr
  traverse (failOnWire . WTask.fromCapnpTaskInfo) ts

rpcTaskShow :: CapnpConnection -> Int64 -> IO PT.TaskInfo
rpcTaskShow conn taskId = do
  CGCorvus.Daemon'tasks'results {CGCorvus.mgr = mgr} <-
    callOn #tasks CGCorvus.Daemon'tasks'params (ccDaemon conn)
  CGTask.TaskManager'get'results {CGTask.task = tClient} <-
    callOn #get CGTask.TaskManager'get'params {CGTask.taskId = taskId} mgr
  CGTask.Task'show'results {CGTask.info = info} <-
    callOn #show CGTask.Task'show'params tClient
  failOnWire (WTask.fromCapnpTaskInfo info)

-- =====================================================================
-- VM lifecycle wrappers
-- =====================================================================

-- | Create a new VM. Returns the new VM's id.
rpcVmCreate
  :: CapnpConnection
  -> Text
  -- ^ name
  -> Text
  -- ^ node reference (name or numeric id)
  -> Int
  -- ^ cpus
  -> Int
  -- ^ ram (MB)
  -> Maybe Text
  -- ^ description
  -> Bool
  -- ^ headless
  -> Bool
  -- ^ guest agent
  -> Bool
  -- ^ cloud-init
  -> Bool
  -- ^ autostart
  -> Bool
  -- ^ rebootQuirk
  -> Text
  -- ^ cpuModel
  -> IO Int64
rpcVmCreate conn name nodeRef cpus ram desc headless ga ci autostart rq cm = do
  CGCorvus.Daemon'vms'results {CGCorvus.mgr = mgr} <-
    callOn #vms CGCorvus.Daemon'vms'params (ccDaemon conn)
  let inner =
        CGVm.VmCreateParams
          { CGVm.name = name
          , CGVm.node = toCapnpEntityRef (entityRefFromText nodeRef)
          , CGVm.cpuCount = fromIntegral cpus
          , CGVm.ramMb = fromIntegral ram
          , CGVm.description = Data.Maybe.fromMaybe "" desc
          , CGVm.headless = headless
          , CGVm.guestAgent = ga
          , CGVm.cloudInit = ci
          , CGVm.autostart = autostart
          , CGVm.rebootQuirk = rq
          , CGVm.cpuModel = cm
          }
  CGVm.VmManager'create'results {CGVm.vm = vmClient} <-
    callOn #create CGVm.VmManager'create'params {CGVm.params = inner} mgr
  -- Pull the id by calling show on the new cap.
  CGVm.Vm'show'results {CGVm.details = det} <-
    callOn #show CGVm.Vm'show'params vmClient
  case det of
    CGVm.VmDetails {CGVm.id = vid} -> pure vid

rpcVmStart :: CapnpConnection -> EntityRef -> Bool -> IO ()
rpcVmStart conn ref wait = do
  vmClient <- getVmClient conn ref
  _ <- callOn #start CGVm.Vm'start'params {CGVm.wait = wait} vmClient
  pure ()

rpcVmStop :: CapnpConnection -> EntityRef -> Bool -> IO ()
rpcVmStop conn ref wait = do
  vmClient <- getVmClient conn ref
  _ <- callOn #stop CGVm.Vm'stop'params {CGVm.wait = wait} vmClient
  pure ()

rpcVmPause :: CapnpConnection -> EntityRef -> IO ()
rpcVmPause conn ref = do
  vmClient <- getVmClient conn ref
  _ <- callOn #pause CGVm.Vm'pause'params vmClient
  pure ()

rpcVmReset :: CapnpConnection -> EntityRef -> IO ()
rpcVmReset conn ref = do
  vmClient <- getVmClient conn ref
  _ <- callOn #reset CGVm.Vm'reset'params vmClient
  pure ()

rpcVmSave :: CapnpConnection -> EntityRef -> IO ()
rpcVmSave conn ref = do
  vmClient <- getVmClient conn ref
  _ <- callOn #save CGVm.Vm'save'params vmClient
  pure ()

rpcVmDelete :: CapnpConnection -> EntityRef -> Bool -> IO ()
rpcVmDelete conn ref keepDisks = do
  vmClient <- getVmClient conn ref
  _ <- callOn #delete CGVm.Vm'delete'params {CGVm.keepDisks = keepDisks} vmClient
  pure ()

-- | Migrate a stopped VM to another node. Returns the task id so
-- the caller can subscribe to migration progress events.
rpcVmMigrate :: CapnpConnection -> EntityRef -> EntityRef -> IO Int64
rpcVmMigrate conn vmRef toNodeRef = do
  vmClient <- getVmClient conn vmRef
  let p = CGVm.VmMigrateParams {CGVm.toNodeRef = toCapnpEntityRef toNodeRef}
  CGVm.Vm'migrate'results {CGVm.taskId = tid} <-
    callOn #migrate CGVm.Vm'migrate'params {CGVm.params = p} vmClient
  pure tid

-- =====================================================================
-- Disk lifecycle wrappers
-- =====================================================================

-- | Create a new disk image. Returns the new disk's id.
rpcDiskCreate
  :: CapnpConnection
  -> Text
  -- ^ name
  -> Int64
  -- ^ size (MB)
  -> CGE.DriveFormat
  -- ^ format (wire-side enum)
  -> Bool
  -- ^ ephemeral
  -> EntityRef
  -- ^ target node (unset = scheduler picks)
  -> IO Int64
rpcDiskCreate conn name sizeMb fmt ephemeral nodeRef = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let inner =
        CGDisk.DiskCreateParams
          { CGDisk.name = name
          , CGDisk.sizeMb = sizeMb
          , CGDisk.format = fmt
          , CGDisk.ephemeral = ephemeral
          , CGDisk.node = toCapnpEntityRef nodeRef
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

-- =====================================================================
-- Network lifecycle wrappers
-- =====================================================================

-- | Create a new virtual network. Returns the new network's id.
rpcNetworkCreate
  :: CapnpConnection
  -> Text
  -- ^ name
  -> Text
  -- ^ node ref (name or numeric id)
  -> Text
  -- ^ subnet (CIDR)
  -> Bool
  -- ^ dhcp
  -> Bool
  -- ^ nat
  -> Bool
  -- ^ autostart
  -> IO Int64
rpcNetworkCreate conn name nodeRef subnet dhcp nat autostart = do
  CGCorvus.Daemon'networks'results {CGCorvus.mgr = mgr} <-
    callOn #networks CGCorvus.Daemon'networks'params (ccDaemon conn)
  let inner =
        CGNet.NetworkCreateParams
          { CGNet.name = name
          , CGNet.node = toCapnpEntityRef (entityRefFromText nodeRef)
          , CGNet.subnet = subnet
          , CGNet.dhcp = dhcp
          , CGNet.nat = nat
          , CGNet.autostart = autostart
          }
  CGNet.NetworkManager'create'results {CGNet.network = nClient} <-
    callOn #create CGNet.NetworkManager'create'params {CGNet.params = inner} mgr
  CGNet.Network'show'results {CGNet.info = info} <-
    callOn #show CGNet.Network'show'params nClient
  case info of CGNet.NetworkInfo {CGNet.id = nid} -> pure nid

rpcNetworkStart :: CapnpConnection -> EntityRef -> IO ()
rpcNetworkStart conn ref = do
  nClient <- getNetworkClient conn ref
  _ <- callOn #start CGNet.Network'start'params nClient
  pure ()

rpcNetworkStop :: CapnpConnection -> EntityRef -> Bool -> IO ()
rpcNetworkStop conn ref force = do
  nClient <- getNetworkClient conn ref
  _ <- callOn #stop CGNet.Network'stop'params {CGNet.force = force} nClient
  pure ()

rpcNetworkDelete :: CapnpConnection -> EntityRef -> IO ()
rpcNetworkDelete conn ref = do
  nClient <- getNetworkClient conn ref
  _ <- callOn #delete CGNet.Network'delete'params nClient
  pure ()

rpcNetworkAttachNode :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcNetworkAttachNode conn nwRef nodeRef = do
  nClient <- getNetworkClient conn nwRef
  let params =
        CGNet.NetworkPeerParams {CGNet.node = toCapnpEntityRef nodeRef}
  _ <-
    callOn
      #attachNode
      CGNet.Network'attachNode'params {CGNet.params = params}
      nClient
  pure ()

rpcNetworkDetachNode :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcNetworkDetachNode conn nwRef nodeRef = do
  nClient <- getNetworkClient conn nwRef
  let params =
        CGNet.NetworkPeerParams {CGNet.node = toCapnpEntityRef nodeRef}
  _ <-
    callOn
      #detachNode
      CGNet.Network'detachNode'params {CGNet.params = params}
      nClient
  pure ()

-- =====================================================================
-- SSH key lifecycle wrappers
-- =====================================================================

rpcSshKeyCreate :: CapnpConnection -> Text -> Text -> IO Int64
rpcSshKeyCreate conn name publicKey = do
  CGCorvus.Daemon'sshKeys'results {CGCorvus.mgr = mgr} <-
    callOn #sshKeys CGCorvus.Daemon'sshKeys'params (ccDaemon conn)
  let inner = CGSsh.SshKeyCreateParams {CGSsh.name = name, CGSsh.publicKey = publicKey}
  CGSsh.SshKeyManager'create'results {CGSsh.key = kClient} <-
    callOn #create CGSsh.SshKeyManager'create'params {CGSsh.params = inner} mgr
  CGSsh.SshKey'show'results {CGSsh.info = info} <-
    callOn #show CGSsh.SshKey'show'params kClient
  case info of CGSsh.SshKeyInfo {CGSsh.id = kid} -> pure kid

rpcSshKeyDelete :: CapnpConnection -> EntityRef -> IO ()
rpcSshKeyDelete conn ref = do
  CGCorvus.Daemon'sshKeys'results {CGCorvus.mgr = mgr} <-
    callOn #sshKeys CGCorvus.Daemon'sshKeys'params (ccDaemon conn)
  CGSsh.SshKeyManager'get'results {CGSsh.key = kClient} <-
    callOn #get CGSsh.SshKeyManager'get'params {CGSsh.ref = toCapnpEntityRef ref} mgr
  _ <- callOn #delete CGSsh.SshKey'delete'params kClient
  pure ()

-- =====================================================================
-- Template lifecycle wrappers
-- =====================================================================

rpcTemplateCreate :: CapnpConnection -> Text -> IO Int64
rpcTemplateCreate conn yaml = do
  CGCorvus.Daemon'templates'results {CGCorvus.mgr = mgr} <-
    callOn #templates CGCorvus.Daemon'templates'params (ccDaemon conn)
  CGTmpl.TemplateManager'create'results {CGTmpl.template = tClient} <-
    callOn #create CGTmpl.TemplateManager'create'params {CGTmpl.yaml = yaml} mgr
  CGTmpl.Template'show'results {CGTmpl.details = det} <-
    callOn #show CGTmpl.Template'show'params tClient
  case det of CGTmpl.TemplateDetails {CGTmpl.id = tid} -> pure tid

rpcTemplateDelete :: CapnpConnection -> EntityRef -> IO ()
rpcTemplateDelete conn ref = do
  CGCorvus.Daemon'templates'results {CGCorvus.mgr = mgr} <-
    callOn #templates CGCorvus.Daemon'templates'params (ccDaemon conn)
  CGTmpl.TemplateManager'get'results {CGTmpl.template = tClient} <-
    callOn #get CGTmpl.TemplateManager'get'params {CGTmpl.ref = toCapnpEntityRef ref} mgr
  _ <- callOn #delete CGTmpl.Template'delete'params tClient
  pure ()

-- =====================================================================
-- Helpers: walk to a resource cap by EntityRef
-- =====================================================================

getVmClient :: CapnpConnection -> EntityRef -> IO (C.Client CGVm.Vm)
getVmClient conn ref = do
  CGCorvus.Daemon'vms'results {CGCorvus.mgr = mgr} <-
    callOn #vms CGCorvus.Daemon'vms'params (ccDaemon conn)
  CGVm.VmManager'get'results {CGVm.vm = vmClient} <-
    callOn #get CGVm.VmManager'get'params {CGVm.ref = toCapnpEntityRef ref} mgr
  pure vmClient

getDiskClient :: CapnpConnection -> EntityRef -> IO (C.Client CGDisk.Disk)
getDiskClient conn ref = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  CGDisk.DiskManager'get'results {CGDisk.disk = dClient} <-
    callOn #get CGDisk.DiskManager'get'params {CGDisk.ref = toCapnpEntityRef ref} mgr
  pure dClient

getNetworkClient :: CapnpConnection -> EntityRef -> IO (C.Client CGNet.Network)
getNetworkClient conn ref = do
  CGCorvus.Daemon'networks'results {CGCorvus.mgr = mgr} <-
    callOn #networks CGCorvus.Daemon'networks'params (ccDaemon conn)
  CGNet.NetworkManager'get'results {CGNet.network = nClient} <-
    callOn #get CGNet.NetworkManager'get'params {CGNet.ref = toCapnpEntityRef ref} mgr
  pure nClient

-- =====================================================================
-- ---------------------------------------------------------------------
-- Empty-EntityRef placeholder for "no network reference" etc.
-- ---------------------------------------------------------------------

emptyCapnpEntityRef :: C.Parsed CGCommon.EntityRef
emptyCapnpEntityRef =
  CGCommon.EntityRef {CGCommon.union' = CGCommon.EntityRef'id 0}

-- =====================================================================
-- VM additional wrappers
-- =====================================================================

rpcVmEdit
  :: CapnpConnection
  -> EntityRef
  -> Maybe Int
  -- ^ new cpus
  -> Maybe Int
  -- ^ new ram (MB)
  -> Maybe Text
  -- ^ new description
  -> Maybe Bool
  -- ^ new headless
  -> Maybe Bool
  -- ^ new guest agent
  -> Maybe Bool
  -- ^ new cloud-init
  -> Maybe Bool
  -- ^ new autostart
  -> Maybe Bool
  -- ^ new rebootQuirk
  -> Maybe Text
  -- ^ new cpuModel
  -> IO ()
rpcVmEdit conn ref mCpus mRam mDesc mHeadless mGa mCi mAs mRq mCm = do
  vmClient <- getVmClient conn ref
  let p =
        CGVm.VmEditParams
          { CGVm.hasName = False
          , CGVm.name = ""
          , CGVm.hasCpuCount = Data.Maybe.isJust mCpus
          , CGVm.cpuCount = maybe 0 fromIntegral mCpus
          , CGVm.hasRamMb = Data.Maybe.isJust mRam
          , CGVm.ramMb = maybe 0 fromIntegral mRam
          , CGVm.hasDescription = Data.Maybe.isJust mDesc
          , CGVm.description = Data.Maybe.fromMaybe "" mDesc
          , CGVm.hasHeadless = Data.Maybe.isJust mHeadless
          , CGVm.headless = Data.Maybe.fromMaybe False mHeadless
          , CGVm.hasGuestAgent = Data.Maybe.isJust mGa
          , CGVm.guestAgent = Data.Maybe.fromMaybe False mGa
          , CGVm.hasCloudInit = Data.Maybe.isJust mCi
          , CGVm.cloudInit = Data.Maybe.fromMaybe False mCi
          , CGVm.hasAutostart = Data.Maybe.isJust mAs
          , CGVm.autostart = Data.Maybe.fromMaybe False mAs
          , CGVm.hasRebootQuirk = Data.Maybe.isJust mRq
          , CGVm.rebootQuirk = Data.Maybe.fromMaybe False mRq
          , CGVm.hasCpuModel = Data.Maybe.isJust mCm
          , CGVm.cpuModel = Data.Maybe.fromMaybe "" mCm
          }
  _ <- callOn #edit CGVm.Vm'edit'params {CGVm.params = p} vmClient
  pure ()

rpcVmCloudInit :: CapnpConnection -> EntityRef -> IO (Maybe PCI.CloudInitInfo)
rpcVmCloudInit conn ref = do
  vmClient <- getVmClient conn ref
  CGVm.Vm'cloudInit'results {CGVm.config = cfg} <-
    callOn #cloudInit CGVm.Vm'cloudInit'params vmClient
  -- The schema always returns a struct; we interpret "no per-VM custom
  -- config" as the zero default (hasUserData = hasNetworkConfig = False,
  -- injectSshKeys = False). Anything else is "custom".
  let parsed = fromCapnpCloudInitInfo cfg
  case parsed of
    PCI.CloudInitInfo {PCI.ciiUserData = Nothing, PCI.ciiNetworkConfig = Nothing, PCI.ciiInjectSshKeys = False} ->
      pure Nothing
    other -> pure (Just other)

rpcVmViewGrant :: CapnpConnection -> EntityRef -> IO ViewGrant
rpcVmViewGrant conn ref = do
  vmClient <- getVmClient conn ref
  CGVm.Vm'viewGrant'results {CGVm.grant = g} <-
    callOn #viewGrant CGVm.Vm'viewGrant'params vmClient
  pure (fromCapnpViewGrant g)

rpcVmSendCtrlAltDel :: CapnpConnection -> EntityRef -> IO ()
rpcVmSendCtrlAltDel conn ref = do
  vmClient <- getVmClient conn ref
  _ <- callOn #sendCtrlAltDel CGVm.Vm'sendCtrlAltDel'params vmClient
  pure ()

-- | Clear the daemon-side serial-console ring buffer for this VM.
-- Wired to the @Ctrl+] f@ keybinding in 'runRawTerminalSession'.
rpcVmSerialConsoleFlush :: CapnpConnection -> EntityRef -> IO ()
rpcVmSerialConsoleFlush conn ref = do
  vmClient <- getVmClient conn ref
  _ <- callOn #serialConsoleFlush CGVm.Vm'serialConsoleFlush'params vmClient
  pure ()

-- | Clear the daemon-side HMP-monitor ring buffer for this VM.
-- Wired to the @Ctrl+] f@ keybinding in 'runRawTerminalSession'
-- for HMP sessions.
rpcVmHmpMonitorFlush :: CapnpConnection -> EntityRef -> IO ()
rpcVmHmpMonitorFlush conn ref = do
  vmClient <- getVmClient conn ref
  _ <- callOn #hmpMonitorFlush CGVm.Vm'hmpMonitorFlush'params vmClient
  pure ()

-- | Guest agent exec: returns (exit code, stdout, stderr).
rpcGuestExec :: CapnpConnection -> EntityRef -> Text -> IO (Int, Text, Text)
rpcGuestExec conn ref cmd = do
  vmClient <- getVmClient conn ref
  CGVm.Vm'guestExec'results {CGVm.result = CGVm.GuestExecResult {..}} <-
    callOn #guestExec CGVm.Vm'guestExec'params {CGVm.command = cmd} vmClient
  pure (fromIntegral exitCode, stdout, stderr)

-- =====================================================================
-- Disk additional wrappers
-- =====================================================================

rpcDiskCreateOverlay :: CapnpConnection -> Text -> EntityRef -> Bool -> IO Int64
rpcDiskCreateOverlay conn name baseRef ephemeral = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let inner =
        CGDisk.DiskCreateOverlayParams
          { CGDisk.name = name
          , CGDisk.backingDiskRef = toCapnpEntityRef baseRef
          , CGDisk.ephemeral = ephemeral
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
  -> DriveFormat
  -> Bool
  -> EntityRef
  -- ^ target node (unset = scheduler picks)
  -> IO Int64
rpcDiskRegister conn name filePath fmt ephemeral nodeRef = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let p =
        CGDisk.DiskRegisterParams
          { CGDisk.name = name
          , CGDisk.filePath = filePath
          , CGDisk.format = capnpDriveFormat fmt
          , CGDisk.ephemeral = ephemeral
          , CGDisk.node = toCapnpEntityRef nodeRef
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

-- | Import a disk synchronously from a local source path.
rpcDiskImport
  :: CapnpConnection
  -> Text
  -- ^ name
  -> Text
  -- ^ srcPath
  -> DriveFormat
  -- ^ format
  -> Bool
  -- ^ ephemeral
  -> EntityRef
  -- ^ target node (unset = scheduler picks)
  -> IO Int64
rpcDiskImport conn name srcPath fmt ephemeral nodeRef = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let p =
        CGDisk.DiskImportParams
          { CGDisk.name = name
          , CGDisk.srcPath = srcPath
          , CGDisk.format = capnpDriveFormat fmt
          , CGDisk.ephemeral = ephemeral
          , CGDisk.node = toCapnpEntityRef nodeRef
          }
  CGDisk.DiskManager'import'results {CGDisk.disk = dClient} <-
    callOn #import_ CGDisk.DiskManager'import'params {CGDisk.params = p} mgr
  CGDisk.Disk'show'results {CGDisk.info = info} <-
    callOn #show CGDisk.Disk'show'params dClient
  case info of CGDisk.DiskImageInfo {CGDisk.id = did} -> pure did

rpcDiskClone :: CapnpConnection -> EntityRef -> Text -> Bool -> IO Int64
rpcDiskClone conn srcRef newName ephemeral = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let p =
        CGDisk.DiskCloneParams
          { CGDisk.sourceRef = toCapnpEntityRef srcRef
          , CGDisk.newName = newName
          , CGDisk.ephemeral = ephemeral
          }
  CGDisk.DiskManager'clone'results {CGDisk.disk = dClient} <-
    callOn #clone CGDisk.DiskManager'clone'params {CGDisk.params = p} mgr
  CGDisk.Disk'show'results {CGDisk.info = info} <-
    callOn #show CGDisk.Disk'show'params dClient
  case info of CGDisk.DiskImageInfo {CGDisk.id = did} -> pure did

rpcDiskRebase :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcDiskRebase conn diskRef newBackingRef = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let p =
        CGDisk.DiskRebaseParams
          { CGDisk.diskRef = toCapnpEntityRef diskRef
          , CGDisk.newBackingDiskRef = toCapnpEntityRef newBackingRef
          }
  _ <- callOn #rebase CGDisk.DiskManager'rebase'params {CGDisk.params = p} mgr
  pure ()

-- | Copy a disk image to another node. Returns the task id for
-- progress observation; the actual byte transfer happens
-- agent-to-agent in the background.
--
-- @toPath@ is the destination path on the new node. Pass
-- 'Nothing' (or @Just ""@) to preserve the source's relative
-- path; an absolute source path requires an explicit
-- destination, otherwise the daemon refuses the copy.
rpcDiskCopy
  :: CapnpConnection
  -> EntityRef
  -> EntityRef
  -> Maybe Text
  -> Bool
  -- ^ recursively stage missing backing-chain ancestors on the destination
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

-- | Move a disk image to another node. Same byte path as
-- 'rpcDiskCopy', but the daemon deletes the source-side placement
-- (and the file) on success.
rpcDiskMove
  :: CapnpConnection
  -> EntityRef
  -> EntityRef
  -> Maybe Text
  -> Bool
  -- ^ recursively stage missing backing-chain ancestors on the destination
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

-- | Attach a disk to a VM. Returns the new drive (attachment) id.
rpcDiskAttach
  :: CapnpConnection
  -> EntityRef
  -- ^ VM
  -> EntityRef
  -- ^ disk
  -> DriveInterface
  -> Maybe DriveMedia
  -- ^ media override (Nothing → @disk@)
  -> Bool
  -- ^ read-only
  -> Bool
  -- ^ discard
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

-- | Detach a disk by name or numeric reference. Walks the VM's
-- drive list to find the drive whose backing disk image matches
-- @diskRef@ (matching the name when @diskRef@ is symbolic, or the
-- disk id when @diskRef@ is numeric), then issues the detach.
-- Throws an exception when no drive matches.
rpcDiskDetachByDisk :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcDiskDetachByDisk conn vmRef diskRef = do
  details <- rpcVmShow conn vmRef
  let matches d = case diskRef of
        WC.RefById did -> PV.diDiskImageId d == did
        WC.RefByName name -> PV.diDiskImageName d == name
  case filter matches (PV.vdDrives details) of
    (drive : _) -> rpcDiskDetach conn vmRef (PV.diId drive)
    [] -> fail ("no drive on VM with disk " <> show diskRef)

-- =====================================================================
-- Snapshot wrappers (per-disk)
-- =====================================================================

rpcSnapshotList :: CapnpConnection -> EntityRef -> IO [PD.SnapshotInfo]
rpcSnapshotList conn diskRef = do
  dClient <- getDiskClient conn diskRef
  CGDisk.Disk'snapshotList'results {CGDisk.snapshots = ss} <-
    callOn #snapshotList CGDisk.Disk'snapshotList'params dClient
  pure (map WDisk.fromCapnpSnapshotInfo ss)

rpcSnapshotCreate :: CapnpConnection -> EntityRef -> Text -> IO Int64
rpcSnapshotCreate conn diskRef name = do
  dClient <- getDiskClient conn diskRef
  CGDisk.Disk'snapshotCreate'results {CGDisk.snapshot = sClient} <-
    callOn #snapshotCreate CGDisk.Disk'snapshotCreate'params {CGDisk.name = name} dClient
  -- Snapshot doesn't expose @show@; the Snapshot cap proves the
  -- creation succeeded.
  _ <- pure sClient
  pure 0

rpcSnapshotDelete :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcSnapshotDelete conn diskRef snapRef = do
  sClient <- getSnapshotClient conn diskRef snapRef
  _ <- callOn #delete CGDisk.Snapshot'delete'params sClient
  pure ()

rpcSnapshotRollback :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcSnapshotRollback conn diskRef snapRef = do
  sClient <- getSnapshotClient conn diskRef snapRef
  _ <- callOn #rollback CGDisk.Snapshot'rollback'params sClient
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

-- =====================================================================
-- Shared directory wrappers
-- =====================================================================

rpcSharedDirAdd
  :: CapnpConnection
  -> EntityRef
  -> Text
  -- ^ host path
  -> Text
  -- ^ tag
  -> SharedDirCache
  -> Bool
  -- ^ read-only
  -> IO Int64
rpcSharedDirAdd conn vmRef path tag cache readOnly = do
  vmClient <- getVmClient conn vmRef
  let p =
        CGVm.SharedDirAddParams
          { CGVm.path = path
          , CGVm.tag = tag
          , CGVm.cache = toCapnpSharedDirCache cache
          , CGVm.readOnly = readOnly
          }
  CGVm.Vm'addSharedDir'results {CGVm.sharedDirId = sid} <-
    callOn #addSharedDir CGVm.Vm'addSharedDir'params {CGVm.params = p} vmClient
  pure sid

rpcSharedDirRemove :: CapnpConnection -> EntityRef -> Int64 -> IO ()
rpcSharedDirRemove conn vmRef sharedDirId = do
  vmClient <- getVmClient conn vmRef
  _ <-
    callOn
      #removeSharedDir
      CGVm.Vm'removeSharedDir'params {CGVm.sharedDirId = sharedDirId}
      vmClient
  pure ()

rpcSharedDirList :: CapnpConnection -> EntityRef -> IO [PSd.SharedDirInfo]
rpcSharedDirList conn vmRef = do
  vmClient <- getVmClient conn vmRef
  CGVm.Vm'listSharedDirs'results {CGVm.sharedDirs = sds} <-
    callOn #listSharedDirs CGVm.Vm'listSharedDirs'params vmClient
  traverse (failOnWire . WSd.fromCapnpSharedDirInfo) sds

-- =====================================================================
-- Network interface wrappers (per-VM)
-- =====================================================================

rpcNetIfAdd
  :: CapnpConnection
  -> EntityRef
  -> NetInterfaceType
  -> Text
  -- ^ host device (\"\" → auto)
  -> Maybe Text
  -- ^ MAC pin (Nothing → daemon picks)
  -> Maybe EntityRef
  -- ^ managed network
  -> IO Int64
rpcNetIfAdd conn vmRef ifaceType hostDevice macAddress mNetwork = do
  vmClient <- getVmClient conn vmRef
  let p =
        CGVm.NetIfAddParams
          { CGVm.type_ = toCapnpNetInterfaceType ifaceType
          , CGVm.hostDevice = hostDevice
          , CGVm.macAddress = Data.Maybe.fromMaybe "" macAddress
          , CGVm.networkRef = maybe emptyCapnpEntityRef toCapnpEntityRef mNetwork
          }
  CGVm.Vm'addNetIf'results {CGVm.netIfId = nid} <-
    callOn #addNetIf CGVm.Vm'addNetIf'params {CGVm.params = p} vmClient
  pure nid

rpcNetIfRemove :: CapnpConnection -> EntityRef -> Int64 -> IO ()
rpcNetIfRemove conn vmRef netIfId = do
  vmClient <- getVmClient conn vmRef
  _ <-
    callOn
      #removeNetIf
      CGVm.Vm'removeNetIf'params {CGVm.netIfId = netIfId}
      vmClient
  pure ()

rpcNetIfList :: CapnpConnection -> EntityRef -> IO [PV.NetIfInfo]
rpcNetIfList conn vmRef = do
  vmClient <- getVmClient conn vmRef
  CGVm.Vm'listNetIfs'results {CGVm.netIfs = nis} <-
    callOn #listNetIfs CGVm.Vm'listNetIfs'params vmClient
  traverse (failOnWire . WVm.fromCapnpNetIfInfo) nis

-- =====================================================================
-- Network edit
-- =====================================================================

rpcNetworkEdit
  :: CapnpConnection
  -> EntityRef
  -> Maybe Text
  -- ^ subnet
  -> Maybe Bool
  -- ^ dhcp
  -> Maybe Bool
  -- ^ nat
  -> Maybe Bool
  -- ^ autostart
  -> IO ()
rpcNetworkEdit conn ref mSubnet mDhcp mNat mAs = do
  nClient <- getNetworkClient conn ref
  let p =
        CGNet.NetworkEditParams
          { CGNet.hasName = False
          , CGNet.name = ""
          , CGNet.hasSubnet = Data.Maybe.isJust mSubnet
          , CGNet.subnet = Data.Maybe.fromMaybe "" mSubnet
          , CGNet.hasDhcp = Data.Maybe.isJust mDhcp
          , CGNet.dhcp = Data.Maybe.fromMaybe False mDhcp
          , CGNet.hasNat = Data.Maybe.isJust mNat
          , CGNet.nat = Data.Maybe.fromMaybe False mNat
          , CGNet.hasAutostart = Data.Maybe.isJust mAs
          , CGNet.autostart = Data.Maybe.fromMaybe False mAs
          }
  _ <- callOn #edit CGNet.Network'edit'params {CGNet.params = p} nClient
  pure ()

-- =====================================================================
-- SSH key attach / detach / list-for-vm
-- =====================================================================

rpcSshKeyAttach :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcSshKeyAttach conn vmRef keyRef = do
  vmClient <- getVmClient conn vmRef
  _ <-
    callOn
      #attachSshKey
      CGVm.Vm'attachSshKey'params {CGVm.keyRef = toCapnpEntityRef keyRef}
      vmClient
  pure ()

rpcSshKeyDetach :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcSshKeyDetach conn vmRef keyRef = do
  vmClient <- getVmClient conn vmRef
  _ <-
    callOn
      #detachSshKey
      CGVm.Vm'detachSshKey'params {CGVm.keyRef = toCapnpEntityRef keyRef}
      vmClient
  pure ()

rpcSshKeyListForVm :: CapnpConnection -> EntityRef -> IO [PSk.SshKeyInfo]
rpcSshKeyListForVm conn vmRef = do
  vmClient <- getVmClient conn vmRef
  CGVm.Vm'listSshKeys'results {CGVm.keys = ks} <-
    callOn #listSshKeys CGVm.Vm'listSshKeys'params vmClient
  pure (map WSsh.fromCapnpSshKeyInfo ks)

-- =====================================================================
-- Template additional wrappers
-- =====================================================================

rpcTemplateUpdate :: CapnpConnection -> EntityRef -> Text -> IO ()
rpcTemplateUpdate conn ref yaml = do
  CGCorvus.Daemon'templates'results {CGCorvus.mgr = mgr} <-
    callOn #templates CGCorvus.Daemon'templates'params (ccDaemon conn)
  CGTmpl.TemplateManager'get'results {CGTmpl.template = tClient} <-
    callOn #get CGTmpl.TemplateManager'get'params {CGTmpl.ref = toCapnpEntityRef ref} mgr
  _ <- callOn #update CGTmpl.Template'update'params {CGTmpl.yaml = yaml} tClient
  pure ()

rpcTemplateInstantiate :: CapnpConnection -> EntityRef -> Text -> Text -> IO Int64
rpcTemplateInstantiate conn ref vmName nodeRef = do
  CGCorvus.Daemon'templates'results {CGCorvus.mgr = mgr} <-
    callOn #templates CGCorvus.Daemon'templates'params (ccDaemon conn)
  CGTmpl.TemplateManager'get'results {CGTmpl.template = tClient} <-
    callOn #get CGTmpl.TemplateManager'get'params {CGTmpl.ref = toCapnpEntityRef ref} mgr
  CGTmpl.Template'instantiate'results {CGTmpl.vm = vmClient} <-
    callOn
      #instantiate
      CGTmpl.Template'instantiate'params
        { CGTmpl.name = vmName
        , CGTmpl.node = toCapnpEntityRef (entityRefFromText nodeRef)
        }
      tClient
  CGVm.Vm'show'results {CGVm.details = det} <-
    callOn #show CGVm.Vm'show'params vmClient
  case det of CGVm.VmDetails {CGVm.id = vid} -> pure vid

-- =====================================================================
-- Apply
-- =====================================================================

-- | Apply a declarative YAML environment. Returns the populated
-- result (when @wait=True@) or just the parent task id (when
-- @wait=False@).
rpcApply :: CapnpConnection -> Text -> Bool -> Bool -> IO (PA.ApplyResult, Int64)
rpcApply conn yaml skipExisting wait = do
  CGCorvus.Daemon'apply'results {CGCorvus.result = r, CGCorvus.taskId = tid} <-
    callOn
      #apply
      CGCorvus.Daemon'apply'params
        { CGCorvus.yaml = yaml
        , CGCorvus.skipExisting = skipExisting
        , CGCorvus.wait = wait
        }
      (ccDaemon conn)
  pure (fromCapnpApplyResult r, tid)

-- =====================================================================
-- Build (streaming)
-- =====================================================================

-- | Client-side @BuildEventSink@ implementation. The daemon calls
-- 'push' for each 'BuildEvent' emitted by the pipeline and 'end'
-- once when the pipeline finishes; both forward to the IO actions
-- the caller supplied.
data ClientBuildEventSink = ClientBuildEventSink
  { cbsOnEvent :: BuildEvent -> IO ()
  , cbsOnEnd :: IO ()
  }

instance SomeServer ClientBuildEventSink

instance CGS.BuildEventSink'server_ ClientBuildEventSink where
  buildEventSink'push (ClientBuildEventSink onEv _) =
    handleParsed $ \CGS.BuildEventSink'push'params {CGS.event = cev} -> do
      case fromCapnpBuildEvent cev of
        Right ev -> do
          _ <- try (onEv ev) :: IO (Either SomeException ())
          pure ()
        Left _ -> pure ()
      pure CGS.BuildEventSink'push'results

  buildEventSink'end (ClientBuildEventSink _ onEnd) =
    handleParsed $ \_ -> do
      _ <- try onEnd :: IO (Either SomeException ())
      pure CGS.BuildEventSink'end'results

-- | Run a build pipeline on the daemon, streaming each emitted
-- 'BuildEvent' through @onEvent@. Returns the parent task id as
-- soon as the daemon has created it; @onEvent@ continues to fire
-- on the connection's supervisor threads until the daemon calls
-- @end@ on the sink, at which point @onEnd@ is invoked once.
--
-- The caller is expected to block (e.g. on an MVar populated by
-- @onEnd@) if it wants to wait for completion.
rpcBuild
  :: CapnpConnection
  -> Text
  -> (BuildEvent -> IO ())
  -> IO ()
  -> IO Int64
rpcBuild conn yaml onEvent onEnd = do
  sinkClient <- export @CGS.BuildEventSink (ccSupervisor conn) (ClientBuildEventSink onEvent onEnd)
  CGCorvus.Daemon'build'results {CGCorvus.taskId = tid} <-
    callOn
      #build
      CGCorvus.Daemon'build'params {CGCorvus.yaml = yaml, CGCorvus.sink = sinkClient}
      (ccDaemon conn)
  pure tid

-- =====================================================================
-- VM streaming (Phase 6b)
-- =====================================================================

-- | Client-side 'ByteSink' implementation. The daemon calls 'write'
-- once per chunk of QEMU output; 'end' is invoked when the daemon
-- closes the relay (e.g. QEMU exited).
data ClientByteSink = ClientByteSink
  { cbsOnWrite :: BS.ByteString -> IO ()
  , cbsOnEnd :: IO ()
  }

instance SomeServer ClientByteSink

instance CGS.ByteSink'server_ ClientByteSink where
  byteSink'write (ClientByteSink onWrite _) =
    handleParsed $ \CGS.ByteSink'write'params {CGS.chunk = chunk} -> do
      _ <- try (onWrite chunk) :: IO (Either SomeException ())
      pure CGS.ByteSink'write'results
  byteSink'end (ClientByteSink _ onEnd) =
    handleParsed $ \_ -> do
      _ <- try onEnd :: IO (Either SomeException ())
      pure CGS.ByteSink'end'results

-- | Internal helper: factor the @ByteSink-in -> input cap-out@
-- dance shared by 'rpcVmSerialConsole' and 'rpcVmHmpMonitor'.
-- Returns a tuple of @(writeInput, endInput)@: call @writeInput@
-- to forward a chunk of client input to QEMU, call @endInput@ when
-- you're done.
streamByteSinkMethod
  :: ( C.IsCap iface
     , C.IsStruct params
     , C.IsStruct results
     , C.Parse params (C.Parsed params)
     , C.Parse results (C.Parsed results)
     )
  => CapnpConnection
  -> C.Client iface
  -> C.Method iface params results
  -> (C.Client CGS.ByteSink -> C.Parsed params)
  -- ^ Build the @params@ struct from the (just-exported) client
  -- output sink.
  -> (C.Parsed results -> C.Client CGS.ByteSink)
  -- ^ Extract the server-supplied input sink from the @results@.
  -> (BS.ByteString -> IO ())
  -- ^ Output callback: invoked with every chunk QEMU sent.
  -> IO ()
  -- ^ End callback: invoked once when the daemon closes the sink.
  -> IO (BS.ByteString -> IO (), IO ())
streamByteSinkMethod conn iface method mkParams getInput onOut onEnd = do
  outSink <- export @CGS.ByteSink (ccSupervisor conn) (ClientByteSink onOut onEnd)
  results <- callOn method (mkParams outSink) iface
  let input = getInput results
      writeInput chunk =
        Control.Monad.void $
          callOn
            #write
            CGS.ByteSink'write'params {CGS.chunk = chunk}
            input
      endInput =
        Control.Monad.void (callOn #end CGS.ByteSink'end'params input)
  pure (writeInput, endInput)

-- | Attach to a VM's serial console. Returns @(writeInput, end)@:
-- call @writeInput@ to forward client keystrokes to QEMU, call
-- @end@ when the client side closes the session. @onOutput@ fires
-- on every chunk of serial output from QEMU; @onEnd@ fires once
-- when the daemon closes its side of the relay (e.g. VM exits).
rpcVmSerialConsole
  :: CapnpConnection
  -> EntityRef
  -> (BS.ByteString -> IO ())
  -> IO ()
  -> IO (BS.ByteString -> IO (), IO ())
rpcVmSerialConsole conn vmRef onOutput onEnd = do
  vmClient <- getVmClient conn vmRef
  streamByteSinkMethod
    conn
    vmClient
    #serialConsole
    (\sink -> CGVm.Vm'serialConsole'params {CGVm.sink = sink})
    (\CGVm.Vm'serialConsole'results {CGVm.input} -> input)
    onOutput
    onEnd

-- | Attach to a VM's HMP monitor. Same shape as
-- 'rpcVmSerialConsole'.
rpcVmHmpMonitor
  :: CapnpConnection
  -> EntityRef
  -> (BS.ByteString -> IO ())
  -> IO ()
  -> IO (BS.ByteString -> IO (), IO ())
rpcVmHmpMonitor conn vmRef onOutput onEnd = do
  vmClient <- getVmClient conn vmRef
  streamByteSinkMethod
    conn
    vmClient
    #hmpMonitor
    (\sink -> CGVm.Vm'hmpMonitor'params {CGVm.sink = sink})
    (\CGVm.Vm'hmpMonitor'results {CGVm.input} -> input)
    onOutput
    onEnd

-- =====================================================================
-- Guest-agent subscription (Phase 6d)
-- =====================================================================

-- | Plain Haskell view of the Cap'n Proto @GuestAgentStatus@ struct.
-- One of these is delivered to the callback for every poll cycle on
-- a subscribed VM.
data GuestAgentStatusEvent = GuestAgentStatusEvent
  { gaseVmId :: !Int64
  , gaseLastHealthcheck :: !Int64
  -- ^ POSIX nanoseconds; @0@ when the agent has never been reached.
  , gaseEnabled :: !Bool
  , gaseReachable :: !Bool
  , gaseMessage :: !Text
  }
  deriving (Eq, Show)

newtype ClientGuestAgentSink = ClientGuestAgentSink
  { cgasOnStatus :: GuestAgentStatusEvent -> IO ()
  }

instance SomeServer ClientGuestAgentSink

instance CGS.GuestAgentStatusSink'server_ ClientGuestAgentSink where
  guestAgentStatusSink'push (ClientGuestAgentSink onStatus) =
    handleParsed $ \CGS.GuestAgentStatusSink'push'params {CGS.status = s} -> do
      let CGS.GuestAgentStatus
            { CGS.vmId = vid
            , CGS.lastHealthcheck = lhc
            , CGS.enabled = en
            , CGS.reachable = rc
            } = s
          msg = case s of
            CGS.GuestAgentStatus {CGS.message = m} -> m
          ev =
            GuestAgentStatusEvent
              { gaseVmId = vid
              , gaseLastHealthcheck = lhc
              , gaseEnabled = en
              , gaseReachable = rc
              , gaseMessage = msg
              }
      _ <- try (onStatus ev) :: IO (Either SomeException ())
      pure CGS.GuestAgentStatusSink'push'results

-- | Subscribe to guest-agent status updates for a VM. The returned
-- 'C.Client' 'CGS.Handle' is the subscription lifetime token —
-- drop it (or let it get GC'd) and the daemon prunes the
-- subscriber on its next push attempt. @onStatus@ fires once per
-- daemon poll cycle (default: every few seconds) for as long as
-- the subscription is alive.
rpcVmSubscribeGuestAgent
  :: CapnpConnection
  -> EntityRef
  -> (GuestAgentStatusEvent -> IO ())
  -> IO (C.Client CGS.Handle)
rpcVmSubscribeGuestAgent conn vmRef onStatus = do
  vmClient <- getVmClient conn vmRef
  sinkClient <-
    export @CGS.GuestAgentStatusSink
      (ccSupervisor conn)
      (ClientGuestAgentSink onStatus)
  CGVm.Vm'subscribeGuestAgent'results {CGVm.handle} <-
    callOn
      #subscribeGuestAgent
      CGVm.Vm'subscribeGuestAgent'params {CGVm.sink = sinkClient}
      vmClient
  pure handle

-- =====================================================================
-- Task progress subscription (Phase 6e)
-- =====================================================================

-- | Plain Haskell view of a 'CGS.TaskProgressEvent'. The union
-- variant is preserved as a sum type; @progress@ events aren't
-- emitted by the current daemon (Phase 6e ships only the terminal
-- @finished@ event), but the type covers the full schema so
-- future progress-emitting actions don't break the API.
data TaskProgressEvent
  = -- | taskId, command, subsystem (as text)
    TpeStarted !Int64 !Text !Text
  | -- | taskId, completed, total, label
    TpeProgress !Int64 !Int64 !Int64 !Text
  | -- | taskId, result (as text), message
    TpeFinished !Int64 !Text !Text
  | TpeUnknown !Int64
  deriving (Eq, Show)

newtype ClientTaskProgressSink = ClientTaskProgressSink
  { ctpsOnEvent :: TaskProgressEvent -> IO ()
  }

instance SomeServer ClientTaskProgressSink

instance CGS.TaskProgressSink'server_ ClientTaskProgressSink where
  taskProgressSink'push (ClientTaskProgressSink onEv) =
    handleParsed $ \CGS.TaskProgressSink'push'params {CGS.event = e} -> do
      let CGS.TaskProgressEvent {CGS.taskId = tid, CGS.union' = u} = e
          decoded = case u of
            CGS.TaskProgressEvent'started
              CGS.TaskProgressEvent'started' {CGS.command = c, CGS.subsystem = ss} ->
                TpeStarted tid c (T.pack (show ss))
            CGS.TaskProgressEvent'progress
              CGS.TaskProgressEvent'progress' {CGS.completed = co, CGS.total = to, CGS.label = lbl} ->
                TpeProgress tid co to lbl
            CGS.TaskProgressEvent'finished
              CGS.TaskProgressEvent'finished' {CGS.result = r, CGS.message = m} ->
                TpeFinished tid (T.pack (show r)) m
            CGS.TaskProgressEvent'unknown' _ -> TpeUnknown tid
      _ <- try (onEv decoded) :: IO (Either SomeException ())
      pure CGS.TaskProgressSink'push'results

-- | Subscribe to progress events for a specific task. The daemon
-- pushes a terminal @finished@ event when the task completes;
-- starting / progress events are reserved for future use. Drop
-- the returned 'CGS.Handle' to stop receiving events.
rpcTaskSubscribe
  :: CapnpConnection
  -> Int64
  -- ^ Task id to watch.
  -> (TaskProgressEvent -> IO ())
  -> IO (C.Client CGS.Handle)
rpcTaskSubscribe conn tid onEvent = do
  CGCorvus.Daemon'tasks'results {CGCorvus.mgr = mgr} <-
    callOn #tasks CGCorvus.Daemon'tasks'params (ccDaemon conn)
  sinkClient <-
    export @CGS.TaskProgressSink
      (ccSupervisor conn)
      (ClientTaskProgressSink onEvent)
  CGTask.TaskManager'subscribe'results {CGTask.handle} <-
    callOn
      #subscribe
      CGTask.TaskManager'subscribe'params {CGTask.taskId = tid, CGTask.sink = sinkClient}
      mgr
  pure handle

-- =====================================================================
-- Cloud-init manager
-- =====================================================================

rpcCloudInitSet
  :: CapnpConnection
  -> EntityRef
  -> Maybe Text
  -- ^ user-data
  -> Maybe Text
  -- ^ network-config
  -> Bool
  -- ^ inject SSH keys
  -> IO ()
rpcCloudInitSet conn vmRef mUserData mNetworkConfig injectKeys = do
  CGCorvus.Daemon'cloudInit'results {CGCorvus.mgr = mgr} <-
    callOn #cloudInit CGCorvus.Daemon'cloudInit'params (ccDaemon conn)
  let info =
        PCI.CloudInitInfo
          { PCI.ciiUserData = mUserData
          , PCI.ciiNetworkConfig = mNetworkConfig
          , PCI.ciiInjectSshKeys = injectKeys
          }
      p =
        CGCI.CloudInitSetParams
          { CGCI.vmRef = toCapnpEntityRef vmRef
          , CGCI.config = toCapnpCloudInitInfo info
          }
  _ <- callOn #set CGCI.CloudInitManager'set'params {CGCI.params = p} mgr
  pure ()

rpcCloudInitGet :: CapnpConnection -> EntityRef -> IO (Maybe PCI.CloudInitInfo)
rpcCloudInitGet conn vmRef = do
  CGCorvus.Daemon'cloudInit'results {CGCorvus.mgr = mgr} <-
    callOn #cloudInit CGCorvus.Daemon'cloudInit'params (ccDaemon conn)
  CGCI.CloudInitManager'get'results {CGCI.config = cfg} <-
    callOn
      #get
      CGCI.CloudInitManager'get'params {CGCI.vmRef = toCapnpEntityRef vmRef}
      mgr
  let parsed = fromCapnpCloudInitInfo cfg
  case parsed of
    PCI.CloudInitInfo {PCI.ciiUserData = Nothing, PCI.ciiNetworkConfig = Nothing, PCI.ciiInjectSshKeys = False} ->
      pure Nothing
    other -> pure (Just other)

rpcCloudInitDelete :: CapnpConnection -> EntityRef -> IO ()
rpcCloudInitDelete conn vmRef = do
  CGCorvus.Daemon'cloudInit'results {CGCorvus.mgr = mgr} <-
    callOn #cloudInit CGCorvus.Daemon'cloudInit'params (ccDaemon conn)
  _ <-
    callOn
      #delete
      CGCI.CloudInitManager'delete'params {CGCI.vmRef = toCapnpEntityRef vmRef}
      mgr
  pure ()

-- =====================================================================
-- Task additional wrappers
-- =====================================================================

rpcTaskListChildren :: CapnpConnection -> Int64 -> IO [PT.TaskInfo]
rpcTaskListChildren conn parentId = do
  CGCorvus.Daemon'tasks'results {CGCorvus.mgr = mgr} <-
    callOn #tasks CGCorvus.Daemon'tasks'params (ccDaemon conn)
  CGTask.TaskManager'listChildren'results {CGTask.tasks = ts} <-
    callOn #listChildren CGTask.TaskManager'listChildren'params {CGTask.parentId = parentId} mgr
  traverse (failOnWire . WTask.fromCapnpTaskInfo) ts

-- ---------------------------------------------------------------------
-- Local enum coercions
-- ---------------------------------------------------------------------

-- | Mirror 'Corvus.Wire.Enums.toCapnpDriveFormat'.
capnpDriveFormat :: DriveFormat -> CGE.DriveFormat
capnpDriveFormat = toCapnpDriveFormat

-- =====================================================================
-- Node wrappers
-- =====================================================================

rpcNodeList :: CapnpConnection -> IO [PNode.NodeInfo]
rpcNodeList conn = do
  CGCorvus.Daemon'nodes'results {CGCorvus.mgr = mgr} <-
    callOn #nodes CGCorvus.Daemon'nodes'params (ccDaemon conn)
  CGNode.NodeManager'list'results {CGNode.nodes = ns} <-
    callOn #list CGNode.NodeManager'list'params mgr
  traverse (failOnWire . WNode.fromCapnpNodeInfo) ns

rpcNodeShow :: CapnpConnection -> EntityRef -> IO PNode.NodeDetails
rpcNodeShow conn ref = do
  nClient <- getNodeClient conn ref
  CGNode.Node'show'results {CGNode.details = det} <-
    callOn #show CGNode.Node'show'params nClient
  failOnWire (WNode.fromCapnpNodeDetails det)

rpcNodeAdd
  :: CapnpConnection
  -> Text
  -- ^ name
  -> Text
  -- ^ host
  -> Int
  -- ^ nodeAgentPort
  -> Int
  -- ^ netAgentPort
  -> Text
  -- ^ basePath
  -> Maybe Text
  -- ^ description (Nothing → empty string on the wire)
  -> NodeAdminState
  -> Bool
  -- ^ netd-disabled
  -> IO Int64
rpcNodeAdd conn name host nodeAgentPort netAgentPort basePath mDesc adminSt netdDisabled = do
  CGCorvus.Daemon'nodes'results {CGCorvus.mgr = mgr} <-
    callOn #nodes CGCorvus.Daemon'nodes'params (ccDaemon conn)
  let inner =
        CGNode.NodeAddParams
          { CGNode.name = name
          , CGNode.host = host
          , CGNode.nodeAgentPort = fromIntegral nodeAgentPort
          , CGNode.netAgentPort = fromIntegral netAgentPort
          , CGNode.basePath = basePath
          , CGNode.description = Data.Maybe.fromMaybe "" mDesc
          , CGNode.adminState = toCapnpNodeAdminState adminSt
          , CGNode.netdDisabled = netdDisabled
          }
  CGNode.NodeManager'create'results {CGNode.node = nClient} <-
    callOn #create CGNode.NodeManager'create'params {CGNode.params = inner} mgr
  CGNode.Node'show'results {CGNode.details = det} <-
    callOn #show CGNode.Node'show'params nClient
  case det of CGNode.NodeDetails {CGNode.id = nid} -> pure nid

rpcNodeEdit
  :: CapnpConnection
  -> EntityRef
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> Maybe (Maybe Text)
  -- ^ description: 'Nothing' = leave; 'Just Nothing' = clear; 'Just (Just t)' = set.
  -> Maybe NodeAdminState
  -> Maybe Bool
  -- ^ netd-disabled: Nothing = leave unchanged.
  -> IO ()
rpcNodeEdit conn ref mName mHost mNodeAgentPort mNetAgentPort mBasePath mDesc mAdminSt mNetdDisabled = do
  nClient <- getNodeClient conn ref
  let (hasDesc, descText) = case mDesc of
        Nothing -> (False, "")
        Just Nothing -> (True, "") -- empty wire text → clear
        Just (Just t) -> (True, t)
      p =
        CGNode.NodeEditParams
          { CGNode.hasName = Data.Maybe.isJust mName
          , CGNode.name = Data.Maybe.fromMaybe "" mName
          , CGNode.hasHost = Data.Maybe.isJust mHost
          , CGNode.host = Data.Maybe.fromMaybe "" mHost
          , CGNode.hasNodeAgentPort = Data.Maybe.isJust mNodeAgentPort
          , CGNode.nodeAgentPort = maybe 0 fromIntegral mNodeAgentPort
          , CGNode.hasNetAgentPort = Data.Maybe.isJust mNetAgentPort
          , CGNode.netAgentPort = maybe 0 fromIntegral mNetAgentPort
          , CGNode.hasBasePath = Data.Maybe.isJust mBasePath
          , CGNode.basePath = Data.Maybe.fromMaybe "" mBasePath
          , CGNode.hasDescription = hasDesc
          , CGNode.description = descText
          , CGNode.hasAdminState = Data.Maybe.isJust mAdminSt
          , -- Schema default = 'online'; encoder still needs a
            -- legal value when 'hasAdminState' is False.
            CGNode.adminState = maybe (toCapnpNodeAdminState NodeOnline) toCapnpNodeAdminState mAdminSt
          , CGNode.hasNetdDisabled = Data.Maybe.isJust mNetdDisabled
          , CGNode.netdDisabled = Data.Maybe.fromMaybe False mNetdDisabled
          }
  _ <- callOn #edit CGNode.Node'edit'params {CGNode.params = p} nClient
  pure ()

rpcNodeDrain :: CapnpConnection -> EntityRef -> IO ()
rpcNodeDrain conn ref = do
  nClient <- getNodeClient conn ref
  _ <- callOn #drain CGNode.Node'drain'params nClient
  pure ()

rpcNodeDelete :: CapnpConnection -> EntityRef -> IO ()
rpcNodeDelete conn ref = do
  nClient <- getNodeClient conn ref
  _ <- callOn #delete CGNode.Node'delete'params nClient
  pure ()

getNodeClient :: CapnpConnection -> EntityRef -> IO (C.Client CGNode.Node)
getNodeClient conn ref = do
  CGCorvus.Daemon'nodes'results {CGCorvus.mgr = mgr} <-
    callOn #nodes CGCorvus.Daemon'nodes'params (ccDaemon conn)
  CGNode.NodeManager'get'results {CGNode.node = nClient} <-
    callOn #get CGNode.NodeManager'get'params {CGNode.ref = toCapnpEntityRef ref} mgr
  pure nClient
