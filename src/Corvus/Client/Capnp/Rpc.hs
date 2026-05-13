{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

    -- * Resource read methods
  , rpcVmList
  , rpcVmShow
  , rpcDiskList
  , rpcDiskShow
  , rpcNetworkList
  , rpcNetworkShow
  , rpcSshKeyList
  , rpcTemplateList
  , rpcTemplateShow
  , rpcTaskList
  , rpcTaskShow

    -- * VM lifecycle
  , rpcVmCreate
  , rpcVmStart
  , rpcVmStop
  , rpcVmPause
  , rpcVmReset
  , rpcVmDelete

    -- * Disk lifecycle
  , rpcDiskCreate
  , rpcDiskDelete
  , rpcDiskResize

    -- * Network lifecycle
  , rpcNetworkCreate
  , rpcNetworkStart
  , rpcNetworkStop
  , rpcNetworkDelete

    -- * SSH key lifecycle
  , rpcSshKeyCreate
  , rpcSshKeyDelete

    -- * Template lifecycle
  , rpcTemplateCreate
  , rpcTemplateDelete
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Cloudinit as CGCI
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Disk as CGDisk
import qualified Capnp.Gen.Enums as CGE
import qualified Capnp.Gen.Network as CGNet
import qualified Capnp.Gen.Sshkey as CGSsh
import qualified Capnp.Gen.Task as CGTask
import qualified Capnp.Gen.Template as CGTmpl
import qualified Capnp.Gen.Vm as CGVm
import Corvus.Client.Capnp.Connection (CapnpConnection (..))
import qualified Corvus.Protocol as P
import qualified Corvus.Protocol.Disk as PD
import qualified Corvus.Protocol.Network as PN
import qualified Corvus.Protocol.SshKey as PSk
import qualified Corvus.Protocol.Task as PT
import qualified Corvus.Protocol.Template as PTm
import qualified Corvus.Protocol.Vm as PV
import Corvus.Wire.Common (EntityRef, fromCapnpStatusInfo, toCapnpEntityRef)
import qualified Corvus.Wire.Disk as WDisk
import Corvus.Wire.Errors (WireError, showWireError)
import qualified Corvus.Wire.Network as WNet
import qualified Corvus.Wire.SshKey as WSsh
import qualified Corvus.Wire.Task as WTask
import qualified Corvus.Wire.Template as WTmpl
import qualified Corvus.Wire.Vm as WVm
import Data.Function ((&))
import Data.Int (Int64)
import qualified Data.Maybe
import Data.Text (Text)

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
  -> IO Int64
rpcVmCreate conn name cpus ram desc headless ga ci autostart = do
  CGCorvus.Daemon'vms'results {CGCorvus.mgr = mgr} <-
    callOn #vms CGCorvus.Daemon'vms'params (ccDaemon conn)
  let inner =
        CGVm.VmCreateParams
          { CGVm.name = name
          , CGVm.cpuCount = fromIntegral cpus
          , CGVm.ramMb = fromIntegral ram
          , CGVm.description = Data.Maybe.fromMaybe "" desc
          , CGVm.headless = headless
          , CGVm.guestAgent = ga
          , CGVm.cloudInit = ci
          , CGVm.autostart = autostart
          , CGVm.drives = []
          , CGVm.netIfs = []
          , CGVm.sshKeys = []
          , CGVm.cloudInitConfig = emptyCloudInitParsed
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

rpcVmDelete :: CapnpConnection -> EntityRef -> Bool -> IO ()
rpcVmDelete conn ref deleteDisks = do
  vmClient <- getVmClient conn ref
  _ <- callOn #delete CGVm.Vm'delete'params {CGVm.deleteDisks = deleteDisks} vmClient
  pure ()

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
  -> IO Int64
rpcDiskCreate conn name sizeMb fmt = do
  CGCorvus.Daemon'disks'results {CGCorvus.mgr = mgr} <-
    callOn #disks CGCorvus.Daemon'disks'params (ccDaemon conn)
  let inner = CGDisk.DiskCreateParams {CGDisk.name = name, CGDisk.sizeMb = sizeMb, CGDisk.format = fmt}
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
  -- ^ subnet (CIDR)
  -> Bool
  -- ^ dhcp
  -> Bool
  -- ^ nat
  -> Bool
  -- ^ autostart
  -> IO Int64
rpcNetworkCreate conn name subnet dhcp nat autostart = do
  CGCorvus.Daemon'networks'results {CGCorvus.mgr = mgr} <-
    callOn #networks CGCorvus.Daemon'networks'params (ccDaemon conn)
  let inner =
        CGNet.NetworkCreateParams
          { CGNet.name = name
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
-- Cloud-init payload helpers
-- =====================================================================

emptyCloudInitParsed :: C.Parsed CGCI.CloudInitInfo
emptyCloudInitParsed =
  CGCI.CloudInitInfo
    { CGCI.hasUserData = False
    , CGCI.userData = ""
    , CGCI.hasNetworkConfig = False
    , CGCI.networkConfig = ""
    , CGCI.injectSshKeys = False
    }
