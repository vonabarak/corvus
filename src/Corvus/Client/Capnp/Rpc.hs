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
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Disk as CGDisk
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
