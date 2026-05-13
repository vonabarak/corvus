{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Daemon bootstrap capability.
--
-- The 'DaemonCap' is the root cap returned to every newly-connected
-- client. Its methods either complete locally (ping / status /
-- shutdown) or return one of the subsystem manager caps, which then
-- handle further requests.
module Corvus.Rpc.Daemon
  ( DaemonCap (..)
  , newDaemonCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Cloudinit as CGCI
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Disk as CGDisk
import qualified Capnp.Gen.Network as CGNet
import qualified Capnp.Gen.Sshkey as CGSsh
import qualified Capnp.Gen.Task as CGTask
import qualified Capnp.Gen.Template as CGTmpl
import qualified Capnp.Gen.Vm as CGVm
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer, handleParsed, methodUnimplemented)
import Corvus.Handlers.Core (handlePing, handleShutdown, handleStatus)
import Corvus.Protocol (Response (..))
import Corvus.Rpc.CloudInit (newCloudInitManagerCap)
import Corvus.Rpc.Disk (newDiskManagerCap)
import Corvus.Rpc.Network (newNetworkManagerCap)
import Corvus.Rpc.SshKey (newSshKeyManagerCap)
import Corvus.Rpc.Task (newTaskManagerCap)
import Corvus.Rpc.Template (newTemplateManagerCap)
import Corvus.Rpc.Vm (newVmManagerCap)
import Corvus.Types (ServerState (..))
import Corvus.Wire.Common (toCapnpStatusInfo)
import Supervisors (Supervisor)

-- | The root Daemon cap, parameterised over the shared server state
-- and a supervisor for sub-cap export.
data DaemonCap = DaemonCap
  { dcState :: !ServerState
  , dcSup :: !Supervisor
  }

newDaemonCap :: ServerState -> Supervisor -> IO DaemonCap
newDaemonCap st sup = pure (DaemonCap st sup)

instance SomeServer DaemonCap

instance CGCorvus.Daemon'server_ DaemonCap where
  daemon'ping _ = handleParsed $ \_ -> do
    _ <- handlePing
    pure CGCorvus.Daemon'ping'results

  daemon'status (DaemonCap st _) = handleParsed $ \_ -> do
    resp <- handleStatus st
    case resp of
      RespStatus info ->
        pure CGCorvus.Daemon'status'results {CGCorvus.info = toCapnpStatusInfo info}
      _ -> throwFailed "daemon'status: unexpected response"

  daemon'shutdown (DaemonCap st _) = handleParsed $ \_ -> do
    _ <- handleShutdown st
    pure CGCorvus.Daemon'shutdown'results

  daemon'vms (DaemonCap st sup) = handleParsed $ \_ -> do
    impl <- newVmManagerCap st sup
    client <- export @CGVm.VmManager sup impl
    pure CGCorvus.Daemon'vms'results {CGCorvus.mgr = client}

  daemon'disks (DaemonCap st sup) = handleParsed $ \_ -> do
    impl <- newDiskManagerCap st sup
    client <- export @CGDisk.DiskManager sup impl
    pure CGCorvus.Daemon'disks'results {CGCorvus.mgr = client}

  daemon'networks (DaemonCap st sup) = handleParsed $ \_ -> do
    impl <- newNetworkManagerCap st sup
    client <- export @CGNet.NetworkManager sup impl
    pure CGCorvus.Daemon'networks'results {CGCorvus.mgr = client}

  daemon'sshKeys (DaemonCap st sup) = handleParsed $ \_ -> do
    impl <- newSshKeyManagerCap st sup
    client <- export @CGSsh.SshKeyManager sup impl
    pure CGCorvus.Daemon'sshKeys'results {CGCorvus.mgr = client}

  daemon'templates (DaemonCap st sup) = handleParsed $ \_ -> do
    impl <- newTemplateManagerCap st sup
    client <- export @CGTmpl.TemplateManager sup impl
    pure CGCorvus.Daemon'templates'results {CGCorvus.mgr = client}

  daemon'tasks (DaemonCap st sup) = handleParsed $ \_ -> do
    impl <- newTaskManagerCap st sup
    client <- export @CGTask.TaskManager sup impl
    pure CGCorvus.Daemon'tasks'results {CGCorvus.mgr = client}

  daemon'cloudInit (DaemonCap st sup) = handleParsed $ \_ -> do
    impl <- newCloudInitManagerCap st sup
    client <- export @CGCI.CloudInitManager sup impl
    pure CGCorvus.Daemon'cloudInit'results {CGCorvus.mgr = client}

  -- Apply and build are streaming/async operations that land in
  -- follow-up commits along with the Build event sink wiring.
  daemon'apply _ = methodUnimplemented
  daemon'build _ = methodUnimplemented
