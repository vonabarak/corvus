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
import Corvus.Action (runAction, runActionAsyncWithId)
import Corvus.Handlers.Apply (ApplyAction (..), handleApplyValidate)
import Corvus.Handlers.Core (handlePing, handleShutdown, handleStatus)
import Corvus.Protocol (Response (..))
import qualified Corvus.Protocol.Apply as PA
import Corvus.Rpc.CloudInit (newCloudInitManagerCap)
import Corvus.Rpc.Disk (newDiskManagerCap)
import Corvus.Rpc.Network (newNetworkManagerCap)
import Corvus.Rpc.SshKey (newSshKeyManagerCap)
import Corvus.Rpc.Task (newTaskManagerCap)
import Corvus.Rpc.Template (newTemplateManagerCap)
import Corvus.Rpc.Vm (newVmManagerCap)
import Corvus.Types (ServerState (..))
import Corvus.Wire.Apply (toCapnpApplyResult)
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

  -- Declarative apply. With @wait=True@ runs synchronously and
  -- returns the populated @result@; with @wait=False@ kicks off an
  -- async task and returns the parent @taskId@ with an empty
  -- @result@.
  daemon'apply (DaemonCap st _) =
    handleParsed $ \CGCorvus.Daemon'apply'params {CGCorvus.yaml = yamlText, CGCorvus.skipExisting = skipExisting, CGCorvus.wait = wait} -> do
      validated <- handleApplyValidate st yamlText
      case validated of
        Left (RespError msg) -> throwFailed msg
        Left _ -> throwFailed "apply: validation failed"
        Right cfg ->
          if wait
            then do
              resp <- runAction st (ApplyAction cfg skipExisting)
              case resp of
                RespApplyResult ar ->
                  pure
                    CGCorvus.Daemon'apply'results
                      { CGCorvus.result = toCapnpApplyResult ar
                      , CGCorvus.taskId = 0
                      }
                RespError msg -> throwFailed msg
                _ -> throwFailed "apply: unexpected response"
            else do
              resp <-
                runActionAsyncWithId
                  st
                  (ApplyAction cfg skipExisting)
                  RespApplyStarted
              case resp of
                RespApplyStarted tid ->
                  pure
                    CGCorvus.Daemon'apply'results
                      { CGCorvus.result = toCapnpApplyResult emptyApplyResult
                      , CGCorvus.taskId = tid
                      }
                RespError msg -> throwFailed msg
                _ -> throwFailed "apply (async): unexpected response"

  -- Build streaming over BuildEventSink is Phase 6; the schema
  -- method here is still unimplemented in Phase 5.
  daemon'build _ = methodUnimplemented

-- | Empty 'ApplyResult', used as the value of the @result@ field in
-- async apply replies (where the actual creation list is delivered
-- via the task subsystem rather than inline).
emptyApplyResult :: PA.ApplyResult
emptyApplyResult =
  PA.ApplyResult
    { PA.arSshKeys = []
    , PA.arDisks = []
    , PA.arNetworks = []
    , PA.arVms = []
    , PA.arTemplates = []
    }
