{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
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
import qualified Capnp as C
import qualified Capnp.Gen.Cloudinit as CGCI
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Disk as CGDisk
import qualified Capnp.Gen.Network as CGNet
import qualified Capnp.Gen.Sshkey as CGSsh
import qualified Capnp.Gen.Streams as CGS
import qualified Capnp.Gen.Task as CGTask
import qualified Capnp.Gen.Template as CGTmpl
import qualified Capnp.Gen.Vm as CGVm
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Corvus.Action (classifyResponse, runAction, runActionAsyncWithId)
import Corvus.Handlers.Apply (ApplyAction (..), handleApplyValidate)
import Corvus.Handlers.Build (BuildSink, runBuildPipeline)
import Corvus.Handlers.Core (handlePing, handleShutdown, handleStatus)
import Corvus.Model
import Corvus.Protocol (Response (..))
import qualified Corvus.Protocol.Apply as PA
import qualified Corvus.Protocol.Build as PB
import Corvus.Rpc.CloudInit (newCloudInitManagerCap)
import Corvus.Rpc.Common (handleParsed)
import Corvus.Rpc.Disk (newDiskManagerCap)
import Corvus.Rpc.Network (newNetworkManagerCap)
import Corvus.Rpc.SshKey (newSshKeyManagerCap)
import Corvus.Rpc.Streams (callSink)
import Corvus.Rpc.Task (newTaskManagerCap)
import Corvus.Rpc.Template (newTemplateManagerCap)
import Corvus.Rpc.Vm (newVmManagerCap)
import Corvus.Types (ServerState (..))
import Corvus.Wire.Apply (toCapnpApplyResult)
import Corvus.Wire.Build (toCapnpBuildEvent)
import Corvus.Wire.Common (toCapnpStatusInfo)
import Data.Function ((&))
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist (insert, update, (=.))
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (fromSqlKey)
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

  -- Build streaming. The client passes a 'BuildEventSink' cap; we
  -- create the parent task synchronously, then kick the pipeline
  -- off on an async that pushes each 'BuildEvent' through
  -- @sink.push@ and terminates with @sink.end@.
  daemon'build (DaemonCap st _) =
    handleParsed $ \CGCorvus.Daemon'build'params {CGCorvus.yaml = yamlText, CGCorvus.sink = sinkClient} -> do
      startedAt <- getCurrentTime
      let pool = ssDbPool st
      taskKey <-
        runSqlPool
          ( insert
              Task
                { taskParent = Nothing
                , taskStartedAt = startedAt
                , taskFinishedAt = Nothing
                , taskSubsystem = SubBuild
                , taskEntityId = Nothing
                , taskEntityName = Nothing
                , taskCommand = "build"
                , taskResult = TaskRunning
                , taskMessage = Nothing
                }
          )
          pool
      let tid = fromSqlKey taskKey
          pushEvent ev = do
            let cev = toCapnpBuildEvent ev
                params = CGS.BuildEventSink'push'params {CGS.event = cev}
            _ <- try (callSink #push params sinkClient) :: IO (Either SomeException ())
            pure ()
          finalize ev = do
            pushEvent ev
            _ <-
              try (callSink #end CGS.BuildEventSink'end'params sinkClient)
                :: IO (Either SomeException ())
            pure ()
      void $ async $ do
        let sink :: BuildSink
            sink = pushEvent
        resp <- try (runBuildPipeline st taskKey sink yamlText) :: IO (Either SomeException Response)
        finishedAt <- getCurrentTime
        case resp of
          Right r -> do
            let (taskRes, taskMsg) = classifyResponse r
            runSqlPool
              ( update
                  taskKey
                  [ TaskFinishedAt =. Just finishedAt
                  , TaskResult =. taskRes
                  , TaskMessage =. taskMsg
                  ]
              )
              pool
            case r of
              RespBuildResult br -> finalize (PB.PipelineEnd br)
              RespError msg -> do
                pushEvent (PB.BuildLogLine ("build error: " <> msg))
                finalize (PB.PipelineEnd (PB.BuildResult []))
              _ -> finalize (PB.PipelineEnd (PB.BuildResult []))
          Left e -> do
            let txt = T.pack (show e)
            runSqlPool
              ( update
                  taskKey
                  [ TaskFinishedAt =. Just finishedAt
                  , TaskResult =. TaskError
                  , TaskMessage =. Just txt
                  ]
              )
              pool
            pushEvent (PB.BuildLogLine ("internal error: " <> txt))
            finalize (PB.PipelineEnd (PB.BuildResult []))
      pure CGCorvus.Daemon'build'results {CGCorvus.taskId = tid}

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
