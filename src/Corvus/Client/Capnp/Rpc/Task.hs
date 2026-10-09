{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Task-related RPC wrappers extracted from "Corvus.Client.Capnp.Rpc".
module Corvus.Client.Capnp.Rpc.Task
  ( -- * Task read methods
    rpcTaskList
  , rpcTaskShow
  , rpcTaskCancel
  , rpcTaskListChildren

    -- * Helpers
  , emptyCapnpEntityRef
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Common as CGCommon
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Task as CGTask
import Corvus.Client.Capnp.Connection (CapnpConnection (..))
import Corvus.Model (TaskResult, TaskSubsystem)
import qualified Corvus.Protocol.Task as PT
import Corvus.Wire.Enums (toCapnpTaskResult, toCapnpTaskSubsystem)
import Corvus.Wire.Errors (WireError, showWireError)
import qualified Corvus.Wire.Task as WTask
import Data.Function ((&))
import Data.Int (Int64)
import Data.Maybe (isJust)

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
-- Task read methods
-- ---------------------------------------------------------------------

rpcTaskList :: CapnpConnection -> Int -> Maybe TaskSubsystem -> Maybe TaskResult -> Bool -> IO [PT.TaskInfo]
rpcTaskList conn limit mSubsystem mResult includeSubtasks = do
  CGCorvus.Daemon'tasks'results {CGCorvus.mgr = mgr} <-
    callOn #tasks CGCorvus.Daemon'tasks'params (ccDaemon conn)
  let inner =
        CGTask.TaskListParams
          { CGTask.limit = fromIntegral limit
          , CGTask.subsystem = maybe (toEnum 0) toCapnpTaskSubsystem mSubsystem
          , CGTask.hasSubsystem = Data.Maybe.isJust mSubsystem
          , CGTask.entityId = 0
          , CGTask.result = maybe (toEnum 0) toCapnpTaskResult mResult
          , CGTask.hasResult = Data.Maybe.isJust mResult
          , CGTask.includeSubtasks = includeSubtasks
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

rpcTaskCancel :: CapnpConnection -> Int64 -> IO ()
rpcTaskCancel conn taskId = do
  CGCorvus.Daemon'tasks'results {CGCorvus.mgr = mgr} <-
    callOn #tasks CGCorvus.Daemon'tasks'params (ccDaemon conn)
  _ <-
    callOn #cancel CGTask.TaskManager'cancel'params {CGTask.taskId = taskId} mgr
  pure ()

rpcTaskListChildren :: CapnpConnection -> Int64 -> IO [PT.TaskInfo]
rpcTaskListChildren conn parentId = do
  CGCorvus.Daemon'tasks'results {CGCorvus.mgr = mgr} <-
    callOn #tasks CGCorvus.Daemon'tasks'params (ccDaemon conn)
  CGTask.TaskManager'listChildren'results {CGTask.tasks = ts} <-
    callOn #listChildren CGTask.TaskManager'listChildren'params {CGTask.parentId = parentId} mgr
  traverse (failOnWire . WTask.fromCapnpTaskInfo) ts

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

emptyCapnpEntityRef :: C.Parsed CGCommon.EntityRef
emptyCapnpEntityRef =
  CGCommon.EntityRef {CGCommon.union' = CGCommon.EntityRef'id 0}
