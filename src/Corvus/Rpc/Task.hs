{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | TaskManager + Task cap implementations.
module Corvus.Rpc.Task
  ( TaskManagerCap (..)
  , TaskCap (..)
  , newTaskManagerCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Task as CGT
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer, handleParsed, methodUnimplemented)
import Corvus.Handlers (handleTaskList, handleTaskShow)
import Corvus.Protocol (Response (..))
import Corvus.Types (ServerState (..))
import Corvus.Wire.Enums (fromCapnpTaskResult, fromCapnpTaskSubsystem)
import Corvus.Wire.Errors (showWireError)
import Corvus.Wire.Task (toCapnpTaskInfo)
import Data.Int (Int64)
import Supervisors (Supervisor)

data TaskManagerCap = TaskManagerCap
  { tkState :: !ServerState
  , tkSup :: !Supervisor
  }

newTaskManagerCap :: ServerState -> Supervisor -> IO TaskManagerCap
newTaskManagerCap st sup = pure (TaskManagerCap st sup)

instance SomeServer TaskManagerCap

instance CGT.TaskManager'server_ TaskManagerCap where
  taskManager'list (TaskManagerCap st _) = handleParsed $ \CGT.TaskManager'list'params {params = CGT.TaskListParams {..}} -> do
    let lim = if limit == 0 then 100 else fromIntegral limit
    mSub <-
      if hasSubsystem
        then case fromCapnpTaskSubsystem subsystem of
          Right s -> pure (Just s)
          Left e -> throwFailed (showWireError e)
        else pure Nothing
    mRes <-
      if hasResult
        then case fromCapnpTaskResult result of
          Right r -> pure (Just r)
          Left e -> throwFailed (showWireError e)
        else pure Nothing
    resp <- handleTaskList st lim mSub mRes False
    case resp of
      RespTaskList tasks ->
        pure CGT.TaskManager'list'results {CGT.tasks = map toCapnpTaskInfo tasks}
      RespError msg -> throwFailed msg
      _ -> throwFailed "taskManager'list: unexpected response"

  taskManager'get (TaskManagerCap st sup) = handleParsed $ \CGT.TaskManager'get'params {..} -> do
    client <- export @CGT.Task sup (TaskCap st taskId)
    pure CGT.TaskManager'get'results {CGT.task = client}

  taskManager'listChildren _ = methodUnimplemented
  taskManager'subscribe _ = methodUnimplemented

data TaskCap = TaskCap
  { _tkState :: !ServerState
  , _tkId :: !Int64
  }

instance SomeServer TaskCap

instance CGT.Task'server_ TaskCap where
  task'show (TaskCap st tid) = handleParsed $ \_ -> do
    resp <- handleTaskShow st tid
    case resp of
      RespTaskInfo info ->
        pure CGT.Task'show'results {CGT.info = toCapnpTaskInfo info}
      RespTaskNotFound -> throwFailed "Task not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "task'show: unexpected response"
