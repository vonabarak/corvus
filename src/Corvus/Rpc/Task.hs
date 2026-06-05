{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | TaskManager + Task cap implementations.
module Corvus.Rpc.Task
  ( TaskManagerCap (..)
  , TaskCap (..)
  , newTaskManagerCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Streams as CGS
import qualified Capnp.Gen.Task as CGT
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent (forkIO, throwTo)
import Control.Concurrent.STM (atomically, modifyTVar', writeTVar)
import Control.Monad (void)
import Corvus.Action (TaskCancelledException (..))
import Corvus.Handlers (handleTaskList, handleTaskListChildren, handleTaskShow)
import Corvus.Protocol (Response (..))
import Corvus.Rpc.Common (handleParsed)
import Corvus.Rpc.Streams (EmptyHandle (..))
import Corvus.Types (ServerState (..), lookupTaskCancelToken, lookupTaskThread)
import Corvus.Wire.Enums (fromCapnpTaskResult, fromCapnpTaskSubsystem)
import Corvus.Wire.Errors (showWireError)
import Corvus.Wire.Task (toCapnpTaskInfo)
import Data.Foldable (for_)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
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

  taskManager'listChildren (TaskManagerCap st _) =
    handleParsed $ \CGT.TaskManager'listChildren'params {..} -> do
      resp <- handleTaskListChildren st parentId
      case resp of
        RespTaskList tasks ->
          pure CGT.TaskManager'listChildren'results {CGT.tasks = map toCapnpTaskInfo tasks}
        RespError msg -> throwFailed msg
        _ -> throwFailed "taskManager'listChildren: unexpected response"

  -- Register a 'TaskProgressSink' against the given task id.
  -- The Action runtime pushes a @finished@ event when the task
  -- completes; subscribers added after the task has already
  -- finished will never receive an event.
  taskManager'subscribe (TaskManagerCap st sup) =
    handleParsed $ \CGT.TaskManager'subscribe'params {CGT.taskId = tid, CGT.sink = sinkClient} -> do
      atomically $
        modifyTVar' (ssTaskProgressSubs st) $
          Map.insertWith (++) tid [sinkClient]
      handle <- export @CGS.Handle sup EmptyHandle
      pure CGT.TaskManager'subscribe'results {CGT.handle = handle}

  -- Best-effort cancel: flip the cooperative token (honoured at
  -- subtask boundaries + long-loop checkpoints and by the
  -- finalizer's terminal-state classification) and, for a worker
  -- blocked in an in-flight RPC, hard-interrupt it with a
  -- 'TaskCancelledException'. The throwTo is forked so this RPC
  -- returns once the request is recorded, not once the task stops.
  -- A no-op for tasks that already finished (no registry entry).
  taskManager'cancel (TaskManagerCap st _) =
    handleParsed $ \CGT.TaskManager'cancel'params {CGT.taskId = tid} -> do
      mTok <- lookupTaskCancelToken st tid
      for_ mTok $ \tok -> atomically (writeTVar tok True)
      mTh <- lookupTaskThread st tid
      for_ mTh $ \th -> void $ forkIO $ throwTo th TaskCancelledException
      pure CGT.TaskManager'cancel'results

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
