-- | Request handling module.
-- Re-exports handlers from submodules and provides the task history
-- read handlers (consumed by the Cap'n Proto Task cap).
--
-- The legacy 'Request'-sum dispatcher used to live here. With the
-- daemon now speaking Cap'n Proto, every method has its own typed
-- entry point in @Corvus.Rpc.*@ and there is no central dispatch
-- function any more.
module Corvus.Handlers
  ( -- * Task history (defined locally; exposed for the Rpc layer)
    handleTaskList
  , handleTaskShow
  , handleTaskListChildren

    -- * Re-exports from submodules
  , module Corvus.Handlers.Core
  , module Corvus.Handlers.Vm
  , module Corvus.Handlers.Disk
  , module Corvus.Handlers.SharedDir
  , module Corvus.Handlers.NetIf
  , module Corvus.Handlers.SshKey
  , module Corvus.Handlers.Template
  , module Corvus.Handlers.Network
  , module Corvus.Handlers.GuestExec
  , module Corvus.Handlers.Apply
  , module Corvus.Handlers.Build
  , module Corvus.Handlers.CloudInit
  )
where

import Corvus.Handlers.Apply
import Corvus.Handlers.Build
import Corvus.Handlers.CloudInit
import Corvus.Handlers.Core
import Corvus.Handlers.Disk
import Corvus.Handlers.GuestExec
import Corvus.Handlers.NetIf
import Corvus.Handlers.Network
import Corvus.Handlers.SharedDir
import Corvus.Handlers.SshKey
import Corvus.Handlers.Template
import Corvus.Handlers.Vm
import Corvus.Model
import Corvus.Protocol
import Corvus.Types
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

--------------------------------------------------------------------------------
-- Task History Handlers (read-only queries)
--------------------------------------------------------------------------------

-- | List task history entries.
handleTaskList :: ServerState -> Int -> Maybe TaskSubsystem -> Maybe TaskResult -> Bool -> IO Response
handleTaskList state limit mSub mResult includeSubtasks = do
  let parentFilter = [TaskParent ==. Nothing | not includeSubtasks]
  tasks <-
    runSqlPool
      ( selectList
          (parentFilter ++ catMaybes [subFilter, resultFilter])
          [Desc TaskStartedAt, LimitTo limit]
      )
      (ssDbPool state)
  pure $ RespTaskList $ map toTaskInfo tasks
  where
    subFilter = fmap (TaskSubsystem ==.) mSub
    resultFilter = fmap (TaskResult ==.) mResult

-- | Show a single task history entry
handleTaskShow :: ServerState -> Int64 -> IO Response
handleTaskShow state taskId = do
  mTask <- runSqlPool (get (toSqlKey taskId :: TaskId)) (ssDbPool state)
  case mTask of
    Nothing -> pure RespTaskNotFound
    Just th ->
      pure $
        RespTaskInfo $
          toTaskInfoWith taskId th

-- | List subtasks for a parent task
handleTaskListChildren :: ServerState -> Int64 -> IO Response
handleTaskListChildren state parentId = do
  tasks <-
    runSqlPool
      ( selectList
          [TaskParent ==. Just (toSqlKey parentId)]
          [Asc TaskId]
      )
      (ssDbPool state)
  pure $ RespTaskList $ map toTaskInfo tasks

toTaskInfo :: Entity Task -> TaskInfo
toTaskInfo (Entity key th) = toTaskInfoWith (fromSqlKey key) th

toTaskInfoWith :: Int64 -> Task -> TaskInfo
toTaskInfoWith tid th =
  TaskInfo
    { tiId = tid
    , tiParentId = fmap fromSqlKey (taskParent th)
    , tiStartedAt = taskStartedAt th
    , tiFinishedAt = taskFinishedAt th
    , tiSubsystem = taskSubsystem th
    , tiEntity = taskEntityRef th
    , tiCommand = taskCommand th
    , tiResult = taskResult th
    , tiMessage = taskMessage th
    , tiClientName = taskClientName th
    }
  where
    -- Build the entity 'NamedRef' from the two flat task columns.
    -- 'taskEntityId' stores the foreign-key id for the subject of
    -- the task (when applicable); 'taskEntityName' stores the
    -- subject's display name captured at task-creation time so it
    -- survives the entity later being renamed or deleted.
    taskEntityRef t = case taskEntityId t of
      Nothing -> Nothing
      Just eid ->
        Just NamedRef {nrId = fromIntegral eid, nrName = fromMaybe "" (taskEntityName t)}

-- | Helper to filter out Nothing values
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs
