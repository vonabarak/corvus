{-# LANGUAGE OverloadedStrings #-}

-- | Reusable subtask infrastructure for multi-step operations.
-- Provides helpers to create, track, and manage subtasks linked to a parent task.
-- Used by the Apply handler and any other handler that performs multi-step operations
-- (VM start, network start/stop, template instantiation, etc.).
module Corvus.Handlers.Subtask
  ( -- * Subtask specification
    SubtaskSpec (..)

    -- * Subtask lifecycle
  , createSubtask
  , startSubtask
  , completeSubtask
  , cancelRemainingSubtasks

    -- * Execution helpers
  , runSubtask
  , withOptionalSubtask
  )
where

import Corvus.Model
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (SqlBackend, runSqlPool)

-- | A subtask specification before DB insertion.
data SubtaskSpec = SubtaskSpec
  { ssSubsystem :: !TaskSubsystem
  , ssCommand :: !Text
  , ssEntityName :: !(Maybe Text)
  }

-- | Create a subtask in NotStarted state linked to a parent task.
createSubtask :: Pool SqlBackend -> TaskId -> SubtaskSpec -> IO TaskId
createSubtask pool parentId spec = do
  now <- getCurrentTime
  runSqlPool
    ( insert
        Task
          { taskParent = Just parentId
          , taskStartedAt = now
          , taskFinishedAt = Nothing
          , taskSubsystem = ssSubsystem spec
          , taskEntityId = Nothing
          , taskEntityName = ssEntityName spec
          , taskCommand = ssCommand spec
          , taskResult = TaskNotStarted
          , taskMessage = Nothing
          }
    )
    pool

-- | Mark a subtask as running (update startedAt to now).
startSubtask :: Pool SqlBackend -> TaskId -> IO ()
startSubtask pool taskId = do
  now <- getCurrentTime
  runSqlPool
    ( update
        taskId
        [ TaskResult =. TaskRunning
        , TaskStartedAt =. now
        ]
    )
    pool

-- | Complete a subtask with result, optional message, and optional entity ID.
completeSubtask :: Pool SqlBackend -> TaskId -> TaskResult -> Maybe Text -> Maybe Int -> IO ()
completeSubtask pool taskId result message mEntityId = do
  now <- getCurrentTime
  runSqlPool
    ( update
        taskId
        ( [ TaskFinishedAt =. Just now
          , TaskResult =. result
          , TaskMessage =. message
          ]
            ++ maybe [] (\eid -> [TaskEntityId =. Just eid]) mEntityId
        )
    )
    pool

-- | Cancel all remaining NotStarted subtasks under a parent.
cancelRemainingSubtasks :: Pool SqlBackend -> TaskId -> IO ()
cancelRemainingSubtasks pool parentId = do
  now <- getCurrentTime
  runSqlPool
    ( updateWhere
        [TaskParent ==. Just parentId, TaskResult ==. TaskNotStarted]
        [ TaskResult =. TaskCancelled
        , TaskFinishedAt =. Just now
        , TaskMessage =. Just "Cancelled: prior step failed"
        ]
    )
    pool

-- | Run a subtask: mark running, execute action, mark success/error.
-- Returns the action result; on failure also cancels remaining subtasks.
runSubtask
  :: Pool SqlBackend
  -> TaskId
  -- ^ Parent task ID (for cancelling siblings on failure)
  -> TaskId
  -- ^ This subtask's ID
  -> IO (Either Text a)
  -- ^ Action to execute
  -> (a -> Maybe Int)
  -- ^ Extract entity ID from successful result
  -> IO (Either Text a)
runSubtask pool parentId subtaskId action extractId = do
  startSubtask pool subtaskId
  result <- action
  case result of
    Left err -> do
      completeSubtask pool subtaskId TaskError (Just err) Nothing
      cancelRemainingSubtasks pool parentId
      pure $ Left err
    Right val -> do
      completeSubtask pool subtaskId TaskSuccess Nothing (extractId val)
      pure $ Right val

-- | Run an action as a subtask if a parent task exists, otherwise run directly.
-- This is the \"standalone task OR subtask\" pattern: callers pass @Nothing@ when
-- the operation is standalone, or @Just parentId@ when it should be tracked as
-- a subtask of a larger operation.
withOptionalSubtask
  :: Pool SqlBackend
  -> Maybe TaskId
  -- ^ Parent task ID (Nothing = standalone, no subtask tracking)
  -> SubtaskSpec
  -- ^ Subtask description
  -> IO (Either Text a)
  -- ^ Action to execute
  -> (a -> Maybe Int)
  -- ^ Extract entity ID from successful result
  -> IO (Either Text a)
withOptionalSubtask _pool Nothing _spec action _extractId = action
withOptionalSubtask pool (Just parentId) spec action extractId = do
  subtaskId <- createSubtask pool parentId spec
  runSubtask pool parentId subtaskId action extractId
