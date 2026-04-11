{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Action type class and runners for unified handler dispatch with task recording.
-- Each mutating operation is a data type that implements the Action class,
-- carrying its own metadata (subsystem, command, entity info) and logic.
-- Generic runners (runAction, runActionAsync, etc.) handle task recording.
module Corvus.Action
  ( -- * Context
    ActionContext (..)

    -- * Type class
  , Action (..)

    -- * Runners
  , runAction
  , runActionAsync
  , runActionAsyncWithId
  , runActionAsSubtask
  , runActionWithSubtasks

    -- * Helpers
  , executeCreate

    -- * Response classification (used by runners)
  , classifyResponse
  , extractEntityFromResponse
  )
where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Corvus.Handlers.Subtask (cancelRemainingSubtasks)
import Corvus.Model
import Corvus.Protocol
import Corvus.Types
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (fromSqlKey)

--------------------------------------------------------------------------------
-- Action Context
--------------------------------------------------------------------------------

-- | Context passed to every action execution.
-- Contains the server state and this action's own task ID
-- (for creating subtasks in composite operations).
data ActionContext = ActionContext
  { acState :: !ServerState
  , acTaskId :: !TaskId
  }

--------------------------------------------------------------------------------
-- Action Type Class
--------------------------------------------------------------------------------

-- | Type class for self-describing, executable operations.
-- Each handler is a data type that implements this class.
class Action a where
  -- | Which subsystem this action belongs to (vm, disk, network, etc.)
  actionSubsystem :: a -> TaskSubsystem

  -- | Command name for task recording (e.g. "create", "start", "resize")
  actionCommand :: a -> Text

  -- | Entity ID if known before execution (e.g. for operations on existing entities)
  actionEntityId :: a -> Maybe Int
  actionEntityId _ = Nothing

  -- | Entity name if known before execution
  actionEntityName :: a -> Maybe Text
  actionEntityName _ = Nothing

  -- | Validate before execution. Returns Nothing if valid, Just errorResponse if not.
  -- Validation failures do not create a task record.
  actionValidate :: ServerState -> a -> IO (Maybe Response)
  actionValidate _ _ = pure Nothing

  -- | Execute the action. Receives context with server state and this action's task ID.
  actionExecute :: ActionContext -> a -> IO Response

--------------------------------------------------------------------------------
-- Runners
--------------------------------------------------------------------------------

-- | Run an action as a top-level task (synchronous).
-- Validates first (no task record on failure), creates task, executes, updates task.
runAction :: (Action a) => ServerState -> a -> IO Response
runAction state action = do
  mErr <- actionValidate state action
  case mErr of
    Just errResp -> pure errResp
    Nothing -> do
      taskKey <- createTaskRecord state action Nothing
      let ctx = ActionContext state taskKey
      runAndFinalize state ctx action

-- | Run an action asynchronously.
-- Validates synchronously, forks execution, returns interim response.
runActionAsync :: (Action a) => ServerState -> a -> Response -> IO Response
runActionAsync state action interimResponse = do
  mErr <- actionValidate state action
  case mErr of
    Just errResp -> pure errResp
    Nothing -> do
      taskKey <- createTaskRecord state action Nothing
      let ctx = ActionContext state taskKey
      _ <- forkIO $ runAndFinalize_ state ctx action
      pure interimResponse

-- | Run an action asynchronously, passing the new task ID to a function that
-- produces the interim response. Used by Apply which returns RespApplyStarted taskId.
runActionAsyncWithId :: (Action a) => ServerState -> a -> (Int64 -> Response) -> IO Response
runActionAsyncWithId state action mkInterimResponse = do
  mErr <- actionValidate state action
  case mErr of
    Just errResp -> pure errResp
    Nothing -> do
      taskKey <- createTaskRecord state action Nothing
      let ctx = ActionContext state taskKey
      _ <- forkIO $ runAndFinalize_ state ctx action
      pure $ mkInterimResponse (fromSqlKey taskKey)

-- | Run an action as a subtask of a parent task.
-- On failure, cancels remaining sibling subtasks.
runActionAsSubtask :: (Action a) => ServerState -> a -> TaskId -> IO Response
runActionAsSubtask state action parentId = do
  mErr <- actionValidate state action
  case mErr of
    Just errResp -> do
      -- Record the failed validation as a subtask
      taskKey <- createTaskRecord state action (Just parentId)
      now <- getCurrentTime
      let (taskResult, message) = classifyResponse errResp
      runSqlPool
        ( update
            taskKey
            [ TaskFinishedAt =. Just now
            , TaskResult =. taskResult
            , TaskMessage =. message
            ]
        )
        (ssDbPool state)
      cancelRemainingSubtasks (ssDbPool state) parentId
      pure errResp
    Nothing -> do
      taskKey <- createTaskRecord state action (Just parentId)
      let ctx = ActionContext state taskKey
      result <- runAndFinalizeResult state ctx action
      case result of
        Left errResp -> do
          cancelRemainingSubtasks (ssDbPool state) parentId
          pure errResp
        Right resp -> pure resp

-- | Run an action as a parent task that may create subtasks.
-- Like runAction but the action receives its own TaskId via ActionContext
-- so it can create subtasks. Same behavior as runAction — the distinction
-- is semantic (callers that need subtask support use this name for clarity).
runActionWithSubtasks :: (Action a) => ServerState -> a -> IO Response
runActionWithSubtasks = runAction

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Create a task record in the database.
createTaskRecord :: (Action a) => ServerState -> a -> Maybe TaskId -> IO TaskId
createTaskRecord state action mParent = do
  now <- getCurrentTime
  runSqlPool
    ( insert
        Task
          { taskParent = mParent
          , taskStartedAt = now
          , taskFinishedAt = Nothing
          , taskSubsystem = actionSubsystem action
          , taskEntityId = actionEntityId action
          , taskEntityName = actionEntityName action
          , taskCommand = actionCommand action
          , taskResult = TaskRunning
          , taskMessage = Nothing
          }
    )
    (ssDbPool state)

-- | Execute an action and update its task record. Returns the response.
runAndFinalize :: (Action a) => ServerState -> ActionContext -> a -> IO Response
runAndFinalize state ctx action = do
  result <- runAndFinalizeResult state ctx action
  case result of
    Left errResp -> pure errResp
    Right resp -> pure resp

-- | Execute an action and update its task record.
-- Returns Left on exception, Right on normal completion.
runAndFinalizeResult :: (Action a) => ServerState -> ActionContext -> a -> IO (Either Response Response)
runAndFinalizeResult state ctx action = do
  let taskKey = acTaskId ctx
      pool = ssDbPool state
  result <- try (actionExecute ctx action)
  finishTime <- getCurrentTime
  case result of
    Right response -> do
      let (taskResult, message) = classifyResponse response
          (mId, mName) = extractEntityFromResponse response
      runSqlPool
        ( update
            taskKey
            [ TaskFinishedAt =. Just finishTime
            , TaskResult =. taskResult
            , TaskMessage =. message
            , TaskEntityId =. (mId `orElse` actionEntityId action)
            , TaskEntityName =. (mName `orElse` actionEntityName action)
            ]
        )
        pool
      pure $ Right response
    Left (err :: SomeException) -> do
      runSqlPool
        ( update
            taskKey
            [ TaskFinishedAt =. Just finishTime
            , TaskResult =. TaskError
            , TaskMessage =. Just (T.pack $ show err)
            ]
        )
        pool
      let errResp = RespError $ "Internal error: " <> T.pack (show err)
      pure $ Left errResp

-- | Like runAndFinalize but discards the return value (for forkIO).
runAndFinalize_ :: (Action a) => ServerState -> ActionContext -> a -> IO ()
runAndFinalize_ state ctx action = do
  _ <- runAndFinalizeResult state ctx action
  pure ()

-- | Left-biased Maybe combiner.
orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing b = b

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Execute an action and extract the created entity ID.
-- Returns Left on error (including exceptions), Right with entity ID on success.
-- Useful for orchestrators (Apply, Template) that manage their own subtask lifecycle
-- and need to call action logic without going through the full runAction runner.
executeCreate :: (Action a) => ServerState -> a -> TaskId -> IO (Either Text Int64)
executeCreate state action taskId = do
  result <- try $ actionExecute (ActionContext state taskId) action
  case result of
    Left (err :: SomeException) -> pure $ Left $ T.pack $ show err
    Right resp -> do
      let (taskResult, msg) = classifyResponse resp
          (mId, _) = extractEntityFromResponse resp
      case taskResult of
        TaskSuccess -> case mId of
          Just eid -> pure $ Right (fromIntegral eid)
          Nothing -> pure $ Left "Action succeeded but no entity ID returned"
        _ -> pure $ Left $ fromMaybe "Unknown error" msg
  where
    fromMaybe :: a -> Maybe a -> a
    fromMaybe def Nothing = def
    fromMaybe _ (Just x) = x

--------------------------------------------------------------------------------
-- Response Classification
--------------------------------------------------------------------------------

-- | Classify a response into task result and optional message.
classifyResponse :: Response -> (TaskResult, Maybe Text)
classifyResponse = \case
  -- Errors
  RespError msg -> (TaskError, Just msg)
  RespVmNotFound -> (TaskError, Just "VM not found")
  RespDiskNotFound -> (TaskError, Just "Disk not found")
  RespDriveNotFound -> (TaskError, Just "Drive not found")
  RespSnapshotNotFound -> (TaskError, Just "Snapshot not found")
  RespNetworkNotFound -> (TaskError, Just "Network not found")
  RespSshKeyNotFound -> (TaskError, Just "SSH key not found")
  RespTemplateNotFound -> (TaskError, Just "Template not found")
  RespSharedDirNotFound -> (TaskError, Just "Shared directory not found")
  RespVmMustBeStopped -> (TaskError, Just "VM must be stopped")
  RespNetworkInUse -> (TaskError, Just "Network is in use")
  RespNetworkError msg -> (TaskError, Just msg)
  RespGuestAgentNotEnabled -> (TaskError, Just "Guest agent not enabled")
  RespGuestAgentError msg -> (TaskError, Just msg)
  RespInvalidTransition _ msg -> (TaskError, Just msg)
  RespFormatNotSupported msg -> (TaskError, Just msg)
  RespTaskNotFound -> (TaskError, Just "Task not found")
  -- Successes
  RespVmCreated vid -> (TaskSuccess, Just $ "Created with ID " <> T.pack (show vid))
  RespVmDeleted -> (TaskSuccess, Just "Deleted")
  RespVmEdited -> (TaskSuccess, Just "Edited")
  RespVmStateChanged s -> (TaskSuccess, Just $ "State: " <> enumToText s)
  RespVmRunning -> (TaskSuccess, Just "Already running")
  RespDiskCreated did -> (TaskSuccess, Just $ "Created with ID " <> T.pack (show did))
  RespDiskOk -> (TaskSuccess, Nothing)
  RespDiskAttached did -> (TaskSuccess, Just $ "Drive ID " <> T.pack (show did))
  RespSnapshotCreated sid -> (TaskSuccess, Just $ "Snapshot ID " <> T.pack (show sid))
  RespSnapshotOk -> (TaskSuccess, Nothing)
  RespSshKeyCreated kid -> (TaskSuccess, Just $ "Key ID " <> T.pack (show kid))
  RespSshKeyOk -> (TaskSuccess, Nothing)
  RespTemplateCreated tid -> (TaskSuccess, Just $ "Template ID " <> T.pack (show tid))
  RespTemplateDeleted -> (TaskSuccess, Just "Deleted")
  RespTemplateInstantiated vid -> (TaskSuccess, Just $ "VM ID " <> T.pack (show vid))
  RespNetworkCreated nid -> (TaskSuccess, Just $ "Network ID " <> T.pack (show nid))
  RespNetworkDeleted -> (TaskSuccess, Just "Deleted")
  RespNetworkEdited -> (TaskSuccess, Just "Edited")
  RespNetworkStarted -> (TaskSuccess, Just "Started")
  RespNetworkStopped -> (TaskSuccess, Just "Stopped")
  RespApplyResult _ -> (TaskSuccess, Nothing)
  RespApplyStarted tid -> (TaskSuccess, Just $ "Task ID " <> T.pack (show tid))
  RespSharedDirAdded did -> (TaskSuccess, Just $ "Dir ID " <> T.pack (show did))
  RespSharedDirOk -> (TaskSuccess, Nothing)
  RespNetIfAdded nid -> (TaskSuccess, Just $ "NetIf ID " <> T.pack (show nid))
  RespNetIfOk -> (TaskSuccess, Nothing)
  RespGuestExecResult code _ _ -> (TaskSuccess, Just $ "Exit code " <> T.pack (show code))
  RespCloudInitOk -> (TaskSuccess, Nothing)
  RespStartupComplete -> (TaskSuccess, Just "Startup complete")
  RespShutdownComplete -> (TaskSuccess, Just "Shutdown complete")
  -- Read-only (shouldn't reach here, but handle gracefully)
  _ -> (TaskSuccess, Nothing)

-- | Extract entity ID and name from response (for create operations).
extractEntityFromResponse :: Response -> (Maybe Int, Maybe Text)
extractEntityFromResponse = \case
  RespVmCreated vid -> (Just (fromIntegral vid), Nothing)
  RespDiskCreated did -> (Just (fromIntegral did), Nothing)
  RespSnapshotCreated sid -> (Just (fromIntegral sid), Nothing)
  RespSshKeyCreated kid -> (Just (fromIntegral kid), Nothing)
  RespTemplateCreated tid -> (Just (fromIntegral tid), Nothing)
  RespNetworkCreated nid -> (Just (fromIntegral nid), Nothing)
  RespTemplateInstantiated vid -> (Just (fromIntegral vid), Nothing)
  RespDiskAttached did -> (Just (fromIntegral did), Nothing)
  _ -> (Nothing, Nothing)
