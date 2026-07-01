{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Action type class and runners for unified handler dispatch with task recording.
-- Each mutating operation is a data type that implements the Action class,
-- carrying its own metadata (subsystem, command, entity info) and logic.
-- Generic runners (runAction, runActionAsync, etc.) handle task recording.
module Corvus.Action
  ( -- * Context
    ActionContext (..)
  , mkActionContext

    -- * Cancellation
  , TaskCancelledException (..)
  , throwIfCancelled
  , isCancelled

    -- * Progress
  , pushTaskProgress

    -- * Type class
  , Action (..)

    -- * Runners
  , runAction
  , runActionAsync
  , runActionAsyncWithId
  , runActionAsSubtask
  , runAndFinalize
  , createTaskRecord

    -- * Helpers
  , executeCreate
  , cancelRemainingSubtasks

    -- * Response classification (used by runners)
  , classifyResponse
  , extractEntityFromResponse
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Streams as CGS
import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Exception (Exception, SomeException, fromException, throwIO, try)
import Control.Monad (when)
import qualified Control.Monad.Catch as MC
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Protocol.Apply (ApplySink, silentApplySink)
import Corvus.Rpc.Streams (callSink)
import Corvus.Types
import Corvus.Wire.Enums (toCapnpTaskResult)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (SqlBackend, fromSqlKey, runSqlPool, toSqlKey)

--------------------------------------------------------------------------------
-- Action Context
--------------------------------------------------------------------------------

-- | Context passed to every action execution.
-- Carries the server state, this action's own task ID (used as the
-- parent when spawning subtasks), the caller's name, and a sink
-- callback the apply pipeline uses to stream progress events.
--
-- @acClientName@ is the @<name>@ suffix of the mTLS CN, the literal
-- @"local"@ for Unix-socket / no-TLS callers, or @"system"@ for
-- daemon-internal work. Subtasks inherit the parent's
-- 'acClientName' (and 'acApplySink') so the audit trail and event
-- stream are consistent across composite operations.
--
-- @acApplySink@ is 'silentApplySink' for callers that aren't
-- streaming. When set, @executeApply@ pushes per-phase / per-entity
-- events through it, and disk-import downloads forward byte-counted
-- progress (see "Corvus.Handlers.Disk.Import").
data ActionContext = ActionContext
  { acState :: !ServerState
  , acTaskId :: !TaskId
  , acClientName :: !Text
  , acApplySink :: !ApplySink
  , acRootTaskId :: !TaskId
  -- ^ The top-level task of this action's tree. Set once when the
  -- root context is built and preserved across subtask contexts
  -- (which only swap 'acTaskId'). 'isCancelled' / 'throwIfCancelled'
  -- look the cancellation token up by this id, so cancelling the
  -- root task is observed by the whole tree — including subtasks
  -- spawned through a fresh context (the build orchestrator's
  -- @mkActionContext state buildTaskId "system"@ pattern), since
  -- that context's root id resolves back to the build task.
  }

-- | Build an 'ActionContext' for callers that don't stream apply
-- progress. The cancellation root is the given task id; subtasks
-- inherit it via the context clone in 'runActionAsSubtask'.
mkActionContext :: ServerState -> TaskId -> Text -> ActionContext
mkActionContext state taskId clientName =
  ActionContext
    { acState = state
    , acTaskId = taskId
    , acClientName = clientName
    , acApplySink = silentApplySink
    , acRootTaskId = taskId
    }

-- | Exception used to interrupt a cancelled task — thrown by
-- cooperative checkpoints ('throwIfCancelled') and by the hard
-- 'TaskManager.cancel' @throwTo@. 'runAndFinalizeResult' catches
-- it and finalises the task as 'TaskCancelled'.
data TaskCancelledException = TaskCancelledException
  deriving (Show)

instance Exception TaskCancelledException

-- | Has cancellation been requested for this action's task tree?
-- Resolves the token from the registry by the root task id, so it
-- is correct for subtasks too.
isCancelled :: ActionContext -> IO Bool
isCancelled ctx = do
  mTok <- lookupTaskCancelToken (acState ctx) (fromSqlKey (acRootTaskId ctx))
  maybe (pure False) readTVarIO mTok

-- | Cooperative cancellation checkpoint: throw
-- 'TaskCancelledException' if cancellation has been requested.
-- Call at loop / step boundaries in long orchestrators so a
-- cancel actually stops launching further work.
throwIfCancelled :: ActionContext -> IO ()
throwIfCancelled ctx = do
  c <- isCancelled ctx
  when c $ throwIO TaskCancelledException

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
runAction :: (Action a) => ServerState -> Text -> a -> IO Response
runAction state clientName action = do
  mErr <- actionValidate state action
  case mErr of
    Just errResp -> pure errResp
    Nothing -> do
      taskKey <- createTaskRecord state clientName action Nothing
      _ <- newTaskCancelToken state (fromSqlKey taskKey)
      tid <- myThreadId
      registerTaskThread state (fromSqlKey taskKey) tid
      let ctx = mkActionContext state taskKey clientName
      runAndFinalize state ctx action

-- | Run an action asynchronously.
-- Validates synchronously, forks execution, returns interim response.
runActionAsync :: (Action a) => ServerState -> Text -> a -> Response -> IO Response
runActionAsync state clientName action interimResponse = do
  mErr <- actionValidate state action
  case mErr of
    Just errResp -> pure errResp
    Nothing -> do
      taskKey <- createTaskRecord state clientName action Nothing
      _ <- newTaskCancelToken state (fromSqlKey taskKey)
      _ <- forkIO $ do
        myThreadId >>= registerTaskThread state (fromSqlKey taskKey)
        let ctx = mkActionContext state taskKey clientName
        runAndFinalize_ state ctx action
      pure interimResponse

-- | Run an action asynchronously, passing the new task ID to a function that
-- produces the interim response. Used by Apply which returns RespApplyStarted taskId.
runActionAsyncWithId :: (Action a) => ServerState -> Text -> a -> (Int64 -> Response) -> IO Response
runActionAsyncWithId state clientName action mkInterimResponse = do
  mErr <- actionValidate state action
  case mErr of
    Just errResp -> pure errResp
    Nothing -> do
      taskKey <- createTaskRecord state clientName action Nothing
      _ <- newTaskCancelToken state (fromSqlKey taskKey)
      _ <- forkIO $ do
        myThreadId >>= registerTaskThread state (fromSqlKey taskKey)
        let ctx = mkActionContext state taskKey clientName
        runAndFinalize_ state ctx action
      pure $ mkInterimResponse (fromSqlKey taskKey)

-- | Run an action as a subtask of a parent task.
-- On failure, cancels remaining sibling subtasks. The subtask
-- inherits the parent's 'acClientName' so the audit trail is
-- consistent across composite operations.
runActionAsSubtask :: (Action a) => ActionContext -> a -> IO Response
runActionAsSubtask parentCtx action = do
  let state = acState parentCtx
      parentId = acTaskId parentCtx
      clientName = acClientName parentCtx
  mErr <- actionValidate state action
  case mErr of
    Just errResp -> do
      -- Record the failed validation as a subtask
      taskKey <- createTaskRecord state clientName action (Just parentId)
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
      taskKey <- createTaskRecord state clientName action (Just parentId)
      let ctx = parentCtx {acTaskId = taskKey}
      result <- runAndFinalizeResult state ctx action
      case result of
        Left errResp -> do
          cancelRemainingSubtasks (ssDbPool state) parentId
          pure errResp
        Right resp -> do
          -- Also cancel siblings if the action returned an error response
          let (taskResult, _) = classifyResponse resp
          case taskResult of
            TaskError -> cancelRemainingSubtasks (ssDbPool state) parentId
            _ -> pure ()
          pure resp

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Create a task record in the database.
--
-- 'clientName' is the caller's identity as recorded on the row: the
-- @<name>@ suffix of the connected client's mTLS CN, the literal
-- @"local"@ for Unix-socket / no-TLS callers, or @"system"@ for
-- daemon-internal work.
createTaskRecord :: (Action a) => ServerState -> Text -> a -> Maybe TaskId -> IO TaskId
createTaskRecord state clientName action mParent = do
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
          , taskClientName = clientName
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
  -- Was cancellation requested for this task tree? Read BEFORE
  -- unregistering, since the top-level task's token is dropped by
  -- 'unregisterTask' below.
  requested <- isCancelled ctx
  -- Drop this task's cancel-token / thread entry. A no-op for
  -- subtasks (their ids were never registered — they share the
  -- parent's token), so it cleanly tears down only top-level tasks.
  unregisterTask state (fromSqlKey taskKey)
  -- Treat the task as cancelled if it was interrupted by a
  -- 'TaskCancelledException' (cooperative checkpoint or hard
  -- throwTo) OR if cancellation was requested while it ran — so
  -- every task in the tree that finishes after a cancel records
  -- consistently.
  let hitCancel = requested || either isCancellation (const False) result
  if hitCancel
    then do
      let msg = "Cancelled by operator"
      runSqlPool
        ( update
            taskKey
            [ TaskFinishedAt =. Just finishTime
            , TaskResult =. TaskCancelled
            , TaskMessage =. Just msg
            ]
        )
        pool
      pushTaskFinished state (fromSqlKey taskKey) TaskCancelled (Just msg)
      pure $ Left (RespError msg)
    else case result of
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
        pushTaskFinished state (fromSqlKey taskKey) taskResult message
        pure $ Right response
      Left (err :: SomeException) -> do
        let errMsg = T.pack (show err)
        runSqlPool
          ( update
              taskKey
              [ TaskFinishedAt =. Just finishTime
              , TaskResult =. TaskError
              , TaskMessage =. Just errMsg
              ]
          )
          pool
        pushTaskFinished state (fromSqlKey taskKey) TaskError (Just errMsg)
        let errResp = RespError $ "Internal error: " <> errMsg
        pure $ Left errResp

-- | Did this exception originate from a task cancellation?
isCancellation :: SomeException -> Bool
isCancellation e =
  Data.Maybe.isJust (fromException e :: Maybe TaskCancelledException)

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
-- The parent's 'ActionContext' supplies 'acClientName' so the inner work shares
-- the audit identity of the outer action.
executeCreate :: (Action a) => ActionContext -> a -> TaskId -> IO (Either Text Int64)
executeCreate parentCtx action taskId = do
  let ctx = parentCtx {acTaskId = taskId}
  result <- try $ actionExecute ctx action
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

-- | Cancel all remaining NotStarted subtasks under a parent.
-- Used by runActionAsSubtask when a sibling fails.
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
  RespTemplateUpdated tid -> (TaskSuccess, Just $ "Template ID " <> T.pack (show tid))
  RespTemplateDeleted -> (TaskSuccess, Just "Deleted")
  RespTemplateInstantiated vid -> (TaskSuccess, Just $ "VM ID " <> T.pack (show vid))
  RespNetworkCreated nid -> (TaskSuccess, Just $ "Network ID " <> T.pack (show nid))
  RespNetworkDeleted -> (TaskSuccess, Just "Deleted")
  RespNetworkEdited -> (TaskSuccess, Just "Edited")
  RespNetworkStarted -> (TaskSuccess, Just "Started")
  RespNetworkStopped -> (TaskSuccess, Just "Stopped")
  RespNetworkPeerAttached -> (TaskSuccess, Just "Peer attached")
  RespNetworkPeerDetached -> (TaskSuccess, Just "Peer detached")
  RespApplyResult _ -> (TaskSuccess, Nothing)
  RespApplyStarted tid -> (TaskSuccess, Just $ "Task ID " <> T.pack (show tid))
  -- A build pipeline's per-build errors are carried in the
  -- 'BuildResult' payload's @brBuilds[*].boError@ field; the
  -- task layer surfaces them as the task's result/message so
  -- `crv task show` reflects what actually happened. Without
  -- this, a build that failed mid-step still showed up as
  -- `result=success` because only the streaming sink saw the
  -- error (via the `PipelineEnd` event) and `--wait`'s polling
  -- reads from `task.result`.
  RespBuildResult br ->
    case firstBuildError br of
      Nothing -> (TaskSuccess, Nothing)
      Just msg -> (TaskError, Just msg)
  RespBuildStarted tid -> (TaskSuccess, Just $ "Task ID " <> T.pack (show tid))
  RespSharedDirAdded did -> (TaskSuccess, Just $ "Dir ID " <> T.pack (show did))
  RespSharedDirOk -> (TaskSuccess, Nothing)
  RespNetIfAdded nid -> (TaskSuccess, Just $ "NetIf ID " <> T.pack (show nid))
  RespGuestExecResult code _ _ -> (TaskSuccess, Just $ "Exit code " <> T.pack (show code))
  RespCloudInitOk -> (TaskSuccess, Nothing)
  RespStartupComplete -> (TaskSuccess, Just "Startup complete")
  RespShutdownComplete -> (TaskSuccess, Just "Shutdown complete")
  RespOk -> (TaskSuccess, Nothing)
  RespDiskImportStarted _ -> (TaskSuccess, Nothing)
  -- Read-only (shouldn't reach here, but handle gracefully)
  _ -> (TaskSuccess, Nothing)

-- | First per-build error in a pipeline result, if any. Returns the
-- error text from the leftmost 'BuildOne' whose 'boError' is 'Just',
-- so the task message points at the build that actually broke the
-- pipeline (subsequent builds get skipped on the first error per
-- 'runPipelineSteps' in "Corvus.Handlers.Build").
firstBuildError :: BuildResult -> Maybe Text
firstBuildError br =
  Data.Maybe.listToMaybe
    [msg | b <- brBuilds br, Just msg <- [boError b]]

-- | Extract entity ID and name from response (for create operations).
extractEntityFromResponse :: Response -> (Maybe Int, Maybe Text)
extractEntityFromResponse = \case
  RespVmCreated vid -> (Just (fromIntegral vid), Nothing)
  RespDiskCreated did -> (Just (fromIntegral did), Nothing)
  RespSnapshotCreated sid -> (Just (fromIntegral sid), Nothing)
  RespSshKeyCreated kid -> (Just (fromIntegral kid), Nothing)
  RespTemplateCreated tid -> (Just (fromIntegral tid), Nothing)
  RespTemplateUpdated tid -> (Just (fromIntegral tid), Nothing)
  RespNetworkCreated nid -> (Just (fromIntegral nid), Nothing)
  RespTemplateInstantiated vid -> (Just (fromIntegral vid), Nothing)
  RespDiskAttached did -> (Just (fromIntegral did), Nothing)
  _ -> (Nothing, Nothing)

--------------------------------------------------------------------------------
-- Task progress fan-out (Phase 6e)
--------------------------------------------------------------------------------

-- | Fan a 'TaskProgressEvent' out to every client subscribed to
-- this task id via @TaskManager.subscribe@. Dead sinks (cap dropped
-- client-side) are pruned. No-op when the subscriber list is empty,
-- which is the common case.
fanoutTaskEvent :: ServerState -> Int64 -> C.Parsed CGS.TaskProgressEvent -> IO ()
fanoutTaskEvent state tid ev = do
  subs <- readTVarIO (ssTaskProgressSubs state)
  let sinks = Map.findWithDefault [] tid subs
  case sinks of
    [] -> pure ()
    _ -> do
      let params = CGS.TaskProgressSink'push'params {CGS.event = ev}
      alive <- traverse (tryPush params) sinks
      atomically $
        modifyTVar' (ssTaskProgressSubs state) $
          Map.insert tid (map fst (filter snd (zip sinks alive)))
  where
    tryPush params sink = do
      r <- MC.try (callSink #push params sink) :: IO (Either SomeException ())
      pure $ case r of
        Right () -> True
        Left _ -> False

-- | Push a @finished@ 'TaskProgressEvent' to this task's subscribers.
pushTaskFinished :: ServerState -> Int64 -> M.TaskResult -> Maybe Text -> IO ()
pushTaskFinished state tid result mMsg =
  fanoutTaskEvent state tid $
    CGS.TaskProgressEvent
      { CGS.taskId = tid
      , CGS.union' =
          CGS.TaskProgressEvent'finished
            CGS.TaskProgressEvent'finished'
              { CGS.result = toCapnpTaskResult result
              , CGS.message = Data.Maybe.fromMaybe T.empty mMsg
              }
      }

-- | Report what a running task is currently waiting on. Records the
-- @label@ as the task's message (so polling clients — @crv task
-- list@ / @show@ / @wait@ — surface it without a live subscription)
-- AND emits a @progress@ 'TaskProgressEvent' for any live
-- subscribers. @completed@/@total@ are 0 when no count applies.
-- The finalizer overwrites the message with the terminal result.
pushTaskProgress :: ServerState -> Int64 -> Text -> (Int64, Int64) -> IO ()
pushTaskProgress state tid label (completed, total) = do
  runSqlPool
    (update (toSqlKey tid :: TaskId) [TaskMessage =. Just label])
    (ssDbPool state)
  fanoutTaskEvent state tid $
    CGS.TaskProgressEvent
      { CGS.taskId = tid
      , CGS.union' =
          CGS.TaskProgressEvent'progress
            CGS.TaskProgressEvent'progress'
              { CGS.completed = completed
              , CGS.total = total
              , CGS.label = label
              }
      }
