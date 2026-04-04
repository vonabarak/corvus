{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Request handling module.
-- Re-exports handlers from submodules and provides the main dispatch function.
-- All mutating requests are wrapped with task history recording.
module Corvus.Handlers
  ( -- * Request handling
    handleRequest

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
  )
where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Corvus.Handlers.Apply
import Corvus.Handlers.Core
import Corvus.Handlers.Disk
import Corvus.Handlers.GuestExec
import Corvus.Handlers.NetIf
import Corvus.Handlers.Network
import Corvus.Handlers.Resolve
import Corvus.Handlers.SharedDir
import Corvus.Handlers.SshKey
import Corvus.Handlers.Template
import Corvus.Handlers.Vm
import Corvus.Model
import Corvus.Protocol
import Corvus.Types
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

--------------------------------------------------------------------------------
-- Request Dispatch
--------------------------------------------------------------------------------

-- | Handle a request and produce a response.
-- Mutating requests are recorded in the task history table.
handleRequest :: ServerState -> Request -> IO Response
handleRequest state req
  | isReadOnly req = dispatchRequest state req
  | isApply req = dispatchApply state req
  | needsParentTask req = withParentTask state req $ \parentId ->
      dispatchRequestWithParent state req parentId
  | otherwise = withTask state req (dispatchRequest state req)

isApply :: Request -> Bool
isApply (ReqApply {}) = True
isApply _ = False

-- | Requests that create subtasks need a parent task.
needsParentTask :: Request -> Bool
needsParentTask (ReqNetworkStart {}) = True
needsParentTask (ReqNetworkStop {}) = True
needsParentTask (ReqTemplateInstantiate {}) = True
needsParentTask _ = False

-- | Dispatch apply with sync/async support and subtask tracking.
dispatchApply :: ServerState -> Request -> IO Response
dispatchApply state req@(ReqApply yaml skipExisting wait) = do
  validated <- handleApplyValidate state yaml
  case validated of
    Left errResp -> pure errResp
    Right config ->
      if wait
        then withParentTask state req $ \parentId ->
          handleApplyExecute state config skipExisting parentId
        else
          withParentTaskAsync
            state
            req
            (handleApplyExecute state config skipExisting)
dispatchApply _ _ = error "dispatchApply: impossible"

-- | Dispatch a command that supports sync (--wait) and async modes.
-- Validates synchronously in both cases. In async mode, forks the
-- execute function in a background thread and returns the interim
-- response immediately; the task history stays "running" until the
-- background thread completes.
dispatchWithWait
  :: ServerState
  -> Request
  -> Bool
  -- ^ Wait flag (True = sync, False = async)
  -> IO (Either Response a)
  -- ^ Validate (sync). Left = error response, Right = validated data.
  -> (a -> IO Response)
  -- ^ Execute (may block for a long time). Takes validated data.
  -> Response
  -- ^ Interim response returned immediately in async mode.
  -> IO Response
dispatchWithWait state req wait validate execute interimResponse = do
  validated <- validate
  case validated of
    Left errResp -> pure errResp
    Right validData ->
      if wait
        then execute validData
        else withTaskAsync state req (execute validData) interimResponse

-- | Like dispatchWithWait but creates a parent task and passes TaskId to the
-- execute function, enabling subtask tracking within the handler.
dispatchWithWaitParent
  :: ServerState
  -> Request
  -> Bool
  -- ^ Wait flag (True = sync, False = async)
  -> IO (Either Response a)
  -- ^ Validate (sync). Left = error response, Right = validated data.
  -> (a -> TaskId -> IO Response)
  -- ^ Execute (may block). Takes validated data and parent TaskId.
  -> Response
  -- ^ Interim response returned immediately in async mode.
  -> IO Response
dispatchWithWaitParent state req wait validate execute interimResponse = do
  validated <- validate
  case validated of
    Left errResp -> pure errResp
    Right validData ->
      if wait
        then withParentTask state req $ \parentId ->
          execute validData parentId
        else
          withParentTaskAsyncInterim
            state
            req
            (execute validData)
            (Just interimResponse)

-- | Dispatch requests that need a parent TaskId for subtask tracking.
dispatchRequestWithParent :: ServerState -> Request -> TaskId -> IO Response
dispatchRequestWithParent state req parentId = case req of
  ReqNetworkStart nwRef -> withNetwork nwRef $ \nwId -> handleNetworkStart state nwId parentId
  ReqNetworkStop nwRef force -> withNetwork nwRef $ \nwId -> handleNetworkStop state nwId force parentId
  ReqTemplateInstantiate tRef name -> withTemplate tRef $ \tid -> handleTemplateInstantiate state tid name parentId
  _ -> error "dispatchRequestWithParent: unhandled request"
  where
    pool = ssDbPool state
    withNetwork ref f = resolveNetwork ref pool >>= either (const $ pure RespNetworkNotFound) f
    withTemplate ref f = resolveTemplate ref pool >>= either (const $ pure RespTemplateNotFound) f

-- | Dispatch a request to the appropriate handler
dispatchRequest :: ServerState -> Request -> IO Response
dispatchRequest state req = case req of
  -- Core handlers
  ReqPing -> handlePing
  ReqStatus -> handleStatus state
  ReqShutdown -> handleShutdown state
  -- VM handlers
  ReqListVms -> handleVmList state
  ReqShowVm vmRef -> withVm vmRef $ \vmId -> handleVmShow state vmId
  ReqVmCreate name cpus ram desc headless ga ci -> handleVmCreate state name cpus ram desc headless ga ci
  ReqVmDelete vmRef -> withVm vmRef $ \vmId -> handleVmDelete state vmId
  ReqVmStart vmRef wait -> withVm vmRef $ \vmId ->
    dispatchWithWaitParent
      state
      req
      wait
      (handleVmStartValidate state vmId)
      (\_ -> handleVmStartExecute state vmId)
      (RespVmStateChanged VmStarting)
  ReqVmStop vmRef wait -> withVm vmRef $ \vmId ->
    dispatchWithWait
      state
      req
      wait
      (handleVmStopValidate state vmId)
      (\_ -> handleVmStopExecute state vmId)
      (RespVmStateChanged VmStopping)
  ReqVmPause vmRef -> withVm vmRef $ \vmId -> handleVmPause state vmId
  ReqVmReset vmRef -> withVm vmRef $ \vmId -> handleVmReset state vmId
  ReqVmEdit vmRef mCpus mRam mDesc mHeadless mGa mCi -> withVm vmRef $ \vmId -> handleVmEdit state vmId mCpus mRam mDesc mHeadless mGa mCi
  ReqVmCloudInit vmRef -> withVm vmRef $ \vmId -> handleVmCloudInit state vmId
  -- Disk image handlers
  ReqDiskCreate name format sizeMb mPath -> handleDiskCreate state name format sizeMb mPath
  ReqDiskCreateOverlay name baseDiskRef optPath -> withDisk baseDiskRef $ \baseDiskId -> handleDiskCreateOverlay state name baseDiskId optPath
  ReqDiskRegister name path mFormat -> handleDiskRegister state name path mFormat
  ReqDiskRefresh diskRef -> withDisk diskRef $ \diskId -> handleDiskRefresh state diskId
  ReqDiskDelete diskRef -> withDisk diskRef $ \diskId -> handleDiskDelete state diskId
  ReqDiskResize diskRef newSizeMb -> withDisk diskRef $ \diskId -> handleDiskResize state diskId newSizeMb
  ReqDiskList -> handleDiskList state
  ReqDiskShow diskRef -> withDisk diskRef $ \diskId -> handleDiskShow state diskId
  ReqDiskClone name baseDiskRef optionalPath -> withDisk baseDiskRef $ \baseDiskId -> handleDiskClone state name baseDiskId optionalPath
  -- Snapshot handlers
  ReqSnapshotCreate diskRef name -> withDisk diskRef $ \diskId -> handleSnapshotCreate state diskId name
  ReqSnapshotDelete diskRef snapRef -> withDisk diskRef $ \diskId -> handleSnapshotDelete state diskId snapRef
  ReqSnapshotRollback diskRef snapRef -> withDisk diskRef $ \diskId -> handleSnapshotRollback state diskId snapRef
  ReqSnapshotMerge diskRef snapRef -> withDisk diskRef $ \diskId -> handleSnapshotMerge state diskId snapRef
  ReqSnapshotList diskRef -> withDisk diskRef $ \diskId -> handleSnapshotList state diskId
  -- Attach/detach handlers
  ReqDiskAttach vmRef diskRef interface media readOnly discard cache ->
    withVmDisk vmRef diskRef $ \vmId diskId -> handleDiskAttach state vmId diskId interface media readOnly discard cache
  ReqDiskDetach vmRef diskRef ->
    withVmDisk vmRef diskRef $ \vmId diskId -> handleDiskDetachByDisk state vmId diskId
  -- Shared directory handlers
  ReqSharedDirAdd vmRef path tag cache readOnly -> withVm vmRef $ \vmId -> handleSharedDirAdd state vmId path tag cache readOnly
  ReqSharedDirRemove vmRef dirRef -> do
    r1 <- resolveVm vmRef pool
    case r1 of
      Left _ -> pure RespVmNotFound
      Right vmId -> do
        r2 <- resolveSharedDir dirRef vmId pool
        case r2 of
          Left _ -> pure RespSharedDirNotFound
          Right dirId -> handleSharedDirRemove state vmId dirId
  ReqSharedDirList vmRef -> withVm vmRef $ \vmId -> handleSharedDirList state vmId
  -- Network interface handlers
  ReqNetIfAdd vmRef ifaceType hostDev mac mNwRef -> do
    r1 <- resolveVm vmRef pool
    case r1 of
      Left _ -> pure RespVmNotFound
      Right vmId -> do
        mNwId <- resolveOptionalNetwork mNwRef
        case mNwId of
          Left _ -> pure RespNetworkNotFound
          Right nwId -> handleNetIfAdd state vmId ifaceType hostDev mac nwId
  ReqNetIfRemove vmRef netIfId -> withVm vmRef $ \vmId -> handleNetIfRemove state vmId netIfId
  ReqNetIfList vmRef -> withVm vmRef $ \vmId -> handleNetIfList state vmId
  -- SSH key handlers
  ReqSshKeyCreate name publicKey -> handleSshKeyCreate state name publicKey
  ReqSshKeyDelete keyRef -> withSshKey keyRef $ \keyId -> handleSshKeyDelete state keyId
  ReqSshKeyList -> handleSshKeyList state
  ReqSshKeyAttach vmRef keyRef -> withVmSshKey vmRef keyRef $ \vmId keyId -> handleSshKeyAttach state vmId keyId
  ReqSshKeyDetach vmRef keyRef -> withVmSshKey vmRef keyRef $ \vmId keyId -> handleSshKeyDetach state vmId keyId
  ReqSshKeyListForVm vmRef -> withVm vmRef $ \vmId -> handleSshKeyListForVm state vmId
  -- Template handlers
  ReqTemplateCreate yaml -> handleTemplateCreate state yaml
  ReqTemplateDelete tRef -> withTemplate tRef $ \tid -> handleTemplateDelete state tid
  ReqTemplateList -> handleTemplateList state
  ReqTemplateShow tRef -> withTemplate tRef $ \tid -> handleTemplateShow state tid
  ReqTemplateInstantiate {} -> error "dispatchRequest: ReqTemplateInstantiate should be handled by dispatchRequestWithParent"
  -- Network handlers
  ReqNetworkCreate name subnet dhcp nat -> handleNetworkCreate state name subnet dhcp nat
  ReqNetworkDelete nwRef -> withNetwork nwRef $ \nwId -> handleNetworkDelete state nwId
  ReqNetworkStart {} -> error "dispatchRequest: ReqNetworkStart should be handled by dispatchRequestWithParent"
  ReqNetworkStop {} -> error "dispatchRequest: ReqNetworkStop should be handled by dispatchRequestWithParent"
  ReqNetworkList -> handleNetworkList state
  ReqNetworkShow nwRef -> withNetwork nwRef $ \nwId -> handleNetworkShow state nwId
  -- Guest execution handlers
  ReqGuestExec vmRef cmd -> withVm vmRef $ \vmId -> handleGuestExec state vmId cmd
  -- Disk URL import
  ReqDiskImportUrl name url mFmt -> handleDiskImportUrl state name url mFmt
  -- Apply config (always handled by dispatchApply via isApply guard)
  ReqApply {} -> error "dispatchRequest: ReqApply should be handled by dispatchApply"
  -- Task history queries (read-only, but dispatched here for completeness)
  ReqTaskList limit mSub mResult inclSub -> handleTaskList state limit mSub mResult inclSub
  ReqTaskShow taskId -> handleTaskShow state taskId
  ReqTaskListChildren parentId -> handleTaskListChildren state parentId
  where
    pool = ssDbPool state

    -- Single-entity resolution helpers (return domain-specific "not found")
    withVm :: Ref -> (Int64 -> IO Response) -> IO Response
    withVm ref f = resolveVm ref pool >>= either (const $ pure RespVmNotFound) f

    withDisk :: Ref -> (Int64 -> IO Response) -> IO Response
    withDisk ref f = resolveDisk ref pool >>= either (const $ pure RespDiskNotFound) f

    withNetwork :: Ref -> (Int64 -> IO Response) -> IO Response
    withNetwork ref f = resolveNetwork ref pool >>= either (const $ pure RespNetworkNotFound) f

    withSshKey :: Ref -> (Int64 -> IO Response) -> IO Response
    withSshKey ref f = resolveSshKey ref pool >>= either (const $ pure RespSshKeyNotFound) f

    withTemplate :: Ref -> (Int64 -> IO Response) -> IO Response
    withTemplate ref f = resolveTemplate ref pool >>= either (const $ pure RespTemplateNotFound) f

    -- Two-entity resolution helpers
    withVmDisk :: Ref -> Ref -> (Int64 -> Int64 -> IO Response) -> IO Response
    withVmDisk vmRef diskRef f = do
      r1 <- resolveVm vmRef pool
      r2 <- resolveDisk diskRef pool
      case (r1, r2) of
        (Right vmId, Right diskId) -> f vmId diskId
        (Left _, _) -> pure RespVmNotFound
        (_, Left _) -> pure RespDiskNotFound

    withVmSshKey :: Ref -> Ref -> (Int64 -> Int64 -> IO Response) -> IO Response
    withVmSshKey vmRef keyRef f = do
      r1 <- resolveVm vmRef pool
      r2 <- resolveSshKey keyRef pool
      case (r1, r2) of
        (Right vmId, Right keyId) -> f vmId keyId
        (Left _, _) -> pure RespVmNotFound
        (_, Left _) -> pure RespSshKeyNotFound

    resolveOptionalNetwork :: Maybe Ref -> IO (Either Text (Maybe Int64))
    resolveOptionalNetwork Nothing = pure $ Right Nothing
    resolveOptionalNetwork (Just nwRef) = do
      r <- resolveNetwork nwRef pool
      case r of
        Left err -> pure $ Left err
        Right nwId -> pure $ Right (Just nwId)

--------------------------------------------------------------------------------
-- Task History
--------------------------------------------------------------------------------

-- | Wrap a long-running handler: insert task record, fork handler in background,
-- return interim response immediately. Task stays "running" until background thread completes.
withTaskAsync :: ServerState -> Request -> IO Response -> Response -> IO Response
withTaskAsync state req action interimResponse = do
  let (subsystem, command, mEntityRef) = classifyRequest req
  now <- getCurrentTime
  mEntityInfo <- resolveEntityInfo mEntityRef

  taskKey <-
    runSqlPool
      ( insert
          Task
            { taskParent = Nothing
            , taskStartedAt = now
            , taskFinishedAt = Nothing
            , taskSubsystem = subsystem
            , taskEntityId = fmap fst mEntityInfo
            , taskEntityName = fmap snd mEntityInfo
            , taskCommand = command
            , taskResult = TaskRunning
            , taskMessage = Nothing
            }
      )
      (ssDbPool state)

  _ <- forkIO $ do
    result <- try action
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
              , TaskEntityId =. (mId <|> fmap fst mEntityInfo)
              , TaskEntityName =. (mName <|> fmap snd mEntityInfo)
              ]
          )
          (ssDbPool state)
      Left (err :: SomeException) ->
        runSqlPool
          ( update
              taskKey
              [ TaskFinishedAt =. Just finishTime
              , TaskResult =. TaskError
              , TaskMessage =. Just (T.pack $ show err)
              ]
          )
          (ssDbPool state)

  pure interimResponse
  where
    (<|>) Nothing b = b
    (<|>) a _ = a

    resolveEntityInfo :: Maybe Ref -> IO (Maybe (Int, Text))
    resolveEntityInfo Nothing = pure Nothing
    resolveEntityInfo (Just ref) = do
      let refText = unRef ref
      case reads (T.unpack refText) :: [(Int, String)] of
        [(n, "")] -> pure $ Just (n, refText)
        _ -> pure $ Just (0, refText)

-- | Wrap a handler with task history recording (synchronous).
-- Inserts a "running" record before the handler, updates it after.
withTask :: ServerState -> Request -> IO Response -> IO Response
withTask state req action = do
  let (subsystem, command, mEntityRef) = classifyRequest req
  now <- getCurrentTime

  -- Resolve entity name from ref (best-effort, for readability)
  mEntityInfo <- resolveEntityInfo mEntityRef

  -- Insert "running" task record
  taskKey <-
    runSqlPool
      ( insert
          Task
            { taskParent = Nothing
            , taskStartedAt = now
            , taskFinishedAt = Nothing
            , taskSubsystem = subsystem
            , taskEntityId = fmap fst mEntityInfo
            , taskEntityName = fmap snd mEntityInfo
            , taskCommand = command
            , taskResult = TaskRunning
            , taskMessage = Nothing
            }
      )
      pool

  -- Run the handler
  result <- try action
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
            , TaskEntityId =. (mId <|> fmap fst mEntityInfo)
            , TaskEntityName =. (mName <|> fmap snd mEntityInfo)
            ]
        )
        pool
      pure response
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
      pure $ RespError $ "Internal error: " <> T.pack (show err)
  where
    pool = ssDbPool state
    (<|>) Nothing b = b
    (<|>) a _ = a

    resolveEntityInfo :: Maybe Ref -> IO (Maybe (Int, Text))
    resolveEntityInfo Nothing = pure Nothing
    resolveEntityInfo (Just ref) = do
      -- Try to resolve the ref to get entity ID and name
      -- Best-effort: return Nothing on failure
      let refText = unRef ref
      case reads (T.unpack refText) :: [(Int, String)] of
        [(n, "")] -> pure $ Just (n, refText)
        _ -> pure $ Just (0, refText) -- Name-based ref, ID unknown until handler runs

-- | Like withTask, but passes the created TaskId to the handler.
-- Used for operations that create subtasks (e.g., apply).
withParentTask :: ServerState -> Request -> (TaskId -> IO Response) -> IO Response
withParentTask state req action = do
  let (subsystem, command, mEntityRef) = classifyRequest req
  now <- getCurrentTime
  mEntityInfo <- resolveEntityInfo mEntityRef
  taskKey <-
    runSqlPool
      ( insert
          Task
            { taskParent = Nothing
            , taskStartedAt = now
            , taskFinishedAt = Nothing
            , taskSubsystem = subsystem
            , taskEntityId = fmap fst mEntityInfo
            , taskEntityName = fmap snd mEntityInfo
            , taskCommand = command
            , taskResult = TaskRunning
            , taskMessage = Nothing
            }
      )
      pool
  result <- try (action taskKey)
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
            , TaskEntityId =. (mId <|> fmap fst mEntityInfo)
            , TaskEntityName =. (mName <|> fmap snd mEntityInfo)
            ]
        )
        pool
      pure response
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
      pure $ RespError $ "Internal error: " <> T.pack (show err)
  where
    pool = ssDbPool state
    (<|>) Nothing b = b
    (<|>) a _ = a
    resolveEntityInfo :: Maybe Ref -> IO (Maybe (Int, Text))
    resolveEntityInfo Nothing = pure Nothing
    resolveEntityInfo (Just ref) = do
      let refText = unRef ref
      case reads (T.unpack refText) :: [(Int, String)] of
        [(n, "")] -> pure $ Just (n, refText)
        _ -> pure $ Just (0, refText)

-- | Like withParentTask, but forks execution and returns immediately.
-- Returns the given interim response (or RespApplyStarted if Nothing).
withParentTaskAsync :: ServerState -> Request -> (TaskId -> IO Response) -> IO Response
withParentTaskAsync state req action = withParentTaskAsyncInterim state req action Nothing

-- | Like withParentTaskAsync but with a custom interim response.
withParentTaskAsyncInterim :: ServerState -> Request -> (TaskId -> IO Response) -> Maybe Response -> IO Response
withParentTaskAsyncInterim state req action mInterimResponse = do
  let (subsystem, command, mEntityRef) = classifyRequest req
  now <- getCurrentTime
  mEntityInfo <- resolveEntityInfo mEntityRef
  taskKey <-
    runSqlPool
      ( insert
          Task
            { taskParent = Nothing
            , taskStartedAt = now
            , taskFinishedAt = Nothing
            , taskSubsystem = subsystem
            , taskEntityId = fmap fst mEntityInfo
            , taskEntityName = fmap snd mEntityInfo
            , taskCommand = command
            , taskResult = TaskRunning
            , taskMessage = Nothing
            }
      )
      pool
  _ <- forkIO $ do
    result <- try (action taskKey)
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
              , TaskEntityId =. (mId <|> fmap fst mEntityInfo)
              , TaskEntityName =. (mName <|> fmap snd mEntityInfo)
              ]
          )
          pool
      Left (err :: SomeException) ->
        runSqlPool
          ( update
              taskKey
              [ TaskFinishedAt =. Just finishTime
              , TaskResult =. TaskError
              , TaskMessage =. Just (T.pack $ show err)
              ]
          )
          pool
  pure $ case mInterimResponse of
    Just resp -> resp
    Nothing -> RespApplyStarted (fromSqlKey taskKey)
  where
    pool = ssDbPool state
    (<|>) Nothing b = b
    (<|>) a _ = a
    resolveEntityInfo :: Maybe Ref -> IO (Maybe (Int, Text))
    resolveEntityInfo Nothing = pure Nothing
    resolveEntityInfo (Just ref) = do
      let refText = unRef ref
      case reads (T.unpack refText) :: [(Int, String)] of
        [(n, "")] -> pure $ Just (n, refText)
        _ -> pure $ Just (0, refText)

-- | Classify a request into subsystem, command name, and optional entity ref.
classifyRequest :: Request -> (TaskSubsystem, Text, Maybe Ref)
classifyRequest = \case
  ReqPing -> (SubSystem, "ping", Nothing)
  ReqStatus -> (SubSystem, "status", Nothing)
  ReqShutdown -> (SubSystem, "shutdown", Nothing)
  ReqListVms -> (SubVm, "list", Nothing)
  ReqShowVm ref -> (SubVm, "show", Just ref)
  ReqVmCreate name _ _ _ _ _ _ -> (SubVm, "create", Just (Ref name))
  ReqVmDelete ref -> (SubVm, "delete", Just ref)
  ReqVmStart ref _ -> (SubVm, "start", Just ref)
  ReqVmStop ref _ -> (SubVm, "stop", Just ref)
  ReqVmPause ref -> (SubVm, "pause", Just ref)
  ReqVmReset ref -> (SubVm, "reset", Just ref)
  ReqVmEdit ref _ _ _ _ _ _ -> (SubVm, "edit", Just ref)
  ReqVmCloudInit ref -> (SubVm, "cloud-init", Just ref)
  ReqDiskCreate name _ _ _ -> (SubDisk, "create", Just (Ref name))
  ReqDiskCreateOverlay name _ _ -> (SubDisk, "overlay", Just (Ref name))
  ReqDiskRegister name _ _ -> (SubDisk, "import", Just (Ref name))
  ReqDiskRefresh ref -> (SubDisk, "refresh", Just ref)
  ReqDiskDelete ref -> (SubDisk, "delete", Just ref)
  ReqDiskResize ref _ -> (SubDisk, "resize", Just ref)
  ReqDiskList -> (SubDisk, "list", Nothing)
  ReqDiskShow ref -> (SubDisk, "show", Just ref)
  ReqDiskClone name _ _ -> (SubDisk, "clone", Just (Ref name))
  ReqDiskImportUrl name _ _ -> (SubDisk, "import-url", Just (Ref name))
  ReqSnapshotCreate ref _ -> (SubSnapshot, "create", Just ref)
  ReqSnapshotDelete ref _ -> (SubSnapshot, "delete", Just ref)
  ReqSnapshotRollback ref _ -> (SubSnapshot, "rollback", Just ref)
  ReqSnapshotMerge ref _ -> (SubSnapshot, "merge", Just ref)
  ReqSnapshotList ref -> (SubSnapshot, "list", Just ref)
  ReqDiskAttach vmRef _ _ _ _ _ _ -> (SubDisk, "attach", Just vmRef)
  ReqDiskDetach vmRef _ -> (SubDisk, "detach", Just vmRef)
  ReqSharedDirAdd ref _ _ _ _ -> (SubSharedDir, "add", Just ref)
  ReqSharedDirRemove ref _ -> (SubSharedDir, "remove", Just ref)
  ReqSharedDirList ref -> (SubSharedDir, "list", Just ref)
  ReqNetIfAdd ref _ _ _ _ -> (SubVm, "add-netif", Just ref)
  ReqNetIfRemove ref _ -> (SubVm, "remove-netif", Just ref)
  ReqNetIfList ref -> (SubVm, "list-netif", Just ref)
  ReqSshKeyCreate name _ -> (SubSshKey, "create", Just (Ref name))
  ReqSshKeyDelete ref -> (SubSshKey, "delete", Just ref)
  ReqSshKeyList -> (SubSshKey, "list", Nothing)
  ReqSshKeyAttach _ ref -> (SubSshKey, "attach", Just ref)
  ReqSshKeyDetach _ ref -> (SubSshKey, "detach", Just ref)
  ReqSshKeyListForVm ref -> (SubSshKey, "list-for-vm", Just ref)
  ReqTemplateCreate _ -> (SubTemplate, "create", Nothing)
  ReqTemplateDelete ref -> (SubTemplate, "delete", Just ref)
  ReqTemplateList -> (SubTemplate, "list", Nothing)
  ReqTemplateShow ref -> (SubTemplate, "show", Just ref)
  ReqTemplateInstantiate ref name -> (SubTemplate, "instantiate", Just ref)
  ReqNetworkCreate name _ _ _ -> (SubNetwork, "create", Just (Ref name))
  ReqNetworkDelete ref -> (SubNetwork, "delete", Just ref)
  ReqNetworkStart ref -> (SubNetwork, "start", Just ref)
  ReqNetworkStop ref _ -> (SubNetwork, "stop", Just ref)
  ReqNetworkList -> (SubNetwork, "list", Nothing)
  ReqNetworkShow ref -> (SubNetwork, "show", Just ref)
  ReqGuestExec ref _ -> (SubVm, "guest-exec", Just ref)
  ReqApply {} -> (SubApply, "apply", Nothing)
  ReqTaskList {} -> (SubSystem, "task-list", Nothing)
  ReqTaskShow _ -> (SubSystem, "task-show", Nothing)
  ReqTaskListChildren _ -> (SubSystem, "task-list-children", Nothing)

-- | Check if a request is read-only (should not be recorded in task history).
isReadOnly :: Request -> Bool
isReadOnly = \case
  ReqPing -> True
  ReqStatus -> True
  ReqListVms -> True
  ReqShowVm _ -> True
  ReqDiskList -> True
  ReqDiskShow _ -> True
  ReqSnapshotList _ -> True
  ReqSharedDirList _ -> True
  ReqNetIfList _ -> True
  ReqSshKeyList -> True
  ReqSshKeyListForVm _ -> True
  ReqTemplateList -> True
  ReqTemplateShow _ -> True
  ReqNetworkList -> True
  ReqNetworkShow _ -> True
  ReqTaskList {} -> True
  ReqTaskShow _ -> True
  ReqTaskListChildren _ -> True
  _ -> False

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
  RespNetworkStarted -> (TaskSuccess, Just "Started")
  RespNetworkStopped -> (TaskSuccess, Just "Stopped")
  RespApplyResult _ -> (TaskSuccess, Nothing)
  RespApplyStarted tid -> (TaskSuccess, Just $ "Task ID " <> T.pack (show tid))
  RespSharedDirAdded did -> (TaskSuccess, Just $ "Dir ID " <> T.pack (show did))
  RespSharedDirOk -> (TaskSuccess, Nothing)
  RespNetIfAdded nid -> (TaskSuccess, Just $ "NetIf ID " <> T.pack (show nid))
  RespNetIfOk -> (TaskSuccess, Nothing)
  RespGuestExecResult code _ _ -> (TaskSuccess, Just $ "Exit code " <> T.pack (show code))
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

--------------------------------------------------------------------------------
-- Task History Handlers
--------------------------------------------------------------------------------

-- | List task history entries.
-- When includeSubtasks is False, only top-level tasks (parent = NULL) are returned.
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
    , tiEntityId = taskEntityId th
    , tiEntityName = taskEntityName th
    , tiCommand = taskCommand th
    , tiResult = taskResult th
    , tiMessage = taskMessage th
    }

-- | Helper to filter out Nothing values
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs
