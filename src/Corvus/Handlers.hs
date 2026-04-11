{-# LANGUAGE LambdaCase #-}

-- | Request handling module.
-- Re-exports handlers from submodules and provides the main dispatch function.
-- Mutating requests are dispatched as Action types with automatic task recording.
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
  , module Corvus.Handlers.CloudInit
  )
where

import Corvus.Action
import Corvus.Handlers.Apply
import Corvus.Handlers.CloudInit
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
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

--------------------------------------------------------------------------------
-- Request Dispatch
--------------------------------------------------------------------------------

-- | Handle a request and produce a response.
-- Read-only requests are dispatched directly.
-- Mutating requests are dispatched as Action types with automatic task recording.
handleRequest :: ServerState -> Request -> IO Response
handleRequest state = \case
  -- Read-only requests (no task recording)
  ReqPing -> handlePing
  ReqStatus -> handleStatus state
  ReqShutdown -> handleShutdown state
  ReqListVms -> handleVmList state
  ReqShowVm vmRef -> withVm vmRef $ \vmId -> handleVmShow state vmId
  ReqDiskList -> handleDiskList state
  ReqDiskShow diskRef -> withDisk diskRef $ \diskId -> handleDiskShow state diskId
  ReqSnapshotList diskRef -> withDisk diskRef $ \diskId -> handleSnapshotList state diskId
  ReqSharedDirList vmRef -> withVm vmRef $ \vmId -> handleSharedDirList state vmId
  ReqNetIfList vmRef -> withVm vmRef $ \vmId -> handleNetIfList state vmId
  ReqSshKeyList -> handleSshKeyList state
  ReqSshKeyListForVm vmRef -> withVm vmRef $ \vmId -> handleSshKeyListForVm state vmId
  ReqTemplateList -> handleTemplateList state
  ReqTemplateShow tRef -> withTemplate tRef $ \tid -> handleTemplateShow state tid
  ReqNetworkList -> handleNetworkList state
  ReqNetworkShow nwRef -> withNetwork nwRef $ \nwId -> handleNetworkShow state nwId
  ReqCloudInitGet vmRef -> withVm vmRef $ \vmId -> handleCloudInitGet state vmId
  ReqSerialConsole vmRef -> withVm vmRef $ \vmId -> handleSerialConsole state vmId
  ReqSerialConsoleFlush vmRef -> withVm vmRef $ \vmId -> handleSerialConsoleFlush state vmId
  ReqTaskList limit mSub mResult inclSub -> handleTaskList state limit mSub mResult inclSub
  ReqTaskShow taskId -> handleTaskShow state taskId
  ReqTaskListChildren parentId -> handleTaskListChildren state parentId
  ReqVmCloudInit vmRef -> withVm vmRef $ \vmId -> handleVmCloudInit state vmId
  -- VM mutations
  ReqVmCreate name cpus ram desc headless ga ci as ->
    runAction state (VmCreate name cpus ram desc headless ga ci as)
  ReqVmDelete vmRef deleteDisks -> withVm vmRef $ \vmId ->
    runActionWithSubtasks state (VmDelete vmId deleteDisks)
  ReqVmStart vmRef wait -> withVm vmRef $ \vmId ->
    if wait
      then runAction state (VmStart vmId)
      else runActionAsync state (VmStart vmId) (RespVmStateChanged VmStarting)
  ReqVmStop vmRef wait -> withVm vmRef $ \vmId ->
    if wait
      then runAction state (VmStop vmId)
      else runActionAsync state (VmStop vmId) (RespVmStateChanged VmStopping)
  ReqVmPause vmRef -> withVm vmRef $ \vmId ->
    runAction state (VmPause vmId)
  ReqVmReset vmRef -> withVm vmRef $ \vmId ->
    runAction state (VmReset vmId)
  ReqVmEdit vmRef mCpus mRam mDesc mHeadless mGa mCi mAs -> withVm vmRef $ \vmId ->
    runAction state (VmEdit vmId mCpus mRam mDesc mHeadless mGa mCi mAs)
  -- Disk mutations
  ReqDiskCreate name format sizeMb mPath ->
    runAction state (DiskCreate name format sizeMb mPath)
  ReqDiskCreateOverlay name baseDiskRef optPath -> withDisk baseDiskRef $ \baseDiskId ->
    runAction state (DiskCreateOverlay name baseDiskId Nothing optPath)
  ReqDiskRegister name path mFormat ->
    runAction state (DiskRegister name path mFormat)
  ReqDiskImportUrl name url mFmt ->
    runAction state (DiskImportUrl name url mFmt)
  ReqDiskRefresh diskRef -> withDisk diskRef $ \diskId ->
    runAction state (DiskRefresh diskId)
  ReqDiskDelete diskRef -> withDisk diskRef $ \diskId ->
    runAction state (DiskDelete diskId)
  ReqDiskResize diskRef newSizeMb -> withDisk diskRef $ \diskId ->
    runAction state (DiskResize diskId newSizeMb)
  ReqDiskClone name baseDiskRef optPath -> withDisk baseDiskRef $ \baseDiskId ->
    runAction state (DiskClone name baseDiskId Nothing optPath)
  -- Snapshot mutations
  ReqSnapshotCreate diskRef name -> withDisk diskRef $ \diskId ->
    runAction state (SnapshotCreate diskId name)
  ReqSnapshotDelete diskRef snapRef -> withDisk diskRef $ \diskId ->
    runAction state (SnapshotDelete diskId snapRef)
  ReqSnapshotRollback diskRef snapRef -> withDisk diskRef $ \diskId ->
    runAction state (SnapshotRollback diskId snapRef)
  ReqSnapshotMerge diskRef snapRef -> withDisk diskRef $ \diskId ->
    runAction state (SnapshotMerge diskId snapRef)
  -- Attach/detach
  ReqDiskAttach vmRef diskRef interface media readOnly discard cache ->
    withVmDisk vmRef diskRef $ \vmId diskId ->
      runAction state (DiskAttach vmId diskId interface media readOnly discard cache)
  ReqDiskDetach vmRef diskRef ->
    withVmDisk vmRef diskRef $ \vmId diskId ->
      runAction state (DiskDetachByDisk vmId diskId)
  -- Shared directories
  ReqSharedDirAdd vmRef path tag cache readOnly -> withVm vmRef $ \vmId ->
    runAction state (SharedDirAdd vmId path tag cache readOnly)
  ReqSharedDirRemove vmRef dirRef -> do
    r1 <- resolveVm vmRef pool
    case r1 of
      Left _ -> pure RespVmNotFound
      Right vmId -> do
        r2 <- resolveSharedDir dirRef vmId pool
        case r2 of
          Left _ -> pure RespSharedDirNotFound
          Right dirId -> runAction state (SharedDirRemove vmId dirId)
  -- Network interfaces
  ReqNetIfAdd vmRef ifaceType hostDev mac mNwRef -> do
    r1 <- resolveVm vmRef pool
    case r1 of
      Left _ -> pure RespVmNotFound
      Right vmId -> do
        mNwId <- resolveOptionalNetwork mNwRef
        case mNwId of
          Left _ -> pure RespNetworkNotFound
          Right nwId -> runAction state (NetIfAdd vmId ifaceType hostDev mac nwId)
  ReqNetIfRemove vmRef netIfId -> withVm vmRef $ \vmId ->
    runAction state (NetIfRemove vmId netIfId)
  -- SSH keys
  ReqSshKeyCreate name publicKey ->
    runAction state (SshKeyCreate name publicKey)
  ReqSshKeyDelete keyRef -> withSshKey keyRef $ \keyId ->
    runAction state (SshKeyDelete keyId)
  ReqSshKeyAttach vmRef keyRef -> withVmSshKey vmRef keyRef $ \vmId keyId ->
    runAction state (SshKeyAttach vmId keyId)
  ReqSshKeyDetach vmRef keyRef -> withVmSshKey vmRef keyRef $ \vmId keyId ->
    runAction state (SshKeyDetach vmId keyId)
  -- Templates
  ReqTemplateCreate yaml ->
    runAction state (TemplateCreate yaml)
  ReqTemplateDelete tRef -> withTemplate tRef $ \tid ->
    runAction state (TemplateDelete tid)
  ReqTemplateInstantiate tRef name -> withTemplate tRef $ \tid ->
    runAction state (TemplateInstantiate tid name)
  -- Networks
  ReqNetworkCreate name subnet dhcp nat as ->
    runAction state (NetworkCreate name subnet dhcp nat as)
  ReqNetworkDelete nwRef -> withNetwork nwRef $ \nwId ->
    runAction state (NetworkDelete nwId)
  ReqNetworkStart nwRef -> withNetwork nwRef $ \nwId ->
    runAction state (NetworkStart nwId)
  ReqNetworkStop nwRef force -> withNetwork nwRef $ \nwId ->
    runAction state (NetworkStop nwId force)
  ReqNetworkEdit nwRef mSubnet mDhcp mNat mAutostart -> withNetwork nwRef $ \nwId ->
    runAction state (NetworkEdit nwId mSubnet mDhcp mNat mAutostart)
  -- Guest execution
  ReqGuestExec vmRef cmd -> withVm vmRef $ \vmId ->
    runAction state (GuestExec vmId cmd)
  -- Cloud-init config
  ReqCloudInitSet vmRef mUserData mNetConfig injectKeys -> withVm vmRef $ \vmId ->
    runAction state (CloudInitSet vmId mUserData mNetConfig injectKeys)
  ReqCloudInitDelete vmRef -> withVm vmRef $ \vmId ->
    runAction state (CloudInitDelete vmId)
  -- Apply (special: validates YAML first, supports sync/async)
  ReqApply yaml skipExisting wait -> do
    validated <- handleApplyValidate state yaml
    case validated of
      Left errResp -> pure errResp
      Right config ->
        if wait
          then runAction state (ApplyAction config skipExisting)
          else runActionAsyncWithId state (ApplyAction config skipExisting) RespApplyStarted
  where
    pool = ssDbPool state

    -- Single-entity resolution helpers
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
