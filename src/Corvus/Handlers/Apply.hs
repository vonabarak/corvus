{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handler for applying declarative environment configurations from YAML.
-- Creates SSH keys, disks, networks, and VMs in dependency order.
module Corvus.Handlers.Apply
  ( handleApplyValidate
  , handleApplyExecute
  )
where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Handlers.Disk
  ( cloneDiskIO
  , createDiskIO
  , createOverlayDiskIO
  , detectFormatFromPath
  , importDiskFromUrlIO
  , registerDiskIO
  )
import Corvus.Handlers.NetIf (generateMacAddress)
import Corvus.Handlers.Resolve (validateName)
import Corvus.Handlers.SshKey (regenerateCloudInitIso)
import Corvus.Handlers.Subtask
import Corvus.Model
import Corvus.Protocol
import Corvus.Qemu.Image (isHttpUrl)
import Corvus.Types
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Yaml (FromJSON (..), decodeEither', withObject, (.!=), (.:), (.:?))
import Database.Persist
import Database.Persist.Postgresql (SqlBackend, runSqlPool)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

--------------------------------------------------------------------------------
-- YAML Types
--------------------------------------------------------------------------------

data ApplyConfig = ApplyConfig
  { acSshKeys :: [ApplySshKey]
  , acDisks :: [ApplyDisk]
  , acNetworks :: [ApplyNetwork]
  , acVms :: [ApplyVm]
  }
  deriving (Show)

instance FromJSON ApplyConfig where
  parseJSON = withObject "ApplyConfig" $ \o ->
    ApplyConfig
      <$> o .:? "sshKeys" .!= []
      <*> o .:? "disks" .!= []
      <*> o .:? "networks" .!= []
      <*> o .:? "vms" .!= []

data ApplySshKey = ApplySshKey
  { askName :: Text
  , askPublicKey :: Text
  }
  deriving (Show)

instance FromJSON ApplySshKey where
  parseJSON = withObject "ApplySshKey" $ \o ->
    ApplySshKey
      <$> o .: "name"
      <*> o .: "publicKey"

-- | Disk definition in the apply YAML config.
--
-- The @path@ field controls where the disk image file is placed:
--
--   * Not specified: the image is placed in the base images directory
--   * Starts with @\/@: interpreted as an absolute path
--   * Otherwise: relative to the base images directory
--   * Ends with @\/@: treated as a directory (auto-generates filename from disk name + format extension)
--   * Does not end with @\/@: treated as the full file path
--
-- Examples:
--
-- @
-- path: "ws25/"           # -> $BASE/ws25/my-disk.qcow2
-- path: "my-disk.raw"     # -> $BASE/my-disk.raw
-- path: "/data/vms/"      # -> /data/vms/my-disk.qcow2
-- path: "/data/disk.raw"  # -> /data/disk.raw
-- @
data ApplyDisk = ApplyDisk
  { adName :: Text
  , adFormat :: Maybe DriveFormat
  , adSizeMb :: Maybe Int
  , adImport :: Maybe Text
  , adOverlay :: Maybe Text
  , adClone :: Maybe Text
  , adPath :: Maybe Text
  }
  deriving (Show)

instance FromJSON ApplyDisk where
  parseJSON = withObject "ApplyDisk" $ \o ->
    ApplyDisk
      <$> o .: "name"
      <*> o .:? "format"
      <*> o .:? "sizeMb"
      <*> o .:? "import"
      <*> o .:? "overlay"
      <*> o .:? "clone"
      <*> o .:? "path"

data ApplyNetwork = ApplyNetwork
  { anName :: Text
  , anSubnet :: Text
  , anDhcp :: Bool
  , anNat :: Bool
  }
  deriving (Show)

instance FromJSON ApplyNetwork where
  parseJSON = withObject "ApplyNetwork" $ \o ->
    ApplyNetwork
      <$> o .: "name"
      <*> o .:? "subnet" .!= ""
      <*> o .:? "dhcp" .!= False
      <*> o .:? "nat" .!= False

data ApplyVm = ApplyVm
  { avName :: Text
  , avCpuCount :: Int
  , avRamMb :: Int
  , avDescription :: Maybe Text
  , avHeadless :: Bool
  , avGuestAgent :: Bool
  , avCloudInit :: Maybe Bool
  , avDrives :: [ApplyDrive]
  , avNetworkInterfaces :: [ApplyNetIf]
  , avSharedDirs :: [ApplySharedDir]
  , avSshKeys :: [Text]
  }
  deriving (Show)

instance FromJSON ApplyVm where
  parseJSON = withObject "ApplyVm" $ \o ->
    ApplyVm
      <$> o .: "name"
      <*> o .: "cpuCount"
      <*> o .: "ramMb"
      <*> o .:? "description"
      <*> o .:? "headless" .!= False
      <*> o .:? "guestAgent" .!= False
      <*> o .:? "cloudInit"
      <*> o .:? "drives" .!= []
      <*> o .:? "networkInterfaces" .!= []
      <*> o .:? "sharedDirs" .!= []
      <*> o .:? "sshKeys" .!= []

data ApplyDrive = ApplyDrive
  { adrDisk :: Text
  , adrInterface :: DriveInterface
  , adrMedia :: Maybe DriveMedia
  , adrReadOnly :: Bool
  , adrCacheType :: CacheType
  , adrDiscard :: Bool
  }
  deriving (Show)

instance FromJSON ApplyDrive where
  parseJSON = withObject "ApplyDrive" $ \o ->
    ApplyDrive
      <$> o .: "disk"
      <*> o .: "interface"
      <*> o .:? "media"
      <*> o .:? "readOnly" .!= False
      <*> o .:? "cacheType" .!= CacheWriteback
      <*> o .:? "discard" .!= False

data ApplyNetIf = ApplyNetIf
  { aniType :: NetInterfaceType
  , aniHostDevice :: Maybe Text
  , aniNetwork :: Maybe Text
  , aniMac :: Maybe Text
  }
  deriving (Show)

instance FromJSON ApplyNetIf where
  parseJSON = withObject "ApplyNetIf" $ \o -> do
    mType <- o .:? "type"
    network <- o .:? "network"
    hostDevice <- o .:? "hostDevice"
    mac <- o .:? "mac"
    ifType <- case (mType, network) of
      (Nothing, Just _) -> pure NetManaged
      (Just t, Just _)
        | t /= NetManaged -> fail "network interface with 'network' must have type 'managed' or omit 'type'"
        | otherwise -> pure NetManaged
      (Just t, Nothing) -> pure t
      (Nothing, Nothing) -> pure NetUser
    pure $ ApplyNetIf ifType hostDevice network mac

data ApplySharedDir = ApplySharedDir
  { asdPath :: Text
  , asdTag :: Text
  , asdCache :: SharedDirCache
  , asdReadOnly :: Bool
  }
  deriving (Show)

instance FromJSON ApplySharedDir where
  parseJSON = withObject "ApplySharedDir" $ \o ->
    ApplySharedDir
      <$> o .: "path"
      <*> o .: "tag"
      <*> o .:? "cache" .!= CacheAuto
      <*> o .:? "readOnly" .!= False

--------------------------------------------------------------------------------
-- Handler
--------------------------------------------------------------------------------

-- | Validate YAML and config (synchronous, fast).
-- Returns Left Response on error, Right ApplyConfig on success.
handleApplyValidate :: ServerState -> Text -> IO (Either Response ApplyConfig)
handleApplyValidate state yamlContent = runServerLogging state $ do
  case decodeEither' (TE.encodeUtf8 yamlContent) of
    Left err -> do
      let msg = T.pack $ show err
      logWarnN $ "Failed to parse apply config YAML: " <> msg
      pure $ Left $ RespError msg
    Right config -> do
      case validateConfig config of
        Left err -> do
          logWarnN $ "Config validation failed: " <> err
          pure $ Left $ RespError err
        Right () -> pure $ Right config

-- | Execute apply with subtask tracking.
-- Called by dispatchApply with the parent task ID.
handleApplyExecute :: ServerState -> ApplyConfig -> Bool -> TaskId -> IO Response
handleApplyExecute state config skipExisting parentTaskId = runServerLogging state $ do
  logInfoN "Applying environment configuration..."

  -- Build the list of subtask specs from the config
  let subtaskSpecs = buildSubtaskSpecs config
  -- Pre-create all subtasks in NotStarted state
  subtaskIds <- liftIO $ mapM (createSubtask (ssDbPool state) parentTaskId) subtaskSpecs

  -- Execute with subtask tracking
  result <- liftIO $ executeApplyWithSubtasks state config skipExisting (ssDbPool state) (zip subtaskSpecs subtaskIds)
  case result of
    Left err -> do
      logWarnN $ "Apply failed: " <> err
      pure $ RespError err
    Right applyResult -> do
      logInfoN "Apply completed successfully"
      pure $ RespApplyResult applyResult

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validateConfig :: ApplyConfig -> Either Text ()
validateConfig config = do
  checkDuplicates "SSH key" $ map askName (acSshKeys config)
  checkDuplicates "disk" $ map adName (acDisks config)
  checkDuplicates "network" $ map anName (acNetworks config)
  checkDuplicates "VM" $ map avName (acVms config)
  forM_ (acSshKeys config) $ \k -> validateName "SSH key" (askName k)
  forM_ (acDisks config) $ \d -> validateName "Disk" (adName d)
  forM_ (acNetworks config) $ \n -> validateName "Network" (anName n)
  forM_ (acVms config) $ \v -> validateName "VM" (avName v)
  forM_ (acDisks config) validateDisk
  forM_ (acVms config) validateVmCloudInit
  where
    checkDuplicates :: Text -> [Text] -> Either Text ()
    checkDuplicates kind names =
      case findDuplicate names of
        Nothing -> Right ()
        Just d -> Left $ "Duplicate " <> kind <> " name: " <> d

    findDuplicate :: [Text] -> Maybe Text
    findDuplicate [] = Nothing
    findDuplicate (x : xs)
      | x `elem` xs = Just x
      | otherwise = findDuplicate xs

    validateVmCloudInit :: ApplyVm -> Either Text ()
    validateVmCloudInit v =
      let ci = effectiveCloudInit v
       in if not (null (avSshKeys v)) && not ci
            then Left $ "VM '" <> avName v <> "': has SSH keys but cloud-init is not enabled"
            else Right ()

    validateDisk :: ApplyDisk -> Either Text ()
    validateDisk d =
      let hasImport = isJust (adImport d)
          hasOverlay = isJust (adOverlay d)
          hasClone = isJust (adClone d)
          hasCreate = isJust (adFormat d) && isJust (adSizeMb d) && not hasImport && not hasOverlay && not hasClone
          strategies = length $ filter id [hasImport, hasOverlay, hasClone]
       in if strategies > 1
            then Left $ "Disk '" <> adName d <> "': cannot specify more than one of 'import', 'overlay', 'clone'"
            else
              if not hasImport && not hasOverlay && not hasClone && not hasCreate
                then Left $ "Disk '" <> adName d <> "': must specify 'import', 'overlay', 'clone', or both 'format' and 'sizeMb'"
                else
                  if isJust (adPath d) && not hasOverlay && not hasClone && not hasCreate
                    then Left $ "Disk '" <> adName d <> "': 'path' can only be used with 'overlay', 'clone', or 'create'"
                    else Right ()

-- | Resolve effective cloudInit value for a VM.
-- If explicitly set, use that. Otherwise, auto-enable when sshKeys are present.
effectiveCloudInit :: ApplyVm -> Bool
effectiveCloudInit v = case avCloudInit v of
  Just ci -> ci
  Nothing -> not (null (avSshKeys v))

--------------------------------------------------------------------------------
-- Execution
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Subtask planning
--------------------------------------------------------------------------------

-- | Build the list of subtask specs from an apply config.
-- Order matches execution order: SSH keys, disks, networks, VMs (with attachments).
buildSubtaskSpecs :: ApplyConfig -> [SubtaskSpec]
buildSubtaskSpecs config =
  map sshKeySpec (acSshKeys config)
    ++ map diskSpec (acDisks config)
    ++ map networkSpec (acNetworks config)
    ++ concatMap vmSpecs (acVms config)
  where
    sshKeySpec k = SubtaskSpec SubSshKey "create" (Just $ askName k)
    diskSpec d = SubtaskSpec SubDisk (diskCommand d) (Just $ adName d)
    diskCommand d
      | isJust (adImport d) = if maybe False isHttpUrl (adImport d) then "import-url" else "import"
      | isJust (adOverlay d) = "overlay"
      | isJust (adClone d) = "clone"
      | otherwise = "create"
    networkSpec n = SubtaskSpec SubNetwork "create" (Just $ anName n)
    vmSpecs v =
      [SubtaskSpec SubVm "create" (Just $ avName v)]

-- | Execute apply with subtask tracking.
-- Takes (spec, taskId) pairs aligned with the config resources.
executeApplyWithSubtasks
  :: ServerState
  -> ApplyConfig
  -> Bool
  -> Pool SqlBackend
  -> [(SubtaskSpec, TaskId)]
  -> IO (Either Text ApplyResult)
executeApplyWithSubtasks _state _config _skipExisting _pool [] =
  pure $
    Right
      ApplyResult
        { arSshKeys = []
        , arDisks = []
        , arNetworks = []
        , arVms = []
        }
executeApplyWithSubtasks state config skipExisting pool subtasks = do
  -- Split subtasks into phases by counting config items
  let nKeys = length (acSshKeys config)
      nDisks = length (acDisks config)
      nNetworks = length (acNetworks config)
      (keySubtasks, rest1) = splitAt nKeys subtasks
      (diskSubtasks, rest2) = splitAt nDisks rest1
      (nwSubtasks, vmSubtasks) = splitAt nNetworks rest2

  -- Extract the parent task ID from any subtask's parent field
  parentTaskId <- case subtasks of
    ((_, firstSubId) : _) -> do
      mParent <- runSqlPool (get firstSubId) pool
      case mParent >>= taskParent of
        Just pid -> pure pid
        Nothing -> error "executeApplyWithSubtasks: subtask has no parent"
    _ -> error "executeApplyWithSubtasks: impossible"

  -- Phase 1: SSH keys
  keyResult <-
    runSubtasksSequential
      state
      (acSshKeys config)
      keySubtasks
      skipExisting
      parentTaskId
      Map.empty
      (\st k sub m -> createOneSshKeyTracked st k sub m skipExisting parentTaskId pool)
  case keyResult of
    Left err -> pure $ Left err
    Right (keyMap, keyCreated) -> do
      -- Phase 2: Disks
      diskResult <-
        runSubtasksSequential
          state
          (acDisks config)
          diskSubtasks
          skipExisting
          parentTaskId
          Map.empty
          (\st d sub m -> createOneDiskTracked st d sub m skipExisting parentTaskId pool)
      case diskResult of
        Left err -> pure $ Left err
        Right (diskMap, diskCreated) -> do
          -- Phase 3: Networks
          nwResult <-
            runSubtasksSequential
              state
              (acNetworks config)
              nwSubtasks
              skipExisting
              parentTaskId
              Map.empty
              (\st n sub m -> createOneNetworkTracked st n sub m skipExisting parentTaskId pool)
          case nwResult of
            Left err -> pure $ Left err
            Right (nwMap, nwCreated) -> do
              -- Phase 4: VMs (each VM has one subtask for the whole create+attach)
              vmResult <- runVmSubtasks state keyMap diskMap nwMap (acVms config) vmSubtasks skipExisting parentTaskId pool
              case vmResult of
                Left err -> pure $ Left err
                Right vmCreated ->
                  pure $
                    Right
                      ApplyResult
                        { arSshKeys = keyCreated
                        , arDisks = diskCreated
                        , arNetworks = nwCreated
                        , arVms = vmCreated
                        }

-- | Run a sequence of subtasks, building up a map and created list.
runSubtasksSequential
  :: ServerState
  -> [a]
  -> [(SubtaskSpec, TaskId)]
  -> Bool
  -> TaskId
  -> Map.Map Text Int64
  -> (ServerState -> a -> (SubtaskSpec, TaskId) -> Map.Map Text Int64 -> IO (Either Text (Text, Int64)))
  -> IO (Either Text (Map.Map Text Int64, [ApplyCreated]))
runSubtasksSequential _ [] _ _ _ m _ = pure $ Right (m, [])
runSubtasksSequential state items subs skipExisting parentId initMap action = go items subs initMap []
  where
    go [] _ m acc = pure $ Right (m, reverse acc)
    go (item : rest) ((spec, subId) : restSubs) m acc = do
      result <- action state item (spec, subId) m
      case result of
        Left err -> pure $ Left err
        Right (name, rid) ->
          go rest restSubs (Map.insert name rid m) (ApplyCreated name rid : acc)
    go _ [] _ _ = pure $ Left "Internal error: subtask count mismatch"

-- | Create one SSH key with subtask tracking
createOneSshKeyTracked
  :: ServerState
  -> ApplySshKey
  -> (SubtaskSpec, TaskId)
  -> Map.Map Text Int64
  -> Bool
  -> TaskId
  -> Pool SqlBackend
  -> IO (Either Text (Text, Int64))
createOneSshKeyTracked state k (_, subId) _m skipExisting parentId pool = do
  mExisting <-
    if skipExisting
      then resolveByName state UniqueSshKeyName Map.empty (askName k)
      else pure Nothing
  case mExisting of
    Just existingId -> do
      completeSubtask pool subId TaskSuccess (Just "Already exists") (Just (fromIntegral existingId))
      pure $ Right (askName k, existingId)
    Nothing -> do
      startSubtask pool subId
      now <- getCurrentTime
      result <-
        try $
          runSqlPool
            ( insert
                SshKey
                  { sshKeyName = askName k
                  , sshKeyPublicKey = askPublicKey k
                  , sshKeyCreatedAt = now
                  }
            )
            (ssDbPool state)
      case result of
        Left e -> do
          let err = "SSH key '" <> askName k <> "': " <> formatException e
          completeSubtask pool subId TaskError (Just err) Nothing
          cancelRemainingSubtasks pool parentId
          pure $ Left err
        Right keyEntity -> do
          let keyId = fromSqlKey keyEntity
          completeSubtask pool subId TaskSuccess Nothing (Just (fromIntegral keyId))
          pure $ Right (askName k, keyId)

-- | Create one disk with subtask tracking
createOneDiskTracked
  :: ServerState
  -> ApplyDisk
  -> (SubtaskSpec, TaskId)
  -> Map.Map Text Int64
  -> Bool
  -> TaskId
  -> Pool SqlBackend
  -> IO (Either Text (Text, Int64))
createOneDiskTracked state d (_, subId) diskMap skipExisting parentId pool = do
  mExisting <-
    if skipExisting
      then resolveByName state UniqueDiskImageName Map.empty (adName d)
      else pure Nothing
  case mExisting of
    Just existingId -> do
      completeSubtask pool subId TaskSuccess (Just "Already exists") (Just (fromIntegral existingId))
      pure $ Right (adName d, existingId)
    Nothing -> do
      startSubtask pool subId
      result <- createOneDisk state diskMap d
      case result of
        Left err -> do
          let fullErr = "Disk '" <> adName d <> "': " <> err
          completeSubtask pool subId TaskError (Just fullErr) Nothing
          cancelRemainingSubtasks pool parentId
          pure $ Left fullErr
        Right diskId -> do
          completeSubtask pool subId TaskSuccess Nothing (Just (fromIntegral diskId))
          pure $ Right (adName d, diskId)

-- | Create one network with subtask tracking
createOneNetworkTracked
  :: ServerState
  -> ApplyNetwork
  -> (SubtaskSpec, TaskId)
  -> Map.Map Text Int64
  -> Bool
  -> TaskId
  -> Pool SqlBackend
  -> IO (Either Text (Text, Int64))
createOneNetworkTracked state n (_, subId) _nwMap skipExisting parentId pool = do
  mExisting <-
    if skipExisting
      then resolveByName state UniqueNetworkName Map.empty (anName n)
      else pure Nothing
  case mExisting of
    Just existingId -> do
      completeSubtask pool subId TaskSuccess (Just "Already exists") (Just (fromIntegral existingId))
      pure $ Right (anName n, existingId)
    Nothing -> do
      startSubtask pool subId
      now <- getCurrentTime
      result <-
        try $
          runSqlPool
            ( insert
                Network
                  { networkName = anName n
                  , networkSubnet = anSubnet n
                  , networkDhcp = anDhcp n
                  , networkNat = anNat n
                  , networkRunning = False
                  , networkDnsmasqPid = Nothing
                  , networkCreatedAt = now
                  }
            )
            (ssDbPool state)
      case result of
        Left e -> do
          let err = "Network '" <> anName n <> "': " <> formatException e
          completeSubtask pool subId TaskError (Just err) Nothing
          cancelRemainingSubtasks pool parentId
          pure $ Left err
        Right nwId -> do
          let nid = fromSqlKey nwId
          completeSubtask pool subId TaskSuccess Nothing (Just (fromIntegral nid))
          pure $ Right (anName n, nid)

-- | Create VMs with subtask tracking
runVmSubtasks
  :: ServerState
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> [ApplyVm]
  -> [(SubtaskSpec, TaskId)]
  -> Bool
  -> TaskId
  -> Pool SqlBackend
  -> IO (Either Text [ApplyCreated])
runVmSubtasks _ _ _ _ [] _ _ _ _ = pure $ Right []
runVmSubtasks state keyMap diskMap nwMap vms subs skipExisting parentId pool = go vms subs []
  where
    go [] _ acc = pure $ Right $ reverse acc
    go (v : vs) ((_, subId) : restSubs) acc = do
      mExisting <-
        if skipExisting
          then resolveByName state UniqueName Map.empty (avName v)
          else pure Nothing
      case mExisting of
        Just _existingId -> do
          completeSubtask pool subId TaskSuccess (Just "Already exists") Nothing
          go vs restSubs acc
        Nothing -> do
          startSubtask pool subId
          result <- createOneVm state keyMap diskMap nwMap v
          case result of
            Left err -> do
              completeSubtask pool subId TaskError (Just err) Nothing
              cancelRemainingSubtasks pool parentId
              pure $ Left err
            Right vmId -> do
              completeSubtask pool subId TaskSuccess Nothing (Just (fromIntegral vmId))
              go vs restSubs (ApplyCreated (avName v) vmId : acc)
    go _ [] _ = pure $ Left "Internal error: subtask count mismatch"

--------------------------------------------------------------------------------
-- Resource creation primitives (shared by tracked path)
--------------------------------------------------------------------------------

createOneDisk :: ServerState -> Map.Map Text Int64 -> ApplyDisk -> IO (Either Text Int64)
createOneDisk state diskMap d = case (adImport d, adOverlay d, adClone d) of
  (Just importPath, _, _)
    | isHttpUrl importPath ->
        importDiskFromUrlIO state (adName d) importPath (adFormat d)
    | otherwise ->
        let format = fromMaybe FormatQcow2 (adFormat d <|> detectFormatFromPath importPath)
         in registerDiskIO state (adName d) importPath format
  (_, Just backingName, _) -> do
    mBackingId <- resolveByName state UniqueDiskImageName diskMap backingName
    case mBackingId of
      Nothing -> pure $ Left $ "backing disk '" <> backingName <> "' not found"
      Just backingId -> createOverlayDiskIO state (adName d) backingId (adSizeMb d) (adPath d)
  (_, _, Just cloneName) -> do
    mSourceId <- resolveByName state UniqueDiskImageName diskMap cloneName
    case mSourceId of
      Nothing -> pure $ Left $ "source disk '" <> cloneName <> "' not found"
      Just sourceId -> cloneDiskIO state (adName d) sourceId (adPath d)
  _ ->
    let format = fromMaybe FormatQcow2 (adFormat d)
        sizeMb = fromMaybe 10240 (adSizeMb d)
     in createDiskIO state (adName d) format sizeMb (adPath d)

createOneVm
  :: ServerState
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> ApplyVm
  -> IO (Either Text Int64)
createOneVm state keyMap diskMap nwMap v = do
  now <- getCurrentTime
  vmResult <-
    try $
      runSqlPool
        ( insert
            Vm
              { vmName = avName v
              , vmCreatedAt = now
              , vmStatus = VmStopped
              , vmCpuCount = avCpuCount v
              , vmRamMb = avRamMb v
              , vmDescription = avDescription v
              , vmPid = Nothing
              , vmHeadless = avHeadless v
              , vmGuestAgent = avGuestAgent v
              , vmCloudInit = effectiveCloudInit v
              , vmHealthcheck = Nothing
              }
        )
        (ssDbPool state)
  case vmResult of
    Left e -> pure $ Left $ "VM '" <> avName v <> "': " <> formatException e
    Right vmId -> createOneVmAttachments state keyMap diskMap nwMap v vmId

createOneVmAttachments
  :: ServerState
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> ApplyVm
  -> VmId
  -> IO (Either Text Int64)
createOneVmAttachments state keyMap diskMap nwMap v vmId = do
  -- Attach drives
  driveResult <- attachDrives state diskMap vmId (avDrives v) (avName v)
  case driveResult of
    Left err -> pure $ Left err
    Right () -> do
      -- Create network interfaces
      niResult <- createNetIfs state nwMap vmId (avNetworkInterfaces v) (avName v)
      case niResult of
        Left err -> pure $ Left err
        Right () -> do
          -- Create shared directories
          forM_ (avSharedDirs v) $ \sd ->
            runSqlPool
              ( insert_
                  SharedDir
                    { sharedDirVmId = vmId
                    , sharedDirPath = asdPath sd
                    , sharedDirTag = asdTag sd
                    , sharedDirCache = asdCache sd
                    , sharedDirReadOnly = asdReadOnly sd
                    , sharedDirPid = Nothing
                    }
              )
              (ssDbPool state)

          -- Attach SSH keys
          keyResult <- attachSshKeys state keyMap vmId (avSshKeys v) (avName v)
          case keyResult of
            Left err -> pure $ Left err
            Right () -> do
              -- Generate cloud-init ISO if cloud-init is enabled
              when (effectiveCloudInit v) $ do
                _ <- regenerateCloudInitIso (ssQemuConfig state) (ssDbPool state) (fromSqlKey vmId) (avName v) (ssLogLevel state) Nothing
                pure ()
              pure $ Right $ fromSqlKey vmId

attachDrives :: ServerState -> Map.Map Text Int64 -> VmId -> [ApplyDrive] -> Text -> IO (Either Text ())
attachDrives state diskMap vmId drives vmName = go drives
  where
    go [] = pure $ Right ()
    go (d : ds) = do
      mDiskId <- resolveByName state UniqueDiskImageName diskMap (adrDisk d)
      case mDiskId of
        Nothing -> pure $ Left $ "VM '" <> vmName <> "': disk '" <> adrDisk d <> "' not found"
        Just diskId -> do
          runSqlPool
            ( insert_
                Drive
                  { driveVmId = vmId
                  , driveDiskImageId = toSqlKey diskId
                  , driveInterface = adrInterface d
                  , driveMedia = adrMedia d
                  , driveReadOnly = adrReadOnly d
                  , driveCacheType = adrCacheType d
                  , driveDiscard = adrDiscard d
                  }
            )
            (ssDbPool state)
          go ds

createNetIfs :: ServerState -> Map.Map Text Int64 -> VmId -> [ApplyNetIf] -> Text -> IO (Either Text ())
createNetIfs state nwMap vmId netIfs vmName = go netIfs
  where
    go [] = pure $ Right ()
    go (ni : nis) = do
      mNetworkId <- case aniNetwork ni of
        Nothing -> pure $ Right Nothing
        Just nwName -> do
          mId <- resolveByName state UniqueNetworkName nwMap nwName
          case mId of
            Nothing -> pure $ Left $ "VM '" <> vmName <> "': network '" <> nwName <> "' not found"
            Just nid -> pure $ Right $ Just nid
      case mNetworkId of
        Left err -> pure $ Left err
        Right networkId -> do
          mac <- maybe generateMacAddress pure (aniMac ni)
          runSqlPool
            ( insert_
                NetworkInterface
                  { networkInterfaceVmId = vmId
                  , networkInterfaceInterfaceType = aniType ni
                  , networkInterfaceHostDevice = fromMaybe "" (aniHostDevice ni)
                  , networkInterfaceMacAddress = mac
                  , networkInterfaceNetworkId = fmap toSqlKey networkId
                  , networkInterfaceGuestIpAddresses = Nothing
                  }
            )
            (ssDbPool state)
          go nis

attachSshKeys :: ServerState -> Map.Map Text Int64 -> VmId -> [Text] -> Text -> IO (Either Text ())
attachSshKeys state keyMap vmId keyNames vmName = go keyNames
  where
    go [] = pure $ Right ()
    go (kn : kns) = do
      mKeyId <- resolveByName state UniqueSshKeyName keyMap kn
      case mKeyId of
        Nothing -> pure $ Left $ "VM '" <> vmName <> "': SSH key '" <> kn <> "' not found"
        Just keyId -> do
          runSqlPool (insert_ $ VmSshKey vmId (toSqlKey keyId)) (ssDbPool state)
          go kns

--------------------------------------------------------------------------------
-- Name Resolution
--------------------------------------------------------------------------------

-- | Format an exception into a user-friendly error message.
-- Strips common Haskell exception wrappers and SQL noise.
formatException :: SomeException -> Text
formatException e =
  let msg = T.pack (show e)
   in fromMaybe msg (extractDetail msg)
  where
    extractDetail msg
      | "already exists" `T.isInfixOf` T.toLower msg = Just "already exists"
      | "unique" `T.isInfixOf` T.toLower msg = Just "already exists (duplicate name)"
      | "violates unique constraint" `T.isInfixOf` T.toLower msg = Just "already exists (duplicate name)"
      | "violates foreign key" `T.isInfixOf` T.toLower msg = Just "referenced resource not found"
      | otherwise = Nothing

-- | Resolve a name to a DB ID. Checks in-config map first, then DB by unique constraint.
resolveByName
  :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend, ToBackendKey SqlBackend record)
  => ServerState
  -> (Text -> Unique record)
  -> Map.Map Text Int64
  -> Text
  -> IO (Maybe Int64)
resolveByName state mkUnique localMap name = case Map.lookup name localMap of
  Just rid -> pure $ Just rid
  Nothing -> do
    mEntity <- runSqlPool (getBy (mkUnique name)) (ssDbPool state)
    pure $ fmap (fromSqlKey . entityKey) mEntity
