{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Handler for applying declarative environment configurations from YAML.
-- Creates SSH keys, disks, networks, and VMs in dependency order.
module Corvus.Handlers.Apply
  ( -- * Action types
    ApplyAction (..)
  , ApplyDiskCreate (..)
  , ApplyVmCreate (..)

    -- * Handlers
  , handleApplyValidate
  , handleApplyExecute
  )
where

import Corvus.Action

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Handlers.CloudInit (RegenerateCloudInit (..))
import Corvus.Handlers.Disk (DiskClone (..), DiskCreate (..), DiskCreateOverlay (..), DiskImportAction (..), DiskRegister (..))
import Corvus.Handlers.Network (NetworkCreate (..))
import Corvus.Handlers.Resolve (validateName)
import Corvus.Handlers.SshKey (SshKeyCreate (..))
import Corvus.Handlers.Template (CloudInitConfigYaml (..))
import Corvus.Handlers.Vm (VmCreate (..))
import Corvus.Model
import Corvus.Protocol
import Corvus.Qemu.Image (detectFormatFromPath, isHttpUrl)
import Corvus.Types
import Corvus.Utils.Network (generateMacAddress)
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
  , adRegister :: Maybe Text
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
      <*> o .:? "register"

data ApplyNetwork = ApplyNetwork
  { anName :: Text
  , anSubnet :: Text
  , anDhcp :: Bool
  , anNat :: Bool
  , anAutostart :: Bool
  }
  deriving (Show)

instance FromJSON ApplyNetwork where
  parseJSON = withObject "ApplyNetwork" $ \o ->
    ApplyNetwork
      <$> o .: "name"
      <*> o .:? "subnet" .!= ""
      <*> o .:? "dhcp" .!= False
      <*> o .:? "nat" .!= False
      <*> o .:? "autostart" .!= False

data ApplyVm = ApplyVm
  { avName :: Text
  , avCpuCount :: Int
  , avRamMb :: Int
  , avDescription :: Maybe Text
  , avHeadless :: Bool
  , avGuestAgent :: Bool
  , avCloudInit :: Maybe Bool
  , avCloudInitConfig :: Maybe CloudInitConfigYaml
  , avDrives :: [ApplyDrive]
  , avNetworkInterfaces :: [ApplyNetIf]
  , avSharedDirs :: [ApplySharedDir]
  , avSshKeys :: [Text]
  , avAutostart :: Bool
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
      <*> o .:? "cloudInitConfig"
      <*> o .:? "drives" .!= []
      <*> o .:? "networkInterfaces" .!= []
      <*> o .:? "sharedDirs" .!= []
      <*> o .:? "sshKeys" .!= []
      <*> o .:? "autostart" .!= False

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
-- Each resource creation is dispatched as an Action subtask via runActionAsSubtask.
handleApplyExecute :: ServerState -> ApplyConfig -> Bool -> TaskId -> IO Response
handleApplyExecute state config skipExisting parentTaskId = runServerLogging state $ do
  logInfoN "Applying environment configuration..."
  result <- liftIO $ executeApply state config skipExisting parentTaskId
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
          hasRegister = isJust (adRegister d)
          hasCreate = isJust (adFormat d) && isJust (adSizeMb d) && not hasImport && not hasOverlay && not hasClone && not hasRegister
          strategies = length $ filter id [hasImport, hasOverlay, hasClone, hasRegister]
       in if strategies > 1
            then Left $ "Disk '" <> adName d <> "': cannot specify more than one of 'import', 'overlay', 'clone', 'register'"
            else
              if not hasImport && not hasOverlay && not hasClone && not hasRegister && not hasCreate
                then Left $ "Disk '" <> adName d <> "': must specify 'import', 'overlay', 'clone', 'register', or both 'format' and 'sizeMb'"
                else
                  if isJust (adPath d) && not hasOverlay && not hasClone && not hasCreate && not hasImport
                    then Left $ "Disk '" <> adName d <> "': 'path' can only be used with 'import', 'overlay', 'clone', or 'create'"
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

-- | Execute apply: create resources in dependency order using Action subtasks.
executeApply :: ServerState -> ApplyConfig -> Bool -> TaskId -> IO (Either Text ApplyResult)
executeApply state config skipExisting parentId = do
  -- Phase 1: SSH keys
  keyResult <- runSequentialCreate (acSshKeys config) Map.empty $ \k _keyMap -> do
    mExisting <- if skipExisting then resolveByName state UniqueSshKeyName Map.empty (askName k) else pure Nothing
    case mExisting of
      Just eid -> pure $ Right (askName k, eid)
      Nothing -> do
        resp <- runActionAsSubtask state (SshKeyCreate (askName k) (askPublicKey k)) parentId
        extractCreatedResult (askName k) resp
  case keyResult of
    Left err -> pure $ Left err
    Right (keyMap, keyCreated) -> do
      -- Phase 2: Disks (need accumulating map for overlay/clone references)
      diskResult <- runSequentialCreate (acDisks config) Map.empty $ \d diskMap -> do
        mExisting <- if skipExisting then resolveByName state UniqueDiskImageName diskMap (adName d) else pure Nothing
        case mExisting of
          Just eid -> pure $ Right (adName d, eid)
          Nothing -> do
            resp <- runActionAsSubtask state (ApplyDiskCreate d diskMap) parentId
            extractCreatedResult (adName d) resp
      case diskResult of
        Left err -> pure $ Left err
        Right (diskMap, diskCreated) -> do
          -- Phase 3: Networks
          nwResult <- runSequentialCreate (acNetworks config) Map.empty $ \n _nwMap -> do
            mExisting <- if skipExisting then resolveByName state UniqueNetworkName Map.empty (anName n) else pure Nothing
            case mExisting of
              Just eid -> pure $ Right (anName n, eid)
              Nothing -> do
                resp <- runActionAsSubtask state (NetworkCreate (anName n) (anSubnet n) (anDhcp n) (anNat n) (anAutostart n)) parentId
                extractCreatedResult (anName n) resp
          case nwResult of
            Left err -> pure $ Left err
            Right (nwMap, nwCreated) -> do
              -- Phase 4: VMs
              vmResult <- runSequentialVms (acVms config) keyMap diskMap nwMap
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
  where
    -- Run items sequentially, building a name→ID map.
    runSequentialCreate
      :: [a]
      -> Map.Map Text Int64
      -> (a -> Map.Map Text Int64 -> IO (Either Text (Text, Int64)))
      -> IO (Either Text (Map.Map Text Int64, [ApplyCreated]))
    runSequentialCreate items initMap action = go items initMap []
      where
        go [] m acc = pure $ Right (m, reverse acc)
        go (item : rest) m acc = do
          result <- action item m
          case result of
            Left err -> pure $ Left err
            Right (name, rid) ->
              go rest (Map.insert name rid m) (ApplyCreated name rid : acc)

    -- Run VM creation sequentially (VMs don't need a name map)
    runSequentialVms
      :: [ApplyVm]
      -> Map.Map Text Int64
      -> Map.Map Text Int64
      -> Map.Map Text Int64
      -> IO (Either Text [ApplyCreated])
    runSequentialVms vms keyMap diskMap nwMap = go vms []
      where
        go [] acc = pure $ Right $ reverse acc
        go (v : vs) acc = do
          mExisting <- if skipExisting then resolveByName state UniqueName Map.empty (avName v) else pure Nothing
          case mExisting of
            Just _existingId -> go vs acc
            Nothing -> do
              resp <- runActionAsSubtask state (ApplyVmCreate keyMap diskMap nwMap v) parentId
              case resp of
                RespVmCreated vmId -> go vs (ApplyCreated (avName v) vmId : acc)
                RespError err -> pure $ Left $ "VM '" <> avName v <> "': " <> err
                _ -> pure $ Left $ "VM '" <> avName v <> "': unexpected response"

    -- Extract entity ID from a creation response.
    extractCreatedResult :: Text -> Response -> IO (Either Text (Text, Int64))
    extractCreatedResult name resp =
      let (result, msg) = classifyResponse resp
          (mId, _) = extractEntityFromResponse resp
       in case result of
            TaskSuccess -> case mId of
              Just eid -> pure $ Right (name, fromIntegral eid)
              Nothing -> pure $ Left $ name <> ": succeeded but no entity ID"
            _ -> pure $ Left $ name <> ": " <> fromMaybe "unknown error" msg

--------------------------------------------------------------------------------
-- Resource creation primitives (shared by tracked path)
--------------------------------------------------------------------------------

createOneVm
  :: ServerState
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> ApplyVm
  -> IO (Either Text Int64)
createOneVm state keyMap diskMap nwMap v = do
  -- Use a dummy TaskId since VM creation doesn't create subtasks
  let dummyTaskId = toSqlKey 0
  vmResult <-
    executeCreate
      state
      ( VmCreate
          (avName v)
          (avCpuCount v)
          (avRamMb v)
          (avDescription v)
          (avHeadless v)
          (avGuestAgent v)
          (effectiveCloudInit v)
          (avAutostart v)
      )
      dummyTaskId
  case vmResult of
    Left err -> pure $ Left $ "VM '" <> avName v <> "': " <> err
    Right vmId -> createOneVmAttachments state keyMap diskMap nwMap v (toSqlKey vmId)

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
              -- Insert cloud-init config if provided
              forM_ (avCloudInitConfig v) $ \cic ->
                runSqlPool
                  ( insert_ $
                      CloudInit
                        vmId
                        (cicyUserData cic)
                        (cicyNetworkConfig cic)
                        (cicyInjectSshKeys cic)
                  )
                  (ssDbPool state)
              -- Generate cloud-init ISO if cloud-init is enabled
              when (effectiveCloudInit v) $ do
                _ <- runAction state (RegenerateCloudInit (fromSqlKey vmId) (avName v))
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

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

data ApplyAction = ApplyAction
  { aaConfig :: ApplyConfig
  , aaSkipExisting :: Bool
  }

instance Action ApplyAction where
  actionSubsystem _ = SubApply
  actionCommand _ = "apply"
  actionExecute ctx a = handleApplyExecute (acState ctx) (aaConfig a) (aaSkipExisting a) (acTaskId ctx)

-- | Apply-specific disk creation that handles overlay/clone name resolution.
data ApplyDiskCreate = ApplyDiskCreate
  { adcConfig :: ApplyDisk
  , adcDiskMap :: Map.Map Text Int64
  }

instance Action ApplyDiskCreate where
  actionSubsystem _ = SubDisk
  actionCommand _ = "create"
  actionEntityName = Just . adName . adcConfig
  actionExecute ctx a =
    let d = adcConfig a
        state = acState ctx
     in case (adImport d, adOverlay d, adClone d, adRegister d) of
          (Just importPath, _, _, _) ->
            actionExecute ctx (DiskImportAction (adName d) importPath (adPath d) (fmap enumToText (adFormat d)))
          (_, _, _, Just registerPath)
            | isHttpUrl registerPath -> pure $ RespError $ "Disk '" <> adName d <> "': register requires a local path, not a URL"
            | otherwise ->
                let format = fromMaybe FormatQcow2 (adFormat d <|> detectFormatFromPath registerPath)
                 in actionExecute ctx (DiskRegister (adName d) registerPath (Just format))
          (_, Just backingName, _, _) -> do
            mBackingId <- resolveByName state UniqueDiskImageName (adcDiskMap a) backingName
            case mBackingId of
              Nothing -> pure $ RespError $ "backing disk '" <> backingName <> "' not found"
              Just backingId -> actionExecute ctx (DiskCreateOverlay (adName d) backingId (adSizeMb d) (adPath d))
          (_, _, Just cloneName, _) -> do
            mSourceId <- resolveByName state UniqueDiskImageName (adcDiskMap a) cloneName
            case mSourceId of
              Nothing -> pure $ RespError $ "source disk '" <> cloneName <> "' not found"
              Just sourceId -> actionExecute ctx (DiskClone (adName d) sourceId Nothing (adPath d))
          _ ->
            let format = fromMaybe FormatQcow2 (adFormat d)
                sizeMb = fromMaybe 10240 (adSizeMb d)
             in actionExecute ctx (DiskCreate (adName d) format (fromIntegral sizeMb) (adPath d))

-- | Apply-specific VM creation with attachments (drives, netifs, SSH keys, cloud-init).
data ApplyVmCreate = ApplyVmCreate
  { avcKeyMap :: Map.Map Text Int64
  , avcDiskMap :: Map.Map Text Int64
  , avcNwMap :: Map.Map Text Int64
  , avcVm :: ApplyVm
  }

instance Action ApplyVmCreate where
  actionSubsystem _ = SubVm
  actionCommand _ = "create"
  actionEntityName = Just . avName . avcVm
  actionExecute ctx a = do
    result <- createOneVm (acState ctx) (avcKeyMap a) (avcDiskMap a) (avcNwMap a) (avcVm a)
    case result of
      Left err -> pure $ RespError err
      Right vmId -> pure $ RespVmCreated vmId
