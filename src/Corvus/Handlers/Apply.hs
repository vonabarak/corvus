{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handler for applying declarative environment configurations from YAML.
-- Creates SSH keys, disks, networks, and VMs in dependency order.
module Corvus.Handlers.Apply
  ( handleApply
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
import Corvus.Model
import Corvus.Protocol
import Corvus.Qemu.Image (isHttpUrl)
import Corvus.Types
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Yaml (FromJSON (..), decodeEither', withObject, (.!=), (.:), (.:?))
import Database.Persist
import Database.Persist.Postgresql (SqlBackend, runSqlPool)

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
  }
  deriving (Show)

instance FromJSON ApplyNetwork where
  parseJSON = withObject "ApplyNetwork" $ \o ->
    ApplyNetwork
      <$> o .: "name"
      <*> o .:? "subnet" .!= ""
      <*> o .:? "dhcp" .!= False

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

handleApply :: ServerState -> Text -> Bool -> IO Response
handleApply state yamlContent skipExisting = runServerLogging state $ do
  case decodeEither' (TE.encodeUtf8 yamlContent) of
    Left err -> do
      let msg = T.pack $ show err
      logWarnN $ "Failed to parse apply config YAML: " <> msg
      pure $ RespError msg
    Right config -> do
      case validateConfig config of
        Left err -> do
          logWarnN $ "Config validation failed: " <> err
          pure $ RespError err
        Right () -> do
          logInfoN "Applying environment configuration..."
          result <- liftIO $ executeApply state config skipExisting
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

executeApply :: ServerState -> ApplyConfig -> Bool -> IO (Either Text ApplyResult)
executeApply state config skipExisting = do
  keyResult <- createSshKeys state (acSshKeys config) skipExisting
  case keyResult of
    Left err -> pure $ Left err
    Right (keyMap, keyCreated) -> do
      diskResult <- createDisks state (acDisks config) skipExisting
      case diskResult of
        Left err -> pure $ Left err
        Right (diskMap, diskCreated) -> do
          nwResult <- createNetworks state (acNetworks config) skipExisting
          case nwResult of
            Left err -> pure $ Left err
            Right (nwMap, nwCreated) -> do
              vmResult <- createVms state keyMap diskMap nwMap (acVms config) skipExisting
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

-- | Create SSH keys, return map of name -> DB key ID
createSshKeys :: ServerState -> [ApplySshKey] -> Bool -> IO (Either Text (Map.Map Text Int64, [ApplyCreated]))
createSshKeys state keys skipExisting = go keys Map.empty []
  where
    go [] m acc = pure $ Right (m, reverse acc)
    go (k : ks) m acc = do
      mExisting <-
        if skipExisting
          then resolveByName state UniqueSshKeyName Map.empty (askName k)
          else pure Nothing
      case mExisting of
        Just existingId ->
          go ks (Map.insert (askName k) existingId m) acc
        Nothing -> do
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
            Left e -> pure $ Left $ "SSH key '" <> askName k <> "': " <> formatException e
            Right keyEntity -> do
              let keyId = fromSqlKey keyEntity
              go ks (Map.insert (askName k) keyId m) (ApplyCreated (askName k) keyId : acc)

-- | Create disks in listed order, return map of name -> DB disk ID
createDisks :: ServerState -> [ApplyDisk] -> Bool -> IO (Either Text (Map.Map Text Int64, [ApplyCreated]))
createDisks state disks skipExisting = go disks Map.empty []
  where
    go [] m acc = pure $ Right (m, reverse acc)
    go (d : ds) m acc = do
      mExisting <-
        if skipExisting
          then resolveByName state UniqueDiskImageName Map.empty (adName d)
          else pure Nothing
      case mExisting of
        Just existingId ->
          go ds (Map.insert (adName d) existingId m) acc
        Nothing -> do
          result <- createOneDisk state m d
          case result of
            Left err -> pure $ Left $ "Disk '" <> adName d <> "': " <> err
            Right diskId ->
              go ds (Map.insert (adName d) diskId m) (ApplyCreated (adName d) diskId : acc)

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

-- | Create networks, return map of name -> DB network ID
createNetworks :: ServerState -> [ApplyNetwork] -> Bool -> IO (Either Text (Map.Map Text Int64, [ApplyCreated]))
createNetworks state networks skipExisting = go networks Map.empty []
  where
    go [] m acc = pure $ Right (m, reverse acc)
    go (n : ns) m acc = do
      mExisting <-
        if skipExisting
          then resolveByName state UniqueNetworkName Map.empty (anName n)
          else pure Nothing
      case mExisting of
        Just existingId ->
          go ns (Map.insert (anName n) existingId m) acc
        Nothing -> do
          now <- getCurrentTime
          result <-
            try $
              runSqlPool
                ( insert
                    Network
                      { networkName = anName n
                      , networkSubnet = anSubnet n
                      , networkDhcp = anDhcp n
                      , networkRunning = False
                      , networkDnsmasqPid = Nothing
                      , networkCreatedAt = now
                      }
                )
                (ssDbPool state)
          case result of
            Left e -> pure $ Left $ "Network '" <> anName n <> "': " <> formatException e
            Right nwId -> do
              let nid = fromSqlKey nwId
              go ns (Map.insert (anName n) nid m) (ApplyCreated (anName n) nid : acc)

-- | Create VMs with all their attachments
createVms
  :: ServerState
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> [ApplyVm]
  -> Bool
  -> IO (Either Text [ApplyCreated])
createVms state keyMap diskMap nwMap vms skipExisting = go vms []
  where
    go [] acc = pure $ Right $ reverse acc
    go (v : vs) acc = do
      mExisting <-
        if skipExisting
          then resolveByName state UniqueName Map.empty (avName v)
          else pure Nothing
      case mExisting of
        Just existingId ->
          go vs acc
        Nothing -> do
          result <- createOneVm state keyMap diskMap nwMap v
          case result of
            Left err -> pure $ Left err
            Right vmId -> go vs (ApplyCreated (avName v) vmId : acc)

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
              -- Generate cloud-init ISO only if cloud-init enabled and SSH keys present
              when (effectiveCloudInit v && not (null (avSshKeys v))) $ do
                _ <- regenerateCloudInitIso (ssQemuConfig state) (ssDbPool state) (fromSqlKey vmId) (avName v) (ssLogLevel state)
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
