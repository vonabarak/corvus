{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Handler for applying declarative environment configurations from YAML.
-- Creates SSH keys, disks, networks, and VMs in dependency order.
module Corvus.Handlers.Apply
  ( handleApply
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Handlers.Disk
  ( createDiskIO
  , createOverlayDiskIO
  , detectFormatFromPath
  , importDiskFromUrlIO
  , registerDiskIO
  )
import Corvus.Handlers.NetIf (generateMacAddress)
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

data ApplyDisk = ApplyDisk
  { adName :: Text
  , adFormat :: Maybe DriveFormat
  , adSizeMb :: Maybe Int
  , adImport :: Maybe Text
  , adOverlay :: Maybe Text
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

data ApplyNetwork = ApplyNetwork
  { anName :: Text
  , anSubnet :: Text
  }
  deriving (Show)

instance FromJSON ApplyNetwork where
  parseJSON = withObject "ApplyNetwork" $ \o ->
    ApplyNetwork
      <$> o .: "name"
      <*> o .: "subnet"

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
      <*> o .:? "cacheType" .!= CacheNone
      <*> o .:? "discard" .!= False

data ApplyNetIf = ApplyNetIf
  { aniType :: NetInterfaceType
  , aniHostDevice :: Maybe Text
  , aniNetwork :: Maybe Text
  }
  deriving (Show)

instance FromJSON ApplyNetIf where
  parseJSON = withObject "ApplyNetIf" $ \o ->
    ApplyNetIf
      <$> o .: "type"
      <*> o .:? "hostDevice"
      <*> o .:? "network"

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

handleApply :: ServerState -> Text -> IO Response
handleApply state yamlContent = runServerLogging state $ do
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
          result <- liftIO $ executeApply state config
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
          hasCreate = isJust (adFormat d) && isJust (adSizeMb d) && not hasImport && not hasOverlay
       in if hasImport && hasOverlay
            then Left $ "Disk '" <> adName d <> "': cannot specify both 'import' and 'overlay'"
            else
              if not hasImport && not hasOverlay && not hasCreate
                then Left $ "Disk '" <> adName d <> "': must specify 'import', 'overlay', or both 'format' and 'sizeMb'"
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

executeApply :: ServerState -> ApplyConfig -> IO (Either Text ApplyResult)
executeApply state config = do
  keyResult <- createSshKeys state (acSshKeys config)
  case keyResult of
    Left err -> pure $ Left err
    Right (keyMap, keyCreated) -> do
      diskResult <- createDisks state (acDisks config)
      case diskResult of
        Left err -> pure $ Left err
        Right (diskMap, diskCreated) -> do
          nwResult <- createNetworks state (acNetworks config)
          case nwResult of
            Left err -> pure $ Left err
            Right (nwMap, nwCreated) -> do
              vmResult <- createVms state keyMap diskMap nwMap (acVms config)
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
createSshKeys :: ServerState -> [ApplySshKey] -> IO (Either Text (Map.Map Text Int64, [ApplyCreated]))
createSshKeys state keys = go keys Map.empty []
  where
    go [] m acc = pure $ Right (m, reverse acc)
    go (k : ks) m acc = do
      now <- getCurrentTime
      result <-
        runSqlPool
          ( insert
              SshKey
                { sshKeyName = askName k
                , sshKeyPublicKey = askPublicKey k
                , sshKeyCreatedAt = now
                }
          )
          (ssDbPool state)
      let keyId = fromSqlKey result
      go ks (Map.insert (askName k) keyId m) (ApplyCreated (askName k) keyId : acc)

-- | Create disks in listed order, return map of name -> DB disk ID
createDisks :: ServerState -> [ApplyDisk] -> IO (Either Text (Map.Map Text Int64, [ApplyCreated]))
createDisks state disks = go disks Map.empty []
  where
    go [] m acc = pure $ Right (m, reverse acc)
    go (d : ds) m acc = do
      result <- createOneDisk state m d
      case result of
        Left err -> pure $ Left $ "Disk '" <> adName d <> "': " <> err
        Right diskId ->
          go ds (Map.insert (adName d) diskId m) (ApplyCreated (adName d) diskId : acc)

createOneDisk :: ServerState -> Map.Map Text Int64 -> ApplyDisk -> IO (Either Text Int64)
createOneDisk state diskMap d = case (adImport d, adOverlay d) of
  (Just importPath, _)
    | isHttpUrl importPath ->
        importDiskFromUrlIO state (adName d) importPath (adFormat d)
    | otherwise ->
        let format = fromMaybe FormatQcow2 (adFormat d <|> detectFormatFromPath importPath)
         in registerDiskIO state (adName d) importPath format (adSizeMb d)
  (_, Just backingName) -> do
    mBackingId <- resolveByName state UniqueDiskImageName diskMap backingName
    case mBackingId of
      Nothing -> pure $ Left $ "backing disk '" <> backingName <> "' not found"
      Just backingId -> createOverlayDiskIO state (adName d) backingId (adSizeMb d)
  _ ->
    let format = fromMaybe FormatQcow2 (adFormat d)
        sizeMb = fromMaybe 10240 (adSizeMb d)
     in createDiskIO state (adName d) format sizeMb

-- | Create networks, return map of name -> DB network ID
createNetworks :: ServerState -> [ApplyNetwork] -> IO (Either Text (Map.Map Text Int64, [ApplyCreated]))
createNetworks state networks = go networks Map.empty []
  where
    go [] m acc = pure $ Right (m, reverse acc)
    go (n : ns) m acc = do
      now <- getCurrentTime
      nwId <-
        runSqlPool
          ( insert
              Network
                { networkName = anName n
                , networkSubnet = anSubnet n
                , networkVdeSwitchPid = Nothing
                , networkDnsmasqPid = Nothing
                , networkCreatedAt = now
                }
          )
          (ssDbPool state)
      let nid = fromSqlKey nwId
      go ns (Map.insert (anName n) nid m) (ApplyCreated (anName n) nid : acc)

-- | Create VMs with all their attachments
createVms
  :: ServerState
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> Map.Map Text Int64
  -> [ApplyVm]
  -> IO (Either Text [ApplyCreated])
createVms state keyMap diskMap nwMap vms = go vms []
  where
    go [] acc = pure $ Right $ reverse acc
    go (v : vs) acc = do
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
  vmId <-
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
          mac <- generateMacAddress
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
