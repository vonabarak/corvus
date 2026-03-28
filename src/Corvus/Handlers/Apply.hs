{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Handler for applying declarative environment configurations from YAML.
-- Creates SSH keys, disks, networks, and VMs in dependency order.
module Corvus.Handlers.Apply
  ( handleApply
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Handlers.Disk (makeRelativeToBase, sanitizeDiskName)
import Corvus.Handlers.SshKey (regenerateCloudInitIso)
import Corvus.Model
import Corvus.Protocol
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Qemu.Image
  ( ImageResult (..)
  , createImage
  , createOverlay
  , decompressXz
  , detectFormatFromUrl
  , downloadImage
  , isHttpUrl
  , resizeImage
  )
import Corvus.Types
import Data.Int (Int64)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.Yaml (FromJSON (..), decodeEither', withObject, (.!=), (.:), (.:?))
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import System.FilePath (takeExtension, (</>))
import System.Random (randomRIO)
import Text.Printf (printf)

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
      -- Validate config
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
  -- Check for duplicate names within each section
  checkDuplicates "SSH key" $ map askName (acSshKeys config)
  checkDuplicates "disk" $ map adName (acDisks config)
  checkDuplicates "network" $ map anName (acNetworks config)
  checkDuplicates "VM" $ map avName (acVms config)

  -- Validate disk definitions (mutually exclusive fields)
  forM_ (acDisks config) validateDisk
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

--------------------------------------------------------------------------------
-- Execution
--------------------------------------------------------------------------------

executeApply :: ServerState -> ApplyConfig -> IO (Either Text ApplyResult)
executeApply state config = do
  -- Phase 1: Create SSH keys
  keyResult <- createSshKeys state (acSshKeys config)
  case keyResult of
    Left err -> pure $ Left err
    Right (keyMap, keyCreated) -> do
      -- Phase 2: Create disks
      diskResult <- createDisks state (acDisks config)
      case diskResult of
        Left err -> pure $ Left err
        Right (diskMap, diskCreated) -> do
          -- Phase 3: Create networks
          nwResult <- createNetworks state (acNetworks config)
          case nwResult of
            Left err -> pure $ Left err
            Right (nwMap, nwCreated) -> do
              -- Phase 4: Create VMs
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
        Left err -> pure $ Left err
        Right diskId ->
          go ds (Map.insert (adName d) diskId m) (ApplyCreated (adName d) diskId : acc)

createOneDisk :: ServerState -> Map.Map Text Int64 -> ApplyDisk -> IO (Either Text Int64)
createOneDisk state diskMap d = case (adImport d, adOverlay d) of
  (Just importPath, _) -> createDiskImport state d importPath
  (_, Just backingName) -> createDiskOverlay state diskMap d backingName
  _ -> createDiskNew state d

-- | Import a disk (local path or URL)
createDiskImport :: ServerState -> ApplyDisk -> Text -> IO (Either Text Int64)
createDiskImport state d importPath = do
  case sanitizeDiskName (adName d) of
    Left err -> pure $ Left $ "Disk '" <> adName d <> "': " <> err
    Right safeName -> do
      basePath <- getEffectiveBasePath (ssQemuConfig state)
      if isHttpUrl importPath
        then do
          -- Download from URL
          let mFmt = adFormat d <|> detectFormatFromUrl importPath
          case mFmt of
            Nothing -> pure $ Left $ "Disk '" <> adName d <> "': cannot detect format from URL"
            Just format -> do
              let urlStr = T.unpack importPath
                  isXz = ".xz" `isSuffixOf` urlStr
                  fmtExt = T.unpack (enumToText format)
                  downloadFileName = T.unpack safeName <> "." <> fmtExt <> if isXz then ".xz" else ""
                  downloadPath = basePath </> downloadFileName
                  finalFileName = T.unpack safeName <> "." <> fmtExt
                  finalPath = basePath </> finalFileName
              dlResult <- downloadImage downloadPath importPath
              case dlResult of
                ImageError err -> pure $ Left $ "Disk '" <> adName d <> "' download failed: " <> err
                _ -> do
                  actualPath <-
                    if isXz
                      then decompressXz downloadPath
                      else pure $ Right finalPath
                  case actualPath of
                    Left err -> pure $ Left $ "Disk '" <> adName d <> "' decompression failed: " <> err
                    Right diskPath -> do
                      let storedPath = makeRelativeToBase basePath diskPath
                      now <- getCurrentTime
                      diskId <-
                        runSqlPool
                          ( insert
                              DiskImage
                                { diskImageName = safeName
                                , diskImageFilePath = storedPath
                                , diskImageFormat = format
                                , diskImageSizeMb = Nothing
                                , diskImageCreatedAt = now
                                , diskImageBackingImageId = Nothing
                                }
                          )
                          (ssDbPool state)
                      pure $ Right $ fromSqlKey diskId
        else do
          -- Local path: register directly
          let format = fromMaybe FormatQcow2 (adFormat d <|> detectFormatFromPath importPath)
              storedPath = makeRelativeToBase basePath (T.unpack importPath)
          now <- getCurrentTime
          diskId <-
            runSqlPool
              ( insert
                  DiskImage
                    { diskImageName = safeName
                    , diskImageFilePath = storedPath
                    , diskImageFormat = format
                    , diskImageSizeMb = fmap fromIntegral (adSizeMb d)
                    , diskImageCreatedAt = now
                    , diskImageBackingImageId = Nothing
                    }
              )
              (ssDbPool state)
          pure $ Right $ fromSqlKey diskId

-- | Create an overlay disk backed by another disk
createDiskOverlay :: ServerState -> Map.Map Text Int64 -> ApplyDisk -> Text -> IO (Either Text Int64)
createDiskOverlay state diskMap d backingName = do
  -- Resolve backing disk: check in-config map first, then DB
  mBackingId <- case Map.lookup backingName diskMap of
    Just bid -> pure $ Just bid
    Nothing -> do
      mEntity <- runSqlPool (getBy (UniqueDiskImageName backingName)) (ssDbPool state)
      pure $ fmap (fromSqlKey . entityKey) mEntity

  case mBackingId of
    Nothing -> pure $ Left $ "Disk '" <> adName d <> "': backing disk '" <> backingName <> "' not found"
    Just backingId -> do
      case sanitizeDiskName (adName d) of
        Left err -> pure $ Left $ "Disk '" <> adName d <> "': " <> err
        Right safeName -> do
          mBackingDisk <- runSqlPool (get (toSqlKey backingId :: DiskImageId)) (ssDbPool state)
          case mBackingDisk of
            Nothing -> pure $ Left $ "Disk '" <> adName d <> "': backing disk disappeared"
            Just backingDisk -> do
              basePath <- getEffectiveBasePath (ssQemuConfig state)
              let fileName = T.unpack safeName <> ".qcow2"
                  overlayPath = basePath </> fileName

              -- Resolve backing disk's actual file path
              let backingFilePath = T.unpack $ diskImageFilePath backingDisk
                  backingAbsPath =
                    if "/" `isPrefixOf` backingFilePath
                      then backingFilePath
                      else basePath </> backingFilePath

              result <- createOverlay overlayPath backingAbsPath (diskImageFormat backingDisk)
              case result of
                ImageError err -> pure $ Left $ "Disk '" <> adName d <> "' overlay failed: " <> err
                ImageFormatNotSupported msg -> pure $ Left msg
                ImageNotFound -> pure $ Left $ "Disk '" <> adName d <> "': backing image file not found"
                ImageSuccess -> do
                  -- Resize if requested
                  case adSizeMb d of
                    Just newSize -> do
                      _ <- resizeImage overlayPath (fromIntegral newSize)
                      pure ()
                    Nothing -> pure ()

                  now <- getCurrentTime
                  let storedPath = makeRelativeToBase basePath overlayPath
                  diskId <-
                    runSqlPool
                      ( insert
                          DiskImage
                            { diskImageName = safeName
                            , diskImageFilePath = storedPath
                            , diskImageFormat = FormatQcow2
                            , diskImageSizeMb = adSizeMb d <|> diskImageSizeMb backingDisk
                            , diskImageCreatedAt = now
                            , diskImageBackingImageId = Just (toSqlKey backingId)
                            }
                      )
                      (ssDbPool state)
                  pure $ Right $ fromSqlKey diskId
  where
    isPrefixOf p s = T.pack p `T.isPrefixOf` T.pack s

-- | Create a new empty disk
createDiskNew :: ServerState -> ApplyDisk -> IO (Either Text Int64)
createDiskNew state d = do
  case sanitizeDiskName (adName d) of
    Left err -> pure $ Left $ "Disk '" <> adName d <> "': " <> err
    Right safeName -> do
      let format = fromMaybe FormatQcow2 (adFormat d)
          sizeMb = fromMaybe 10240 (adSizeMb d)
      basePath <- getEffectiveBasePath (ssQemuConfig state)
      let fileName = T.unpack safeName <> "." <> T.unpack (enumToText format)
          filePath = basePath </> fileName
      result <- createImage filePath format (fromIntegral sizeMb)
      case result of
        ImageError err -> pure $ Left $ "Disk '" <> adName d <> "' create failed: " <> err
        _ -> do
          now <- getCurrentTime
          diskId <-
            runSqlPool
              ( insert
                  DiskImage
                    { diskImageName = safeName
                    , diskImageFilePath = makeRelativeToBase basePath filePath
                    , diskImageFormat = format
                    , diskImageSizeMb = Just sizeMb
                    , diskImageCreatedAt = now
                    , diskImageBackingImageId = Nothing
                    }
              )
              (ssDbPool state)
          pure $ Right $ fromSqlKey diskId

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

  -- Insert VM record
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
              -- Generate cloud-init ISO
              _ <- regenerateCloudInitIso (ssQemuConfig state) (ssDbPool state) (fromSqlKey vmId) (avName v) (ssLogLevel state)
              pure $ Right $ fromSqlKey vmId

attachDrives :: ServerState -> Map.Map Text Int64 -> VmId -> [ApplyDrive] -> Text -> IO (Either Text ())
attachDrives state diskMap vmId drives vmName = go drives
  where
    go [] = pure $ Right ()
    go (d : ds) = do
      mDiskId <- resolveDiskId state diskMap (adrDisk d)
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
      -- Resolve network if specified
      mNetworkId <- case aniNetwork ni of
        Nothing -> pure $ Right Nothing
        Just nwName -> do
          mId <- resolveNetworkId state nwMap nwName
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
      mKeyId <- resolveKeyId state keyMap kn
      case mKeyId of
        Nothing -> pure $ Left $ "VM '" <> vmName <> "': SSH key '" <> kn <> "' not found"
        Just keyId -> do
          runSqlPool (insert_ $ VmSshKey vmId (toSqlKey keyId)) (ssDbPool state)
          go kns

--------------------------------------------------------------------------------
-- Name Resolution Helpers
--------------------------------------------------------------------------------

-- | Resolve a disk name to a DB ID. Checks in-config map first, then DB.
resolveDiskId :: ServerState -> Map.Map Text Int64 -> Text -> IO (Maybe Int64)
resolveDiskId state diskMap name = case Map.lookup name diskMap of
  Just did -> pure $ Just did
  Nothing -> do
    mEntity <- runSqlPool (getBy (UniqueDiskImageName name)) (ssDbPool state)
    pure $ fmap (fromSqlKey . entityKey) mEntity

-- | Resolve a network name to a DB ID. Checks in-config map first, then DB.
resolveNetworkId :: ServerState -> Map.Map Text Int64 -> Text -> IO (Maybe Int64)
resolveNetworkId state nwMap name = case Map.lookup name nwMap of
  Just nid -> pure $ Just nid
  Nothing -> do
    mEntity <- runSqlPool (getBy (UniqueNetworkName name)) (ssDbPool state)
    pure $ fmap (fromSqlKey . entityKey) mEntity

-- | Resolve an SSH key name to a DB ID. Checks in-config map first, then DB.
resolveKeyId :: ServerState -> Map.Map Text Int64 -> Text -> IO (Maybe Int64)
resolveKeyId state keyMap name = case Map.lookup name keyMap of
  Just kid -> pure $ Just kid
  Nothing -> do
    mEntity <- runSqlPool (getBy (UniqueSshKeyName name)) (ssDbPool state)
    pure $ fmap (fromSqlKey . entityKey) mEntity

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Generate a random MAC address with QEMU OUI (52:54:00)
generateMacAddress :: IO Text
generateMacAddress = do
  [b1, b2, b3] <- replicateM 3 (randomRIO (0, 255 :: Int))
  let mac = T.pack $ printf "52:54:00:%02x:%02x:%02x" b1 b2 b3
  pure mac

-- | Detect disk format from a local file path extension
detectFormatFromPath :: Text -> Maybe DriveFormat
detectFormatFromPath path =
  case takeExtension (T.unpack path) of
    ".qcow2" -> Just FormatQcow2
    ".raw" -> Just FormatRaw
    ".img" -> Just FormatRaw
    ".vmdk" -> Just FormatVmdk
    ".vdi" -> Just FormatVdi
    _ -> Nothing
