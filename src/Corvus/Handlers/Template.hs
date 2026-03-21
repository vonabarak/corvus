{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Corvus.Handlers.Template
  ( handleTemplateCreate,
    handleTemplateList,
    handleTemplateShow,
    handleTemplateDelete,
    handleTemplateInstantiate,
  )
where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN, runStdoutLoggingT)
import Corvus.Handlers.Disk (sanitizeDiskName, resolveDiskPath, getRunningAttachedVms)
import Corvus.Handlers.SshKey (regenerateCloudInitIso)
import Corvus.Model
import Corvus.Protocol
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Qemu.Image (cloneImage, createOverlay, resizeImage, ImageResult (..))
import Corvus.Types
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (getCurrentTime, UTCTime)
import Data.Yaml (FromJSON (..), decodeEither', withObject, (.:), (.:?), (.!=))
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlPersistT)
import GHC.Generics (Generic)
import System.FilePath ((</>))
import Text.Printf (printf)
import System.Random (randomRIO)

--------------------------------------------------------------------------------
-- YAML Types
--------------------------------------------------------------------------------

data TemplateYaml = TemplateYaml
  { tyName :: Text,
    tyCpuCount :: Int,
    tyRamMb :: Int,
    tyDescription :: Maybe Text,
    tyDrives :: [TemplateDriveYaml],
    tyNetworkInterfaces :: [TemplateNetworkInterfaceYaml],
    tySshKeys :: [TemplateSshKeyYaml]
  }
  deriving (Show, Generic)

instance FromJSON TemplateYaml where
  parseJSON = withObject "TemplateYaml" $ \o ->
    TemplateYaml
      <$> o .: "name"
      <*> o .: "cpuCount"
      <*> o .: "ramMb"
      <*> o .:? "description"
      <*> o .: "drives"
      <*> o .:? "networkInterfaces" .!= []
      <*> o .:? "sshKeys" .!= []

data TemplateDriveYaml = TemplateDriveYaml
  { tdyDiskImageName :: Text,
    tdyInterface :: DriveInterface,
    tdyMedia :: Maybe DriveMedia,
    tdyReadOnly :: Maybe Bool,
    tdyCacheType :: Maybe CacheType,
    tdyDiscard :: Maybe Bool,
    tdyStrategy :: TemplateCloneStrategy,
    tdyNewSizeMb :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON TemplateDriveYaml where
  parseJSON = withObject "TemplateDriveYaml" $ \o ->
    TemplateDriveYaml
      <$> o .: "diskImageName"
      <*> o .: "interface"
      <*> o .:? "media"
      <*> o .:? "readOnly"
      <*> o .:? "cacheType"
      <*> o .:? "discard"
      <*> o .: "strategy"
      <*> o .:? "newSizeMb"

data TemplateNetworkInterfaceYaml = TemplateNetworkInterfaceYaml
  { tnyType :: NetInterfaceType,
    tnyHostDevice :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON TemplateNetworkInterfaceYaml where
  parseJSON = withObject "TemplateNetworkInterfaceYaml" $ \o ->
    TemplateNetworkInterfaceYaml
      <$> o .: "type"
      <*> o .:? "hostDevice"

data TemplateSshKeyYaml = TemplateSshKeyYaml
  { tkyName :: Text
  }
  deriving (Show, Generic)

instance FromJSON TemplateSshKeyYaml where
  parseJSON = withObject "TemplateSshKeyYaml" $ \o ->
    TemplateSshKeyYaml
      <$> o .: "name"

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

handleTemplateCreate :: ServerState -> Text -> IO Response
handleTemplateCreate state yamlContent = runStdoutLoggingT $ do
  case decodeEither' (T.encodeUtf8 yamlContent) of
    Left err -> do
      let msg = T.pack $ show err
      logWarnN $ "Failed to parse template YAML: " <> msg
      pure $ RespError msg
    Right ty -> do
      logInfoN $ "Creating template: " <> tyName ty
      now <- liftIO getCurrentTime
      result <- liftIO $ runSqlPool (createTemplate ty now) (ssDbPool state)
      case result of
        Left err -> do
          logWarnN $ "Failed to create template: " <> err
          pure $ RespError err
        Right tid -> do
          logInfoN $ "Created template with ID: " <> T.pack (show $ fromSqlKey tid)
          pure $ RespTemplateCreated (fromSqlKey tid)

handleTemplateList :: ServerState -> IO Response
handleTemplateList state = do
  templates <- runSqlPool (selectList [] [Asc TemplateVmName]) (ssDbPool state)
  let infoList = map toTemplateInfo templates
  pure $ RespTemplateList infoList
  where
    toTemplateInfo (Entity tid t) = TemplateVmInfo
      { tviId = fromSqlKey tid,
        tviName = templateVmName t,
        tviCpuCount = templateVmCpuCount t,
        tviRamMb = templateVmRamMb t,
        tviDescription = templateVmDescription t
      }

handleTemplateShow :: ServerState -> Int64 -> IO Response
handleTemplateShow state tidLong = do
  let tid = toSqlKey tidLong
  result <- runSqlPool (getTemplateDetails tid) (ssDbPool state)
  case result of
    Nothing -> pure RespTemplateNotFound
    Just details -> pure $ RespTemplateInfo details

handleTemplateDelete :: ServerState -> Int64 -> IO Response
handleTemplateDelete state tidLong = runStdoutLoggingT $ do
  let tid = toSqlKey tidLong
  liftIO $ runSqlPool (deleteTemplate tid) (ssDbPool state)
  pure RespTemplateDeleted

handleTemplateInstantiate :: ServerState -> Int64 -> Text -> IO Response
handleTemplateInstantiate state tidLong newVmName = runStdoutLoggingT $ do
  logInfoN $ "Instantiating template " <> T.pack (show tidLong) <> " as '" <> newVmName <> "'"
  
  -- 1. Get template details
  mDetails <- liftIO $ runSqlPool (getTemplateDetails (toSqlKey tidLong)) (ssDbPool state)
  case mDetails of
    Nothing -> pure RespTemplateNotFound
    Just details -> do
      -- 2. Create VM record
      now <- liftIO getCurrentTime
      vmId <- liftIO $ runSqlPool (insert $ Vm newVmName now VmStopped (tvdCpuCount details) (tvdRamMb details) (tvdDescription details) Nothing) (ssDbPool state)
      
      -- 3. Instantiate drives
      driveResults <- forM (tvdDrives details) $ \td -> do
        liftIO $ instantiateDriveIO state vmId newVmName td
      
      -- Check for errors
      let errors = [err | Left err <- driveResults]
      if not (null errors)
        then do
          let msg = T.intercalate "; " errors
          logWarnN $ "Failed to instantiate drives: " <> msg
          pure $ RespError msg
        else do
          -- 4. Finish instantiation (Net and SSH)
          liftIO $ finishInstantiation state vmId details
          logInfoN $ "Instantiated VM with ID: " <> T.pack (show $ fromSqlKey vmId)
          pure $ RespTemplateInstantiated (fromSqlKey vmId)

--------------------------------------------------------------------------------
-- Internal Functions (SQL)
--------------------------------------------------------------------------------

createTemplate :: TemplateYaml -> UTCTime -> SqlPersistT IO (Either Text TemplateVmId)
createTemplate ty now = do
  -- Resolve disk images
  mDiskIds <- forM (tyDrives ty) $ \tdy -> do
    mDisk <- getBy (UniqueDiskImageName (tdyDiskImageName tdy))
    case mDisk of
      Nothing -> pure $ Left $ "Disk image not found: " <> tdyDiskImageName tdy
      Just (Entity tid _) -> pure $ Right tid

  -- Resolve SSH keys
  mKeyIds <- forM (tySshKeys ty) $ \tky -> do
    mKey <- getBy (UniqueSshKeyName (tkyName tky))
    case mKey of
      Nothing -> pure $ Left $ "SSH key not found: " <> tkyName tky
      Just (Entity kid _) -> pure $ Right kid

  case (sequence mDiskIds, sequence mKeyIds) of
    (Right diskIds, Right keyIds) -> do
      tid <- insert $ TemplateVm (tyName ty) (tyCpuCount ty) (tyRamMb ty) (tyDescription ty) now
      
      forM_ (zip diskIds (tyDrives ty)) $ \(diskId, tdy) -> do
        insert_ $ TemplateDrive
          tid
          diskId
          (tdyInterface tdy)
          (tdyMedia tdy)
          (maybe False id (tdyReadOnly tdy))
          (maybe CacheNone id (tdyCacheType tdy))
          (maybe False id (tdyDiscard tdy))
          (tdyStrategy tdy)
          (tdyNewSizeMb tdy)
      
      forM_ (tyNetworkInterfaces ty) $ \tny -> do
        insert_ $ TemplateNetworkInterface tid (tnyType tny) (tnyHostDevice tny)
      
      forM_ keyIds $ \keyId -> do
        insert_ $ TemplateSshKey tid keyId
      
      pure $ Right tid
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left err

getTemplateDetails :: TemplateVmId -> SqlPersistT IO (Maybe TemplateDetails)
getTemplateDetails tid = do
  mTemplate <- get tid
  case mTemplate of
    Nothing -> pure Nothing
    Just t -> do
      drives <- selectList [TemplateDriveTemplateId ==. tid] []
      driveInfos <- forM drives $ \(Entity _ td) -> do
        mDisk <- get (templateDriveDiskImageId td)
        let diskName = maybe "unknown" diskImageName mDisk
        pure $ TemplateDriveInfo
          { tvdiDiskImageId = fromSqlKey (templateDriveDiskImageId td),
            tvdiDiskImageName = diskName,
            tvdiInterface = templateDriveInterface td,
            tvdiMedia = templateDriveMedia td,
            tvdiReadOnly = templateDriveReadOnly td,
            tvdiCacheType = templateDriveCacheType td,
            tvdiDiscard = templateDriveDiscard td,
            tvdiCloneStrategy = templateDriveCloneStrategy td,
            tvdiNewSizeMb = templateDriveNewSizeMb td
          }

      netIfs <- selectList [TemplateNetworkInterfaceTemplateId ==. tid] []
      let netIfInfos = map (\(Entity _ tni) -> TemplateNetIfInfo (templateNetworkInterfaceInterfaceType tni) (templateNetworkInterfaceHostDevice tni)) netIfs

      sshKeys <- selectList [TemplateSshKeyTemplateId ==. tid] []
      sshKeyInfos <- forM sshKeys $ \(Entity _ tsk) -> do
        mKey <- get (templateSshKeySshKeyId tsk)
        let keyName = maybe "unknown" sshKeyName mKey
        pure $ TemplateSshKeyInfo (fromSqlKey $ templateSshKeySshKeyId tsk) keyName

      pure $ Just TemplateDetails
        { tvdId = fromSqlKey tid,
          tvdName = templateVmName t,
          tvdCpuCount = templateVmCpuCount t,
          tvdRamMb = templateVmRamMb t,
          tvdDescription = templateVmDescription t,
          tvdCreatedAt = templateVmCreatedAt t,
          tvdDrives = driveInfos,
          tvdNetIfs = netIfInfos,
          tvdSshKeys = sshKeyInfos
        }

deleteTemplate :: TemplateVmId -> SqlPersistT IO ()
deleteTemplate tid = do
  deleteWhere [TemplateDriveTemplateId ==. tid]
  deleteWhere [TemplateNetworkInterfaceTemplateId ==. tid]
  deleteWhere [TemplateSshKeyTemplateId ==. tid]
  delete tid

finishInstantiation :: ServerState -> VmId -> TemplateDetails -> IO ()
finishInstantiation state vmId details = runStdoutLoggingT $ do
  -- Network
  forM_ (tvdNetIfs details) $ \tni -> do
    mac <- liftIO generateMacAddress
    liftIO $ runSqlPool (insert_ $ NetworkInterface vmId (tvniType tni) (maybe "" id (tvniHostDevice tni)) mac) (ssDbPool state)

  -- SSH Keys
  forM_ (tvdSshKeys details) $ \tsk -> do
    liftIO $ runSqlPool (insert_ $ VmSshKey vmId (toSqlKey $ tvskiId tsk)) (ssDbPool state)

  -- Generate cloud-init ISO if SSH keys are present
  if not (null (tvdSshKeys details))
    then do
      logInfoN $ "Generating cloud-init ISO for instantiated VM with " <> T.pack (show (length (tvdSshKeys details))) <> " SSH key(s)"
      result <- liftIO $ regenerateCloudInitIso (ssQemuConfig state) (ssDbPool state) (fromSqlKey vmId) (tvdName details)
      case result of
        Left err -> logWarnN $ "Failed to generate cloud-init ISO: " <> err
        Right _ -> logInfoN "Cloud-init ISO generated and attached"
    else
      logInfoN "No SSH keys attached, skipping cloud-init ISO generation"

generateMacAddress :: IO Text
generateMacAddress = do
  [b1, b2, b3] <- forM [1..3] $ \_ -> randomRIO (0, 255 :: Int)
  -- QEMU OUI is 52:54:00
  let mac = T.pack $ printf "52:54:00:%02x:%02x:%02x" b1 b2 b3
  pure mac

--------------------------------------------------------------------------------
-- Internal Functions (IO & Orchestration)
--------------------------------------------------------------------------------

instantiateDriveIO :: ServerState -> VmId -> Text -> TemplateDriveInfo -> IO (Either Text ())
instantiateDriveIO state vmId vmName td = runStdoutLoggingT $ do
  mDisk <- liftIO $ runSqlPool (get (toSqlKey $ tvdiDiskImageId td :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure $ Left "Master disk image not found"
    Just masterDisk -> do
      case tvdiCloneStrategy td of
        StrategyDirect -> do
          liftIO $ runSqlPool (insert_ $ Drive vmId (toSqlKey $ tvdiDiskImageId td) (tvdiInterface td) (tvdiMedia td) (tvdiReadOnly td) (tvdiCacheType td) (tvdiDiscard td)) (ssDbPool state)
          pure $ Right ()
        
        StrategyClone -> do
          let newName = vmName <> "-" <> tvdiDiskImageName td
          case sanitizeDiskName newName of
            Left err -> pure $ Left err
            Right safeName -> do
              basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
              let fileName = T.unpack safeName <> "." <> T.unpack (enumToText (diskImageFormat masterDisk))
                  destPath = basePath </> fileName
              
              srcPath <- liftIO $ resolveDiskPath (ssQemuConfig state) masterDisk
              
              -- Check if master is in use (should be stopped for cloning if we want consistency)
              runningVms <- liftIO $ runSqlPool (getRunningAttachedVms (tvdiDiskImageId td)) (ssDbPool state)
              if not (null runningVms)
                then pure $ Left $ "Cannot clone disk " <> tvdiDiskImageName td <> " because it is attached to running VMs"
                else do
                  result <- liftIO $ cloneImage srcPath destPath
                  case result of
                    ImageError err -> pure $ Left err
                    ImageNotFound -> pure $ Left "Master image file not found"
                    _ -> do
                      now <- liftIO getCurrentTime
                      newDiskId <- liftIO $ runSqlPool (do
                        dId <- insert DiskImage
                          { diskImageName = safeName,
                            diskImageFilePath = T.pack destPath,
                            diskImageFormat = diskImageFormat masterDisk,
                            diskImageSizeMb = diskImageSizeMb masterDisk,
                            diskImageCreatedAt = now,
                            diskImageBackingImageId = diskImageBackingImageId masterDisk
                          }
                        -- Clone snapshots
                        baseSnapshots <- selectList [SnapshotDiskImageId ==. (toSqlKey $ tvdiDiskImageId td)] []
                        forM_ baseSnapshots $ \snapEntity -> do
                          let snap = entityVal snapEntity
                          insert_ snap {snapshotDiskImageId = dId}
                        pure dId
                        ) (ssDbPool state)
                      
                      -- Resize if requested
                      case tvdiNewSizeMb td of
                        Just newSize -> do
                          res <- liftIO $ resizeImage destPath (fromIntegral newSize)
                          case res of
                            ImageSuccess -> do
                               liftIO $ runSqlPool (update newDiskId [DiskImageSizeMb =. Just newSize]) (ssDbPool state)
                               pure ()
                            _ -> pure () -- Log warning?
                        Nothing -> pure ()
                      
                      -- Attach to VM
                      liftIO $ runSqlPool (insert_ $ Drive vmId newDiskId (tvdiInterface td) (tvdiMedia td) (tvdiReadOnly td) (tvdiCacheType td) (tvdiDiscard td)) (ssDbPool state)
                      pure $ Right ()

        StrategyOverlay -> do
          let newName = vmName <> "-" <> tvdiDiskImageName td <> "-overlay"
          case sanitizeDiskName newName of
            Left err -> pure $ Left err
            Right safeName -> do
              basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
              let fileName = T.unpack safeName <> ".qcow2"
                  destPath = basePath </> fileName
              
              srcPath <- liftIO $ resolveDiskPath (ssQemuConfig state) masterDisk
              
              result <- liftIO $ createOverlay destPath srcPath (diskImageFormat masterDisk)
              case result of
                ImageError err -> pure $ Left err
                ImageFormatNotSupported msg -> pure $ Left msg
                ImageNotFound -> pure $ Left "Master image file not found"
                ImageSuccess -> do
                  now <- liftIO getCurrentTime
                  newDiskId <- liftIO $ runSqlPool (insert DiskImage
                    { diskImageName = safeName,
                      diskImageFilePath = T.pack destPath,
                      diskImageFormat = FormatQcow2,
                      diskImageSizeMb = diskImageSizeMb masterDisk,
                      diskImageCreatedAt = now,
                      diskImageBackingImageId = Just (toSqlKey $ tvdiDiskImageId td)
                    }) (ssDbPool state)
                  
                   -- Resize if requested
                  case tvdiNewSizeMb td of
                    Just newSize -> do
                      res <- liftIO $ resizeImage destPath (fromIntegral newSize)
                      case res of
                        ImageSuccess -> do
                           liftIO $ runSqlPool (update newDiskId [DiskImageSizeMb =. Just newSize]) (ssDbPool state)
                           pure ()
                        _ -> pure ()
                    Nothing -> pure ()

                  -- Attach to VM
                  liftIO $ runSqlPool (insert_ $ Drive vmId newDiskId (tvdiInterface td) (tvdiMedia td) (tvdiReadOnly td) (tvdiCacheType td) (tvdiDiscard td)) (ssDbPool state)
                  pure $ Right ()
