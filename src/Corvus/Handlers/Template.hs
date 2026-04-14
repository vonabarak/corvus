{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Corvus.Handlers.Template
  ( -- * Action types
    TemplateCreate (..)
  , TemplateUpdate (..)
  , TemplateDelete (..)
  , TemplateInstantiate (..)

    -- * Handlers
  , handleTemplateCreate
  , handleTemplateUpdate
  , handleTemplateList
  , handleTemplateShow
  , handleTemplateDelete
  , handleTemplateInstantiate

    -- * YAML schema (re-used by Apply)
  , TemplateYaml (..)
  , TemplateDriveYaml (..)
  , TemplateNetworkInterfaceYaml (..)
  , TemplateSshKeyYaml (..)
  , CloudInitConfigYaml (..)
  , insertTemplateYaml
  )
where

import Corvus.Action
import Corvus.Handlers.Vm (VmCreate (..))

import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Handlers.CloudInit (RegenerateCloudInit (..))
import Corvus.Handlers.Disk (DiskAttach (..), DiskClone (..), DiskCreate (..), DiskCreateOverlay (..))
import Corvus.Handlers.Resolve (validateName)
import Corvus.Model
import Corvus.Protocol
import Corvus.Types
import Corvus.Utils.Network (generateMacAddress)
import Data.Aeson (Value (..))
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Yaml (FromJSON (..), decodeEither', withObject, (.!=), (.:), (.:?))
import qualified Data.Yaml as Yaml
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, toSqlKey)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- YAML Types
--------------------------------------------------------------------------------

data TemplateYaml = TemplateYaml
  { tyName :: Text
  , tyCpuCount :: Int
  , tyRamMb :: Int
  , tyDescription :: Maybe Text
  , tyHeadless :: Bool
  , tyCloudInit :: Bool
  , tyGuestAgent :: Bool
  , tyAutostart :: Bool
  , tyCloudInitConfig :: Maybe CloudInitConfigYaml
  , tyDrives :: [TemplateDriveYaml]
  , tyNetworkInterfaces :: [TemplateNetworkInterfaceYaml]
  , tySshKeys :: [TemplateSshKeyYaml]
  }
  deriving (Show, Generic)

instance FromJSON TemplateYaml where
  parseJSON = withObject "TemplateYaml" $ \o ->
    TemplateYaml
      <$> o .: "name"
      <*> o .: "cpuCount"
      <*> o .: "ramMb"
      <*> o .:? "description"
      <*> o .:? "headless" .!= False
      <*> o .:? "cloudInit" .!= False
      <*> o .:? "guestAgent" .!= False
      <*> o .:? "autostart" .!= False
      <*> o .:? "cloudInitConfig"
      <*> o .: "drives"
      <*> o .:? "networkInterfaces" .!= []
      <*> o .:? "sshKeys" .!= []

data TemplateDriveYaml = TemplateDriveYaml
  { tdyDiskImageName :: Maybe Text
  , tdyInterface :: DriveInterface
  , tdyMedia :: Maybe DriveMedia
  , tdyReadOnly :: Maybe Bool
  , tdyCacheType :: Maybe CacheType
  , tdyDiscard :: Maybe Bool
  , tdyStrategy :: TemplateCloneStrategy
  , tdySizeMb :: Maybe Int
  , tdyFormat :: Maybe DriveFormat
  }
  deriving (Show, Generic)

instance FromJSON TemplateDriveYaml where
  parseJSON = withObject "TemplateDriveYaml" $ \o ->
    TemplateDriveYaml
      <$> o .:? "diskImageName"
      <*> o .: "interface"
      <*> o .:? "media"
      <*> o .:? "readOnly"
      <*> o .:? "cacheType"
      <*> o .:? "discard"
      <*> o .: "strategy"
      <*> o .:? "sizeMb"
      <*> o .:? "format"

data TemplateNetworkInterfaceYaml = TemplateNetworkInterfaceYaml
  { tnyType :: NetInterfaceType
  , tnyHostDevice :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON TemplateNetworkInterfaceYaml where
  parseJSON = withObject "TemplateNetworkInterfaceYaml" $ \o ->
    TemplateNetworkInterfaceYaml
      <$> o .: "type"
      <*> o .:? "hostDevice"

newtype TemplateSshKeyYaml = TemplateSshKeyYaml
  { tkyName :: Text
  }
  deriving (Show, Generic)

instance FromJSON TemplateSshKeyYaml where
  parseJSON = withObject "TemplateSshKeyYaml" $ \o ->
    TemplateSshKeyYaml
      <$> o .: "name"

-- | Cloud-init config YAML, used in both template and apply contexts.
-- userData and networkConfig are parsed as YAML Values and serialized to text for DB storage.
-- Supports both structured YAML (objects/arrays) and raw text strings (e.g. PowerShell scripts
-- for cloudbase-init on Windows). Raw strings are stored as-is; structured values are
-- serialized via @Yaml.encode@.
data CloudInitConfigYaml = CloudInitConfigYaml
  { cicyUserData :: Maybe Text
  , cicyNetworkConfig :: Maybe Text
  , cicyInjectSshKeys :: Bool
  }
  deriving (Show, Generic)

instance FromJSON CloudInitConfigYaml where
  parseJSON = withObject "CloudInitConfigYaml" $ \o -> do
    mUserDataVal <- o .:? "userData" :: Yaml.Parser (Maybe Value)
    mNetworkConfigVal <- o .:? "networkConfig" :: Yaml.Parser (Maybe Value)
    injectKeys <- o .:? "injectSshKeys" .!= True
    let valueToText (String t) = t
        valueToText v = T.decodeUtf8 (Yaml.encode v)
    pure
      CloudInitConfigYaml
        { cicyUserData = fmap valueToText mUserDataVal
        , cicyNetworkConfig = fmap valueToText mNetworkConfigVal
        , cicyInjectSshKeys = injectKeys
        }

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

handleTemplateCreate :: ServerState -> Text -> IO Response
handleTemplateCreate state yamlContent = runServerLogging state $ do
  case decodeEither' (T.encodeUtf8 yamlContent) of
    Left err -> do
      let msg = T.pack $ show err
      logWarnN $ "Failed to parse template YAML: " <> msg
      pure $ RespError msg
    Right ty ->
      case validateName "Template" (tyName ty) of
        Left err -> pure $ RespError err
        Right () -> do
          logInfoN $ "Creating template: " <> tyName ty
          now <- liftIO getCurrentTime
          result <- liftIO $ runSqlPool (insertTemplateYaml ty now) (ssDbPool state)
          case result of
            Left err -> do
              logWarnN $ "Failed to create template: " <> err
              pure $ RespError err
            Right tid -> do
              logInfoN $ "Created template with ID: " <> T.pack (show $ fromSqlKey tid)
              pure $ RespTemplateCreated (fromSqlKey tid)

-- | Atomically replace an existing template with the contents of a new YAML.
-- The old rows are deleted and new ones inserted within a single transaction,
-- so a failed insert (e.g. renaming to a name that already exists) rolls back
-- and leaves the original template intact.
handleTemplateUpdate :: ServerState -> Int64 -> Text -> IO Response
handleTemplateUpdate state oldTidLong yamlContent = runServerLogging state $ do
  case decodeEither' (T.encodeUtf8 yamlContent) of
    Left err -> do
      let msg = T.pack $ show err
      logWarnN $ "Failed to parse template YAML: " <> msg
      pure $ RespError msg
    Right ty ->
      case validateName "Template" (tyName ty) of
        Left err -> pure $ RespError err
        Right () -> do
          logInfoN $ "Updating template #" <> T.pack (show oldTidLong) <> " -> " <> tyName ty
          now <- liftIO getCurrentTime
          let oldTid = toSqlKey oldTidLong :: TemplateVmId
          let replace = do
                deleteTemplate oldTid
                insertTemplateYaml ty now
              runReplace = do
                mOld <- get oldTid
                case mOld of
                  Nothing -> pure $ Left "Template not found"
                  Just old ->
                    let newName = tyName ty
                     in if newName /= templateVmName old
                          then do
                            mExisting <- getBy (UniqueTemplateVmName newName)
                            case mExisting of
                              Just _ -> pure $ Left $ "Template with name '" <> newName <> "' already exists"
                              Nothing -> replace
                          else replace
          result <- liftIO $ runSqlPool runReplace (ssDbPool state)
          case result of
            Left err -> do
              logWarnN $ "Failed to update template: " <> err
              pure $ RespError err
            Right newTid -> do
              logInfoN $ "Updated template with ID: " <> T.pack (show $ fromSqlKey newTid)
              pure $ RespTemplateUpdated (fromSqlKey newTid)

handleTemplateList :: ServerState -> IO Response
handleTemplateList state = do
  templates <- runSqlPool (selectList [] [Asc TemplateVmName]) (ssDbPool state)
  let infoList = map toTemplateInfo templates
  pure $ RespTemplateList infoList
  where
    toTemplateInfo (Entity tid t) =
      TemplateVmInfo
        { tviId = fromSqlKey tid
        , tviName = templateVmName t
        , tviCpuCount = templateVmCpuCount t
        , tviRamMb = templateVmRamMb t
        , tviDescription = templateVmDescription t
        , tviHeadless = templateVmHeadless t
        , tviGuestAgent = templateVmGuestAgent t
        , tviAutostart = templateVmAutostart t
        }

handleTemplateShow :: ServerState -> Int64 -> IO Response
handleTemplateShow state tidLong = do
  let tid = toSqlKey tidLong
  result <- runSqlPool (getTemplateDetails tid) (ssDbPool state)
  case result of
    Nothing -> pure RespTemplateNotFound
    Just details -> pure $ RespTemplateInfo details

handleTemplateDelete :: ServerState -> Int64 -> IO Response
handleTemplateDelete state tidLong = runServerLogging state $ do
  let tid = toSqlKey tidLong
  liftIO $ runSqlPool (deleteTemplate tid) (ssDbPool state)
  pure RespTemplateDeleted

handleTemplateInstantiate :: ServerState -> Int64 -> Text -> TaskId -> IO Response
handleTemplateInstantiate state tidLong newVmName parentTaskId = runServerLogging state $ do
  logInfoN $ "Instantiating template " <> T.pack (show tidLong) <> " as '" <> newVmName <> "'"
  let pool = ssDbPool state

  -- 1. Get template details
  mDetails <- liftIO $ runSqlPool (getTemplateDetails (toSqlKey tidLong)) pool
  case mDetails of
    Nothing -> pure RespTemplateNotFound
    Just details -> do
      -- Subtask 1: Create VM record (delegates to VmCreate action)
      vmResp <-
        liftIO $
          runActionAsSubtask
            state
            ( VmCreate
                newVmName
                (tvdCpuCount details)
                (tvdRamMb details)
                (tvdDescription details)
                (tvdHeadless details)
                (tvdGuestAgent details)
                (tvdCloudInit details)
                (tvdAutostart details)
            )
            parentTaskId
      case vmResp of
        RespVmCreated vmIdLong -> do
          let vmId = toSqlKey vmIdLong :: VmId

          -- Subtask per drive: Instantiate drives
          driveResults <- forM (tvdDrives details) $ \td ->
            liftIO $ do
              resp <- runActionAsSubtask state (InstantiateDrive vmId newVmName td) parentTaskId
              pure $ classifyResponse resp

          let errors = [err | (TaskError, Just err) <- driveResults]
          if not (null errors)
            then do
              let msg = T.intercalate "; " errors
              logWarnN $ "Failed to instantiate drives: " <> msg
              pure $ RespError msg
            else do
              -- Subtask: Finish instantiation (Net, SSH, cloud-init)
              liftIO $ finishInstantiation state vmId details parentTaskId
              logInfoN $ "Instantiated VM with ID: " <> T.pack (show $ fromSqlKey vmId)
              pure $ RespTemplateInstantiated (fromSqlKey vmId)
        RespError err -> pure $ RespError err
        _ -> pure $ RespError "Unexpected response from VM creation"

--------------------------------------------------------------------------------
-- Internal Functions (SQL)
--------------------------------------------------------------------------------

-- | Insert an already-parsed 'TemplateYaml' into the database.
-- Used by both 'handleTemplateCreate' and 'handleTemplateUpdate' and by the
-- Apply subsystem when processing a @templates:@ section.
insertTemplateYaml :: TemplateYaml -> UTCTime -> SqlPersistT IO (Either Text TemplateVmId)
insertTemplateYaml ty now = do
  -- Validate drive definitions
  let driveErrs = concatMap validateDrive (tyDrives ty)
  if not (null driveErrs)
    then pure $ Left $ T.intercalate "; " driveErrs
    else do
      -- Resolve disk images (only for non-create strategies)
      mDiskIds <- forM (tyDrives ty) $ \tdy ->
        case tdyStrategy tdy of
          StrategyCreate -> pure $ Right Nothing
          _ -> case tdyDiskImageName tdy of
            Nothing -> pure $ Left "diskImageName is required for clone/overlay/direct strategies"
            Just diskName -> do
              mDisk <- getBy (UniqueDiskImageName diskName)
              case mDisk of
                Nothing -> pure $ Left $ "Disk image not found: " <> diskName
                Just (Entity did _) -> pure $ Right (Just did)

      -- Resolve SSH keys
      mKeyIds <- forM (tySshKeys ty) $ \tky -> do
        mKey <- getBy (UniqueSshKeyName (tkyName tky))
        case mKey of
          Nothing -> pure $ Left $ "SSH key not found: " <> tkyName tky
          Just (Entity kid _) -> pure $ Right kid

      -- Validate: SSH keys require cloud-init
      let hasKeys = not (null (tySshKeys ty))
      if hasKeys && not (tyCloudInit ty)
        then pure $ Left "Template has SSH keys but cloud-init is not enabled"
        else case (sequence mDiskIds, sequence mKeyIds) of
          (Right diskIds, Right keyIds) -> do
            mTid <- insertUnique $ TemplateVm (tyName ty) (tyCpuCount ty) (tyRamMb ty) (tyDescription ty) (tyHeadless ty) (tyCloudInit ty) (tyGuestAgent ty) (tyAutostart ty) now
            case mTid of
              Nothing -> pure $ Left $ "Template with name '" <> tyName ty <> "' already exists"
              Just tid -> do
                forM_ (zip diskIds (tyDrives ty)) $ \(mDiskId, tdy) ->
                  insert_ $
                    TemplateDrive
                      tid
                      mDiskId
                      (tdyDiskImageName tdy)
                      (tdyInterface tdy)
                      (tdyMedia tdy)
                      (fromMaybe False (tdyReadOnly tdy))
                      (fromMaybe CacheNone (tdyCacheType tdy))
                      (fromMaybe False (tdyDiscard tdy))
                      (tdyStrategy tdy)
                      (tdySizeMb tdy)
                      (tdyFormat tdy)

                forM_ (tyNetworkInterfaces ty) $ \tny ->
                  insert_ $ TemplateNetworkInterface tid (tnyType tny) (tnyHostDevice tny)

                forM_ keyIds $ \keyId ->
                  insert_ $ TemplateSshKey tid keyId

                -- Insert cloud-init config if provided
                forM_ (tyCloudInitConfig ty) $ \cic ->
                  insert_ $
                    TemplateCloudInit
                      tid
                      (cicyUserData cic)
                      (cicyNetworkConfig cic)
                      (cicyInjectSshKeys cic)

                pure $ Right tid
          (Left err, _) -> pure $ Left err
          (_, Left err) -> pure $ Left err
  where
    validateDrive tdy = case tdyStrategy tdy of
      StrategyCreate ->
        let errs1 = ["format is required for 'create' strategy" | isNothing (tdyFormat tdy)]
            errs2 = ["sizeMb is required for 'create' strategy" | isNothing (tdySizeMb tdy)]
         in errs1 ++ errs2
      _ ->
        ["diskImageName is required for '" <> enumToText (tdyStrategy tdy) <> "' strategy" | isNothing (tdyDiskImageName tdy)]

getTemplateDetails :: TemplateVmId -> SqlPersistT IO (Maybe TemplateDetails)
getTemplateDetails tid = do
  mTemplate <- get tid
  case mTemplate of
    Nothing -> pure Nothing
    Just t -> do
      drives <- selectList [TemplateDriveTemplateId ==. tid] []
      driveInfos <- forM drives $ \(Entity _ td) -> do
        mDiskName <- case templateDriveDiskName td of
          Just n -> pure $ Just n
          Nothing -> case templateDriveDiskImageId td of
            Nothing -> pure Nothing
            Just diskId -> do
              mDisk <- get diskId
              pure $ Just $ maybe "unknown" diskImageName mDisk
        pure $
          TemplateDriveInfo
            { tvdiDiskImageId = fmap fromSqlKey (templateDriveDiskImageId td)
            , tvdiDiskImageName = mDiskName
            , tvdiInterface = templateDriveInterface td
            , tvdiMedia = templateDriveMedia td
            , tvdiReadOnly = templateDriveReadOnly td
            , tvdiCacheType = templateDriveCacheType td
            , tvdiDiscard = templateDriveDiscard td
            , tvdiCloneStrategy = templateDriveCloneStrategy td
            , tvdiSizeMb = templateDriveSizeMb td
            , tvdiFormat = templateDriveFormat td
            }

      netIfs <- selectList [TemplateNetworkInterfaceTemplateId ==. tid] []
      let netIfInfos = map (\(Entity _ tni) -> TemplateNetIfInfo (templateNetworkInterfaceInterfaceType tni) (templateNetworkInterfaceHostDevice tni)) netIfs

      sshKeys <- selectList [TemplateSshKeyTemplateId ==. tid] []
      sshKeyInfos <- forM sshKeys $ \(Entity _ tsk) -> do
        mKey <- get (templateSshKeySshKeyId tsk)
        let keyName = maybe "unknown" sshKeyName mKey
        pure $ TemplateSshKeyInfo (fromSqlKey $ templateSshKeySshKeyId tsk) keyName

      -- Get cloud-init config if present
      mCiConfig <- getBy (UniqueTemplateCloudInitVm tid)
      let ciInfo =
            fmap
              ( \(Entity _ tci) ->
                  CloudInitInfo
                    { ciiUserData = templateCloudInitUserData tci
                    , ciiNetworkConfig = templateCloudInitNetworkConfig tci
                    , ciiInjectSshKeys = templateCloudInitInjectSshKeys tci
                    }
              )
              mCiConfig

      pure $
        Just
          TemplateDetails
            { tvdId = fromSqlKey tid
            , tvdName = templateVmName t
            , tvdCpuCount = templateVmCpuCount t
            , tvdRamMb = templateVmRamMb t
            , tvdDescription = templateVmDescription t
            , tvdHeadless = templateVmHeadless t
            , tvdCloudInit = templateVmCloudInit t
            , tvdGuestAgent = templateVmGuestAgent t
            , tvdAutostart = templateVmAutostart t
            , tvdCloudInitConfig = ciInfo
            , tvdCreatedAt = templateVmCreatedAt t
            , tvdDrives = driveInfos
            , tvdNetIfs = netIfInfos
            , tvdSshKeys = sshKeyInfos
            }

deleteTemplate :: TemplateVmId -> SqlPersistT IO ()
deleteTemplate tid = do
  deleteWhere [TemplateDriveTemplateId ==. tid]
  deleteWhere [TemplateNetworkInterfaceTemplateId ==. tid]
  deleteWhere [TemplateSshKeyTemplateId ==. tid]
  deleteBy (UniqueTemplateCloudInitVm tid)
  delete tid

finishInstantiation :: ServerState -> VmId -> TemplateDetails -> TaskId -> IO ()
finishInstantiation state vmId details parentTaskId = runServerLogging state $ do
  -- Network
  forM_ (tvdNetIfs details) $ \tni -> do
    mac <- liftIO generateMacAddress
    liftIO $ runSqlPool (insert_ $ NetworkInterface vmId (tvniType tni) (fromMaybe "" (tvniHostDevice tni)) mac Nothing Nothing) (ssDbPool state)

  -- SSH Keys
  forM_ (tvdSshKeys details) $ \tsk -> do
    liftIO $ runSqlPool (insert_ $ VmSshKey vmId (toSqlKey (tvskiId tsk))) (ssDbPool state)

  -- Copy cloud-init config from template if present
  forM_ (tvdCloudInitConfig details) $ \ciConfig ->
    liftIO $
      runSqlPool
        ( insert_ $
            CloudInit
              vmId
              (ciiUserData ciConfig)
              (ciiNetworkConfig ciConfig)
              (ciiInjectSshKeys ciConfig)
        )
        (ssDbPool state)

  -- Generate cloud-init ISO if cloud-init is enabled (tracked as subtask)
  when (tvdCloudInit details) $ do
    logInfoN "Generating cloud-init ISO for instantiated VM"
    resp <- liftIO $ runActionAsSubtask state (RegenerateCloudInit (fromSqlKey vmId) (tvdName details)) parentTaskId
    case resp of
      RespError err -> logWarnN $ "Failed to generate cloud-init ISO: " <> err
      _ -> logInfoN "Cloud-init ISO generated and attached"

--------------------------------------------------------------------------------
-- Internal Functions (IO & Orchestration)
--------------------------------------------------------------------------------

instantiateDriveIO :: ActionContext -> VmId -> Text -> TemplateDriveInfo -> IO (Either Text ())
instantiateDriveIO ctx vmId vmName td = do
  let state = acState ctx
      parentTaskId = acTaskId ctx
      vmIdLong = fromSqlKey vmId
      nameSuffix = fromMaybe "disk" (tvdiDiskImageName td)
      attachDisk newDiskId = runActionAsSubtask state (DiskAttach vmIdLong newDiskId (tvdiInterface td) (tvdiMedia td) (tvdiReadOnly td) (tvdiDiscard td) (tvdiCacheType td)) parentTaskId
  case tvdiCloneStrategy td of
    StrategyDirect -> case tvdiDiskImageId td of
      Nothing -> pure $ Left "direct strategy requires a disk image"
      Just diskIdLong -> do
        resp <- attachDisk diskIdLong
        case resp of
          RespError err -> pure $ Left err
          _ -> pure $ Right ()
    StrategyClone -> case tvdiDiskImageId td of
      Nothing -> pure $ Left "clone strategy requires a disk image"
      Just diskIdLong -> do
        let newName = vmName <> "-" <> nameSuffix
        resp <- runActionAsSubtask state (DiskClone newName diskIdLong (tvdiSizeMb td) Nothing) parentTaskId
        case resp of
          RespDiskCreated newDiskId -> do
            attachResp <- attachDisk newDiskId
            case attachResp of
              RespError err -> pure $ Left err
              _ -> pure $ Right ()
          RespError err -> pure $ Left err
          _ -> pure $ Left "Unexpected response from disk clone"
    StrategyOverlay -> case tvdiDiskImageId td of
      Nothing -> pure $ Left "overlay strategy requires a disk image"
      Just diskIdLong -> do
        let newName = vmName <> "-" <> nameSuffix <> "-overlay"
        resp <- runActionAsSubtask state (DiskCreateOverlay newName diskIdLong (tvdiSizeMb td) Nothing) parentTaskId
        case resp of
          RespDiskCreated newDiskId -> do
            attachResp <- attachDisk newDiskId
            case attachResp of
              RespError err -> pure $ Left err
              _ -> pure $ Right ()
          RespError err -> pure $ Left err
          _ -> pure $ Left "Unexpected response from disk overlay"
    StrategyCreate -> case (tvdiFormat td, tvdiSizeMb td) of
      (Just fmt, Just sizeMb) -> do
        let newName = vmName <> "-" <> nameSuffix
        resp <- runActionAsSubtask state (DiskCreate newName fmt (fromIntegral sizeMb) Nothing) parentTaskId
        case resp of
          RespDiskCreated newDiskId -> do
            attachResp <- attachDisk newDiskId
            case attachResp of
              RespError err -> pure $ Left err
              _ -> pure $ Right ()
          RespError err -> pure $ Left err
          _ -> pure $ Left "Unexpected response from disk create"
      _ -> pure $ Left "create strategy requires 'format' and 'sizeMb'"

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

newtype TemplateCreate = TemplateCreate {tcrYaml :: Text}

instance Action TemplateCreate where
  actionSubsystem _ = SubTemplate
  actionCommand _ = "create"
  actionExecute ctx a = handleTemplateCreate (acState ctx) (tcrYaml a)

data TemplateUpdate = TemplateUpdate
  { tupOldId :: Int64
  , tupYaml :: Text
  }

instance Action TemplateUpdate where
  actionSubsystem _ = SubTemplate
  actionCommand _ = "update"
  actionEntityId = Just . fromIntegral . tupOldId
  actionExecute ctx a = handleTemplateUpdate (acState ctx) (tupOldId a) (tupYaml a)

newtype TemplateDelete = TemplateDelete {tdelTemplateId :: Int64}

instance Action TemplateDelete where
  actionSubsystem _ = SubTemplate
  actionCommand _ = "delete"
  actionEntityId = Just . fromIntegral . tdelTemplateId
  actionExecute ctx a = handleTemplateDelete (acState ctx) (tdelTemplateId a)

data TemplateInstantiate = TemplateInstantiate
  { tiTemplateId :: Int64
  , tiName :: Text
  }

instance Action TemplateInstantiate where
  actionSubsystem _ = SubTemplate
  actionCommand _ = "instantiate"
  actionEntityId = Just . fromIntegral . tiTemplateId
  actionExecute ctx a = handleTemplateInstantiate (acState ctx) (tiTemplateId a) (tiName a) (acTaskId ctx)

-- | Instantiate a single template drive (clone, overlay, or direct attach).
data InstantiateDrive = InstantiateDrive
  { idVmId :: !VmId
  , idVmName :: !Text
  , idDriveInfo :: !TemplateDriveInfo
  }

instance Action InstantiateDrive where
  actionSubsystem _ = SubDisk
  actionCommand _ = "instantiate"
  actionEntityName a = tvdiDiskImageName (idDriveInfo a)
  actionExecute ctx a = do
    result <- instantiateDriveIO ctx (idVmId a) (idVmName a) (idDriveInfo a)
    case result of
      Left err -> pure $ RespError err
      Right () -> pure RespOk
