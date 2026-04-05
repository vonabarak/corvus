{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SSH key management handlers.
-- Handles SSH key CRUD operations, VM associations, and cloud-init ISO generation.
module Corvus.Handlers.SshKey
  ( -- * SSH key handlers
    handleSshKeyCreate
  , handleSshKeyDelete
  , handleSshKeyList

    -- * VM association handlers
  , handleSshKeyAttach
  , handleSshKeyDetach
  , handleSshKeyListForVm

    -- * Cloud-init ISO management
  , regenerateCloudInitIso
  )
where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel (..), LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.CloudInit
import Corvus.Handlers.Resolve (validateName)
import Corvus.Handlers.Subtask (SubtaskSpec (..), withOptionalSubtask)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu.Config (QemuConfig)
import Corvus.Types
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)
import System.Directory (doesFileExist, removeFile)

--------------------------------------------------------------------------------
-- SSH Key CRUD Handlers
--------------------------------------------------------------------------------

-- | Create a new SSH key
handleSshKeyCreate :: ServerState -> Text -> Text -> IO Response
handleSshKeyCreate state name publicKey =
  case validateName "SSH key" name of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Creating SSH key: " <> name

      let pool = ssDbPool state

      -- Check if name already exists
      existing <- liftIO $ runSqlPool (getBy (UniqueSshKeyName name)) pool
      case existing of
        Just _ -> do
          logWarnN $ "SSH key with name '" <> name <> "' already exists"
          pure $ RespError $ "SSH key with name '" <> name <> "' already exists"
        Nothing -> do
          now <- liftIO getCurrentTime
          keyId <-
            liftIO $
              runSqlPool
                ( insert
                    SshKey
                      { sshKeyName = name
                      , sshKeyPublicKey = publicKey
                      , sshKeyCreatedAt = now
                      }
                )
                pool
          logInfoN $ "Created SSH key with ID: " <> T.pack (show $ fromSqlKey keyId)
          pure $ RespSshKeyCreated $ fromSqlKey keyId

-- | Delete an SSH key (fails if attached to any VMs)
handleSshKeyDelete :: ServerState -> Int64 -> IO Response
handleSshKeyDelete state keyId = runServerLogging state $ do
  logInfoN $ "Deleting SSH key: " <> T.pack (show keyId)

  let pool = ssDbPool state
  let keyKey = toSqlKey keyId :: SshKeyId

  -- Check if key exists
  mKey <- liftIO $ runSqlPool (get keyKey) pool
  case mKey of
    Nothing -> pure RespSshKeyNotFound
    Just _ -> do
      -- Check if key is attached to any VMs
      attachments <- liftIO $ runSqlPool (selectList [VmSshKeySshKeyId ==. keyKey] []) pool
      case attachments of
        [] -> do
          -- No attachments, safe to delete
          liftIO $ runSqlPool (delete keyKey) pool
          logInfoN "SSH key deleted"
          pure RespSshKeyOk
        _ -> do
          -- Key is in use — resolve VM names for the error message
          vmPairs <- liftIO $ forM attachments $ \att -> do
            let vmKey = vmSshKeyVmId (entityVal att)
            mVm <- runSqlPool (get vmKey) pool
            let name = maybe "(deleted)" vmName mVm
            pure (fromSqlKey vmKey, name)
          logWarnN $ "SSH key is attached to VMs: " <> T.pack (show (map fst vmPairs))
          pure $ RespSshKeyInUse vmPairs

-- | List all SSH keys
handleSshKeyList :: ServerState -> IO Response
handleSshKeyList state = runServerLogging state $ do
  logDebugN "Listing all SSH keys"

  let pool = ssDbPool state

  keys <- liftIO $ runSqlPool (selectList [] [Asc SshKeyName]) pool
  infos <- liftIO $ forM keys $ \(Entity keyId key) -> do
    attachments <- runSqlPool (selectList [VmSshKeySshKeyId ==. keyId] []) pool
    vmPairs <- forM attachments $ \att -> do
      let vmKey = vmSshKeyVmId (entityVal att)
      mVm <- runSqlPool (get vmKey) pool
      let name = maybe "(deleted)" vmName mVm
      pure (fromSqlKey vmKey, name)
    pure
      SshKeyInfo
        { skiId = fromSqlKey keyId
        , skiName = sshKeyName key
        , skiPublicKey = sshKeyPublicKey key
        , skiCreatedAt = sshKeyCreatedAt key
        , skiAttachedVms = vmPairs
        }
  pure $ RespSshKeyList infos

--------------------------------------------------------------------------------
-- VM Association Handlers
--------------------------------------------------------------------------------

-- | Attach an SSH key to a VM
-- This also regenerates the cloud-init ISO and registers/attaches it as a disk
handleSshKeyAttach :: ServerState -> Int64 -> Int64 -> IO Response
handleSshKeyAttach state vmId keyId = runServerLogging state $ do
  logInfoN $ "Attaching SSH key " <> T.pack (show keyId) <> " to VM " <> T.pack (show vmId)

  let pool = ssDbPool state
  let vmKey = toSqlKey vmId :: VmId
  let sshKeyKey = toSqlKey keyId :: SshKeyId

  -- Check VM exists
  mVm <- liftIO $ runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm -> do
      -- Check cloud-init is enabled
      if not (vmCloudInit vm)
        then do
          logWarnN "Cannot attach SSH key: cloud-init is not enabled on this VM"
          pure $ RespError "Cannot attach SSH key: cloud-init is not enabled on this VM"
        else do
          -- Check key exists
          mKey <- liftIO $ runSqlPool (get sshKeyKey) pool
          case mKey of
            Nothing -> pure RespSshKeyNotFound
            Just _ -> do
              -- Check if already attached
              existing <-
                liftIO $
                  runSqlPool
                    (getBy (UniqueVmSshKey vmKey sshKeyKey))
                    pool
              case existing of
                Just _ -> do
                  logWarnN "SSH key is already attached to this VM"
                  pure $ RespError "SSH key is already attached to this VM"
                Nothing -> do
                  -- Create attachment
                  liftIO $
                    runSqlPool
                      ( insert_
                          VmSshKey
                            { vmSshKeyVmId = vmKey
                            , vmSshKeySshKeyId = sshKeyKey
                            }
                      )
                      pool
                  logInfoN "SSH key attached"

                  -- Regenerate cloud-init ISO
                  result <- liftIO $ regenerateCloudInitIso (ssQemuConfig state) pool vmId (vmName vm) (ssLogLevel state) Nothing
                  case result of
                    Left err -> do
                      logWarnN $ "Failed to regenerate cloud-init ISO: " <> err
                      pure $ RespError $ "Key attached but ISO generation failed: " <> err
                    Right _ -> do
                      logInfoN "Cloud-init ISO regenerated"
                      pure RespSshKeyOk

-- | Detach an SSH key from a VM
-- This also regenerates/removes the cloud-init ISO
handleSshKeyDetach :: ServerState -> Int64 -> Int64 -> IO Response
handleSshKeyDetach state vmId keyId = runServerLogging state $ do
  logInfoN $ "Detaching SSH key " <> T.pack (show keyId) <> " from VM " <> T.pack (show vmId)

  let pool = ssDbPool state
  let vmKey = toSqlKey vmId :: VmId
  let sshKeyKey = toSqlKey keyId :: SshKeyId

  -- Check VM exists
  mVm <- liftIO $ runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm -> do
      -- Check if attachment exists
      mAttachment <-
        liftIO $
          runSqlPool
            (getBy (UniqueVmSshKey vmKey sshKeyKey))
            pool
      case mAttachment of
        Nothing -> pure RespSshKeyNotFound
        Just (Entity attachmentId _) -> do
          -- Remove attachment
          liftIO $ runSqlPool (delete attachmentId) pool
          logInfoN "SSH key detached"

          -- Regenerate cloud-init ISO if cloud-init is enabled
          if vmCloudInit vm
            then do
              result <- liftIO $ regenerateCloudInitIso (ssQemuConfig state) pool vmId (vmName vm) (ssLogLevel state) Nothing
              case result of
                Left err -> do
                  logWarnN $ "Failed to regenerate cloud-init ISO: " <> err
                  pure $ RespError $ "Key detached but ISO update failed: " <> err
                Right _ -> do
                  logInfoN "Cloud-init ISO updated"
                  pure RespSshKeyOk
            else pure RespSshKeyOk

-- | List SSH keys attached to a VM
handleSshKeyListForVm :: ServerState -> Int64 -> IO Response
handleSshKeyListForVm state vmId = runServerLogging state $ do
  logDebugN $ "Listing SSH keys for VM " <> T.pack (show vmId)

  let pool = ssDbPool state
  let vmKey = toSqlKey vmId :: VmId

  -- Check VM exists
  mVm <- liftIO $ runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just _ -> do
      -- Get all attachments for this VM
      attachments <- liftIO $ runSqlPool (selectList [VmSshKeyVmId ==. vmKey] []) pool
      -- Fetch the actual keys
      infos <- liftIO $ forM attachments $ \(Entity _ attachment) -> do
        let sshKeyKey = vmSshKeySshKeyId attachment
        mKey <- runSqlPool (get sshKeyKey) pool
        case mKey of
          Nothing -> pure Nothing
          Just key -> do
            -- Get all VMs this key is attached to
            allAttachments <- runSqlPool (selectList [VmSshKeySshKeyId ==. sshKeyKey] []) pool
            vmPairs <- forM allAttachments $ \att -> do
              let vmKey = vmSshKeyVmId (entityVal att)
              mVm <- runSqlPool (get vmKey) pool
              let vName = maybe "(deleted)" vmName mVm
              pure (fromSqlKey vmKey, vName)
            pure $
              Just
                SshKeyInfo
                  { skiId = fromSqlKey sshKeyKey
                  , skiName = sshKeyName key
                  , skiPublicKey = sshKeyPublicKey key
                  , skiCreatedAt = sshKeyCreatedAt key
                  , skiAttachedVms = vmPairs
                  }
      pure $ RespSshKeyList $ map (\(Just x) -> x) $ filter (/= Nothing) infos

--------------------------------------------------------------------------------
-- Cloud-Init ISO Management
--------------------------------------------------------------------------------

-- | Regenerate cloud-init ISO for a VM.
-- Always generates the ISO (needed for guest agent installation even without SSH keys).
-- When @mParentTaskId@ is @Just parentId@, the operation is tracked as a subtask.
regenerateCloudInitIso :: QemuConfig -> Pool SqlBackend -> Int64 -> Text -> LogLevel -> Maybe TaskId -> IO (Either Text ())
regenerateCloudInitIso qemuConfig pool vmId vmName logLevel mParentTaskId = do
  let spec = SubtaskSpec SubSshKey "cloud-init" (Just vmName)
  withOptionalSubtask
    pool
    mParentTaskId
    spec
    (doRegenerateCloudInitIso qemuConfig pool vmId vmName logLevel)
    (const Nothing)

-- | Internal: actual cloud-init ISO regeneration logic.
doRegenerateCloudInitIso :: QemuConfig -> Pool SqlBackend -> Int64 -> Text -> LogLevel -> IO (Either Text ())
doRegenerateCloudInitIso qemuConfig pool vmId vmName logLevel = do
  let vmKey = toSqlKey vmId :: VmId

  -- Get all SSH keys attached to this VM
  attachments <- runSqlPool (selectList [VmSshKeyVmId ==. vmKey] []) pool
  sshKeys <- forM attachments $ \(Entity _ attachment) -> do
    mKey <- runSqlPool (get (vmSshKeySshKeyId attachment)) pool
    pure $ fmap sshKeyPublicKey mKey

  let publicKeys = catMaybes sshKeys

  -- Check for custom cloud-init config
  mCustomConfig <- runSqlPool (getBy (UniqueCloudInitVm vmKey)) pool

  -- Always generate ISO (guest agent installation + SSH keys if any)
  vmDir <- getCloudInitDir qemuConfig vmName
  let config = case mCustomConfig of
        Just (Entity _ ci) ->
          defaultCloudInitConfig
            { ciHostname = vmName
            , ciInstanceId = "corvus-" <> T.pack (show vmId)
            , ciCustomUserData = cloudInitUserData ci
            , ciNetworkConfig = cloudInitNetworkConfig ci
            , ciInjectSshKeys = cloudInitInjectSshKeys ci
            }
        Nothing ->
          defaultCloudInitConfig
            { ciHostname = vmName
            , ciInstanceId = "corvus-" <> T.pack (show vmId)
            }
  result <- generateCloudInitIso vmDir config publicKeys
  case result of
    Left err -> pure $ Left err
    Right isoPath -> do
      -- Ensure disk is registered
      ensureCloudInitDiskRegistered pool vmId vmName (T.pack isoPath) logLevel
      pure $ Right ()

-- | Remove cloud-init ISO for a VM
removeCloudInitIsoForVm :: QemuConfig -> Text -> IO ()
removeCloudInitIsoForVm qemuConfig vmName = do
  isoPath <- getCloudInitIsoPath qemuConfig vmName
  exists <- doesFileExist isoPath
  when exists $ removeFile isoPath

-- | Ensure cloud-init disk is registered and attached to VM
ensureCloudInitDiskRegistered :: Pool SqlBackend -> Int64 -> Text -> Text -> LogLevel -> IO ()
ensureCloudInitDiskRegistered pool vmId vmName isoPath logLevel = runFilteredLogging logLevel $ do
  let vmKey = toSqlKey vmId :: VmId
  let diskName = vmName <> "-cloud-init"

  -- Check if disk already exists with this path
  mExisting <- liftIO $ runSqlPool (getBy (UniqueImagePath isoPath)) pool
  case mExisting of
    Just (Entity diskId _) -> do
      -- Disk exists, ensure it's attached
      logDebugN $ "Cloud-init disk already registered: " <> T.pack (show $ fromSqlKey diskId)
      ensureDiskAttached pool vmKey diskId
    Nothing -> do
      -- Register new disk
      now <- liftIO getCurrentTime
      diskId <-
        liftIO $
          runSqlPool
            ( insert
                DiskImage
                  { diskImageName = diskName
                  , diskImageFilePath = isoPath
                  , diskImageFormat = FormatRaw
                  , diskImageSizeMb = Nothing
                  , diskImageCreatedAt = now
                  , diskImageBackingImageId = Nothing
                  }
            )
            pool
      logInfoN $ "Registered cloud-init disk with ID: " <> T.pack (show $ fromSqlKey diskId)
      ensureDiskAttached pool vmKey diskId

-- | Ensure a disk is attached to a VM as CDROM
ensureDiskAttached
  :: Pool SqlBackend
  -> VmId
  -> DiskImageId
  -> LoggingT IO ()
ensureDiskAttached pool vmKey diskId = do
  -- Check if already attached
  existing <-
    liftIO $
      runSqlPool
        (selectList [M.DriveVmId ==. vmKey, M.DriveDiskImageId ==. diskId] [])
        pool
  case existing of
    (_ : _) -> logDebugN "Cloud-init disk already attached"
    [] -> do
      -- Attach as CDROM
      driveId <-
        liftIO $
          runSqlPool
            ( insert
                Drive
                  { driveVmId = vmKey
                  , driveDiskImageId = diskId
                  , driveInterface = InterfaceIde
                  , driveMedia = Just MediaCdrom
                  , driveReadOnly = True
                  , driveCacheType = CacheNone
                  , driveDiscard = False
                  }
            )
            pool
      logInfoN $ "Attached cloud-init disk as CDROM, drive ID: " <> T.pack (show $ fromSqlKey driveId)
