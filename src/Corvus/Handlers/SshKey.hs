{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SSH key management handlers.
-- Handles SSH key CRUD operations, VM associations, and cloud-init ISO generation.
module Corvus.Handlers.SshKey
  ( -- * SSH key handlers
    handleSshKeyCreate,
    handleSshKeyDelete,
    handleSshKeyList,

    -- * VM association handlers
    handleSshKeyAttach,
    handleSshKeyDetach,
    handleSshKeyListForVm,
  )
where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logDebugN, logInfoN, logWarnN, runStdoutLoggingT)
import Corvus.CloudInit
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Protocol
import Corvus.Qemu.Config (defaultQemuConfig)
import Corvus.Types
import Data.Int (Int64)
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
handleSshKeyCreate state name publicKey = runStdoutLoggingT $ do
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
                  { sshKeyName = name,
                    sshKeyPublicKey = publicKey,
                    sshKeyCreatedAt = now
                  }
            )
            pool
      logInfoN $ "Created SSH key with ID: " <> T.pack (show $ fromSqlKey keyId)
      pure $ RespSshKeyCreated $ fromSqlKey keyId

-- | Delete an SSH key (fails if attached to any VMs)
handleSshKeyDelete :: ServerState -> Int64 -> IO Response
handleSshKeyDelete state keyId = runStdoutLoggingT $ do
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
          -- Key is in use
          let vmIds = map (fromSqlKey . vmSshKeyVmId . entityVal) attachments
          logWarnN $ "SSH key is attached to VMs: " <> T.pack (show vmIds)
          pure $ RespSshKeyInUse vmIds

-- | List all SSH keys
handleSshKeyList :: ServerState -> IO Response
handleSshKeyList state = runStdoutLoggingT $ do
  logDebugN "Listing all SSH keys"

  let pool = ssDbPool state

  keys <- liftIO $ runSqlPool (selectList [] [Asc SshKeyName]) pool
  infos <- liftIO $ forM keys $ \(Entity keyId key) -> do
    attachments <- runSqlPool (selectList [VmSshKeySshKeyId ==. keyId] []) pool
    let vmIds = map (fromSqlKey . vmSshKeyVmId . entityVal) attachments
    pure
      SshKeyInfo
        { skiId = fromSqlKey keyId,
          skiName = sshKeyName key,
          skiPublicKey = sshKeyPublicKey key,
          skiCreatedAt = sshKeyCreatedAt key,
          skiAttachedVms = vmIds
        }
  pure $ RespSshKeyList infos

--------------------------------------------------------------------------------
-- VM Association Handlers
--------------------------------------------------------------------------------

-- | Attach an SSH key to a VM
-- This also regenerates the cloud-init ISO and registers/attaches it as a disk
handleSshKeyAttach :: ServerState -> Int64 -> Int64 -> IO Response
handleSshKeyAttach state vmId keyId = runStdoutLoggingT $ do
  logInfoN $ "Attaching SSH key " <> T.pack (show keyId) <> " to VM " <> T.pack (show vmId)

  let pool = ssDbPool state
  let vmKey = toSqlKey vmId :: VmId
  let sshKeyKey = toSqlKey keyId :: SshKeyId

  -- Check VM exists
  mVm <- liftIO $ runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm -> do
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
                        { vmSshKeyVmId = vmKey,
                          vmSshKeySshKeyId = sshKeyKey
                        }
                  )
                  pool
              logInfoN "SSH key attached"

              -- Regenerate cloud-init ISO
              result <- liftIO $ regenerateCloudInitIso state vmId (vmName vm)
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
handleSshKeyDetach state vmId keyId = runStdoutLoggingT $ do
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

          -- Regenerate or remove cloud-init ISO
          result <- liftIO $ regenerateCloudInitIso state vmId (vmName vm)
          case result of
            Left err -> do
              logWarnN $ "Failed to regenerate cloud-init ISO: " <> err
              pure $ RespError $ "Key detached but ISO update failed: " <> err
            Right _ -> do
              logInfoN "Cloud-init ISO updated"
              pure RespSshKeyOk

-- | List SSH keys attached to a VM
handleSshKeyListForVm :: ServerState -> Int64 -> IO Response
handleSshKeyListForVm state vmId = runStdoutLoggingT $ do
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
            let allVmIds = map (fromSqlKey . vmSshKeyVmId . entityVal) allAttachments
            pure $
              Just
                SshKeyInfo
                  { skiId = fromSqlKey sshKeyKey,
                    skiName = sshKeyName key,
                    skiPublicKey = sshKeyPublicKey key,
                    skiCreatedAt = sshKeyCreatedAt key,
                    skiAttachedVms = allVmIds
                  }
      pure $ RespSshKeyList $ map (\(Just x) -> x) $ filter (/= Nothing) infos

--------------------------------------------------------------------------------
-- Cloud-Init ISO Management
--------------------------------------------------------------------------------

-- | Regenerate cloud-init ISO for a VM
-- If no SSH keys are attached, removes the existing ISO and disk registration
-- Otherwise, generates a new ISO and ensures it's registered as a disk
regenerateCloudInitIso :: ServerState -> Int64 -> Text -> IO (Either Text ())
regenerateCloudInitIso state vmId vmName = do
  let pool = ssDbPool state
  let vmKey = toSqlKey vmId :: VmId

  -- Get all SSH keys attached to this VM
  attachments <- runSqlPool (selectList [VmSshKeyVmId ==. vmKey] []) pool
  sshKeys <- forM attachments $ \(Entity _ attachment) -> do
    mKey <- runSqlPool (get (vmSshKeySshKeyId attachment)) pool
    pure $ fmap sshKeyPublicKey mKey

  let publicKeys = [k | Just k <- sshKeys]

  if null publicKeys
    then do
      -- No keys, remove ISO if it exists
      removeCloudInitIsoForVm vmName
      pure $ Right ()
    else do
      -- Generate new ISO
      vmDir <- getCloudInitDir defaultQemuConfig vmName
      let config =
            defaultCloudInitConfig
              { ciHostname = vmName,
                ciInstanceId = "corvus-" <> T.pack (show vmId)
              }
      result <- generateCloudInitIso vmDir config publicKeys
      case result of
        Left err -> pure $ Left err
        Right isoPath -> do
          -- Ensure disk is registered
          ensureCloudInitDiskRegistered state vmId vmName (T.pack isoPath)
          pure $ Right ()

-- | Remove cloud-init ISO for a VM
removeCloudInitIsoForVm :: Text -> IO ()
removeCloudInitIsoForVm vmName = do
  isoPath <- getCloudInitIsoPath defaultQemuConfig vmName
  exists <- doesFileExist isoPath
  if exists
    then removeFile isoPath
    else pure ()

-- | Ensure cloud-init disk is registered and attached to VM
ensureCloudInitDiskRegistered :: ServerState -> Int64 -> Text -> Text -> IO ()
ensureCloudInitDiskRegistered state vmId vmName isoPath = runStdoutLoggingT $ do
  let pool = ssDbPool state
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
                  { diskImageName = diskName,
                    diskImageFilePath = isoPath,
                    diskImageFormat = FormatRaw,
                    diskImageSizeMb = Nothing,
                    diskImageCreatedAt = now
                  }
            )
            pool
      logInfoN $ "Registered cloud-init disk with ID: " <> T.pack (show $ fromSqlKey diskId)
      ensureDiskAttached pool vmKey diskId

-- | Ensure a disk is attached to a VM as CDROM
ensureDiskAttached ::
  Pool SqlBackend ->
  VmId ->
  DiskImageId ->
  LoggingT IO ()
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
                  { driveVmId = vmKey,
                    driveDiskImageId = diskId,
                    driveInterface = InterfaceIde,
                    driveMedia = Just MediaCdrom,
                    driveReadOnly = True,
                    driveCacheType = CacheNone,
                    driveDiscard = False
                  }
            )
            pool
      logInfoN $ "Attached cloud-init disk as CDROM, drive ID: " <> T.pack (show $ fromSqlKey driveId)

