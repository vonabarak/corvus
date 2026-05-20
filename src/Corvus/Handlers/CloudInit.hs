{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cloud-init configuration handlers.
-- Handles CRUD operations for custom cloud-init configs per VM,
-- and the RegenerateCloudInit action for ISO generation.
module Corvus.Handlers.CloudInit
  ( -- * Action types
    CloudInitSet (..)
  , CloudInitDelete (..)
  , RegenerateCloudInit (..)

    -- * Handlers
  , handleCloudInitSet
  , handleCloudInitGet
  , handleCloudInitDelete
  )
where

import Corvus.Action

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel, LoggingT, logDebugN, logInfoN, logWarnN)
import Corvus.CloudInit
  ( CloudInitConfig (..)
  , defaultCloudInitConfig
  , generateMetaData
  , getCloudInitDir
  , renderUserData
  )
import Corvus.Handlers.Disk.Db (recordDiskImageNode)
import Corvus.Handlers.Disk.Path (makeRelativeToBase)
import Corvus.Model
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol
import Corvus.Qemu.Config (QemuConfig, getEffectiveBasePath)
import Corvus.Types
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (SqlBackend, runSqlPool)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

-- | Set or update cloud-init config for a VM
handleCloudInitSet :: ServerState -> Int64 -> Maybe Text -> Maybe Text -> Bool -> IO Response
handleCloudInitSet state vmId mUserData mNetworkConfig injectKeys = runServerLogging state $ do
  logInfoN $ "Setting cloud-init config for VM " <> T.pack (show vmId)

  let pool = ssDbPool state
  let vmKey = toSqlKey vmId :: VmId

  -- Check VM exists and has cloud-init enabled
  mVm <- liftIO $ runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm
      | not (vmCloudInit vm) ->
          pure $ RespError "Cloud-init is not enabled on this VM"
      | otherwise -> do
          -- Upsert the cloud-init config
          liftIO $
            runSqlPool
              ( upsertBy
                  (UniqueCloudInitVm vmKey)
                  CloudInit
                    { cloudInitVmId = vmKey
                    , cloudInitUserData = mUserData
                    , cloudInitNetworkConfig = mNetworkConfig
                    , cloudInitInjectSshKeys = injectKeys
                    }
                  [ CloudInitUserData =. mUserData
                  , CloudInitNetworkConfig =. mNetworkConfig
                  , CloudInitInjectSshKeys =. injectKeys
                  ]
              )
              pool
          -- Regenerate ISO with new config
          ciResp <- liftIO $ runAction state (RegenerateCloudInit vmId (vmName vm))
          case ciResp of
            RespError err -> pure $ RespError $ "Cloud-init ISO regeneration failed: " <> err
            _ -> pure RespCloudInitOk

-- | Get cloud-init config for a VM
handleCloudInitGet :: ServerState -> Int64 -> IO Response
handleCloudInitGet state vmId = do
  let pool = ssDbPool state
  let vmKey = toSqlKey vmId :: VmId

  mVm <- runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just _ -> do
      mConfig <- runSqlPool (getBy (UniqueCloudInitVm vmKey)) pool
      case mConfig of
        Nothing -> pure $ RespCloudInitConfig Nothing
        Just (Entity _ ci) ->
          pure $
            RespCloudInitConfig $
              Just
                CloudInitInfo
                  { ciiUserData = cloudInitUserData ci
                  , ciiNetworkConfig = cloudInitNetworkConfig ci
                  , ciiInjectSshKeys = cloudInitInjectSshKeys ci
                  }

-- | Delete custom cloud-init config for a VM (revert to defaults)
handleCloudInitDelete :: ServerState -> Int64 -> IO Response
handleCloudInitDelete state vmId = runServerLogging state $ do
  logInfoN $ "Deleting cloud-init config for VM " <> T.pack (show vmId)

  let pool = ssDbPool state
  let vmKey = toSqlKey vmId :: VmId

  mVm <- liftIO $ runSqlPool (get vmKey) pool
  case mVm of
    Nothing -> pure RespVmNotFound
    Just vm
      | not (vmCloudInit vm) ->
          pure $ RespError "Cloud-init is not enabled on this VM"
      | otherwise -> do
          -- Delete the custom config
          liftIO $ runSqlPool (deleteBy (UniqueCloudInitVm vmKey)) pool
          -- Regenerate ISO with defaults
          ciResp <- liftIO $ runAction state (RegenerateCloudInit vmId (vmName vm))
          case ciResp of
            RespError err -> pure $ RespError $ "Cloud-init ISO regeneration failed: " <> err
            _ -> pure RespCloudInitOk

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

data CloudInitSet = CloudInitSet
  { cisVmId :: Int64
  , cisUserData :: Maybe Text
  , cisNetworkConfig :: Maybe Text
  , cisInjectKeys :: Bool
  }

instance Action CloudInitSet where
  actionSubsystem _ = SubVm
  actionCommand _ = "cloud-init-set"
  actionEntityId = Just . fromIntegral . cisVmId
  actionExecute ctx a = handleCloudInitSet (acState ctx) (cisVmId a) (cisUserData a) (cisNetworkConfig a) (cisInjectKeys a)

newtype CloudInitDelete = CloudInitDelete {cidVmId :: Int64}

instance Action CloudInitDelete where
  actionSubsystem _ = SubVm
  actionCommand _ = "cloud-init-delete"
  actionEntityId = Just . fromIntegral . cidVmId
  actionExecute ctx a = handleCloudInitDelete (acState ctx) (cidVmId a)

-- | Regenerate cloud-init ISO for a VM (collects SSH keys + config from DB).
data RegenerateCloudInit = RegenerateCloudInit
  { rciVmId :: Int64
  , rciVmName :: Text
  }

instance Action RegenerateCloudInit where
  actionSubsystem _ = SubVm
  actionCommand _ = "cloud-init"
  actionEntityName = Just . rciVmName
  actionExecute ctx a = do
    let state = acState ctx
    result <- regenerateCloudInitIsoForVm state (rciVmId a) (rciVmName a)
    pure $ either RespError (const RespOk) result

--------------------------------------------------------------------------------
-- Internal: cloud-init ISO lifecycle (not exported)
--------------------------------------------------------------------------------

-- | Regenerate the cloud-init ISO for a VM.
-- Collects SSH keys and custom cloud-init config from DB, asks
-- the node agent to assemble the ISO, then ensures the ISO is
-- registered as a disk and attached to the VM. Fails if the
-- node agent is unreachable.
regenerateCloudInitIsoForVm :: ServerState -> Int64 -> Text -> IO (Either Text ())
regenerateCloudInitIsoForVm state vmId vmName = do
  let qemuConfig = ssQemuConfig state
      pool = ssDbPool state
      logLevel = ssLogLevel state
      vmKey = toSqlKey vmId :: VmId

  -- Get all SSH keys attached to this VM
  attachments <- runSqlPool (selectList [VmSshKeyVmId ==. vmKey] []) pool
  sshKeys <- forM attachments $ \(Entity _ attachment) -> do
    mKey <- runSqlPool (get (vmSshKeySshKeyId attachment)) pool
    pure $ fmap sshKeyPublicKey mKey

  let publicKeys = catMaybes sshKeys

  -- Check for custom cloud-init config
  mCustomConfig <- runSqlPool (getBy (UniqueCloudInitVm vmKey)) pool

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
      userData = renderUserData config publicKeys
      metaData = generateMetaData config
      mNet = ciNetworkConfig config

  outer <- withVmNodeAgent state vmId $ \nac ->
    NOA.cloudInitGenerateIso nac (T.pack vmDir) userData metaData mNet
  case outer of
    Left err -> do
      runFilteredLogging logLevel $
        logWarnN $
          "nodeagent unavailable; cannot regenerate cloud-init ISO: " <> err
      pure $ Left err
    Right result -> case result of
      Left e -> pure $ Left (T.pack (show e))
      Right isoPath -> do
        ensureCloudInitDiskRegistered pool qemuConfig vmId vmName isoPath logLevel
        pure $ Right ()

-- | Ensure cloud-init disk is registered and attached to VM as CDROM.
--
-- Records both the logical 'DiskImage' row and the per-node
-- 'DiskImageNode' placement keyed on the VM's node. Without
-- the placement row, 'assembleVmSpec' would silently filter
-- the drive out of 'VmSpec.vsDrives' (because the join lookup
-- on @(image, vm.nodeId)@ returns 'Nothing'), and the VM would
-- start without its NoCloud ISO mounted — meaning cloud-init
-- inside the guest sees no metadata source and no SSH keys
-- ever get injected.
ensureCloudInitDiskRegistered :: Pool SqlBackend -> QemuConfig -> Int64 -> Text -> Text -> LogLevel -> IO ()
ensureCloudInitDiskRegistered pool qemuConfig vmId vmName isoPath logLevel = runFilteredLogging logLevel $ do
  let vmKey = toSqlKey vmId :: VmId
  let diskName = vmName <> "-cloud-init"
  basePath <- liftIO $ getEffectiveBasePath qemuConfig
  let storedPath = makeRelativeToBase basePath (T.unpack isoPath)
  -- Pull the VM's node id so the placement row points at the
  -- right node. A missing VM row here means the caller has
  -- raced cloud-init regen with a delete; bail without a
  -- placement (the VM is going away anyway).
  mVm <- liftIO $ runSqlPool (get vmKey) pool
  let mNodeKey = vmNodeId <$> mVm

  mExisting <- liftIO $ runSqlPool (getBy (UniqueDiskImageName diskName)) pool
  case mExisting of
    Just (Entity diskId _) -> do
      logDebugN $ "Cloud-init disk already registered: " <> T.pack (show $ fromSqlKey diskId)
      mapM_ (recordPlacement diskId storedPath) mNodeKey
      ensureDiskAttached pool vmKey diskId
    Nothing -> do
      now <- liftIO getCurrentTime
      diskId <-
        liftIO $
          runSqlPool
            ( insert
                DiskImage
                  { diskImageName = diskName
                  , diskImageFormat = FormatRaw
                  , diskImageSizeMb = Nothing
                  , diskImageCreatedAt = now
                  , diskImageBackingImageId = Nothing
                  }
            )
            pool
      logInfoN $ "Registered cloud-init disk with ID: " <> T.pack (show $ fromSqlKey diskId)
      mapM_ (recordPlacement diskId storedPath) mNodeKey
      ensureDiskAttached pool vmKey diskId
  where
    recordPlacement diskId path nodeKey =
      liftIO $
        runSqlPool (recordDiskImageNode diskId nodeKey path) pool

-- | Ensure a disk is attached to a VM as CDROM
ensureDiskAttached :: Pool SqlBackend -> VmId -> DiskImageId -> LoggingT IO ()
ensureDiskAttached pool vmKey diskId = do
  existing <-
    liftIO $
      runSqlPool
        (selectList [DriveVmId ==. vmKey, DriveDiskImageId ==. diskId] [])
        pool
  case existing of
    (_ : _) -> logDebugN "Cloud-init disk already attached"
    [] -> do
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
