{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Derive disk-image handlers.
module Corvus.Handlers.Disk.Derive
  ( DiskCreateOverlay (..)
  , DiskClone (..)
  )
where

import Corvus.Action
import Corvus.Images

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import Corvus.Handlers.Disk.Agent
  ( cloneImageViaAgent
  , createImageViaAgent
  , createOverlayViaAgent
  , deleteImageViaAgent
  , getImageInfoViaAgent
  , getImageSizeViaAgent
  , resizeImageViaAgent
  )
import Corvus.Handlers.Disk.Attach (DiskAttach (..), DiskDetachByDisk (..), handleDiskAttach, handleDiskDetach)
import Corvus.Handlers.Disk.Db (deleteDiskAndSnapshots, deleteDiskImageNodeRow, diskImageNodeFilePathFor, getAttachedVms, getBackingChainIds, getDiskImageInfo, getOverlayIds, getReadWriteAttachedVms, getRunningAttachedVms, hasPlacementOnNode, listDiskImageNodes, listDiskImages, recordDiskImageNode)
import Corvus.Handlers.Disk.Import (DiskImportAction (..), handleDiskImportCopy)
import Corvus.Handlers.Disk.Path (makeRelativeToBase, resolveDiskFilePath, resolveDiskFilePathPure, resolveDiskPath, sanitizeDiskName)
import Corvus.Handlers.Disk.Rebase (DiskRebase (..), handleDiskRebase)
import Corvus.Handlers.Disk.Snapshot (SnapshotCreate (..), SnapshotDelete (..), SnapshotMerge (..), SnapshotRollback (..), handleSnapshotCreate, handleSnapshotDelete, handleSnapshotList, handleSnapshotMerge, handleSnapshotRollback)
import Corvus.Handlers.Disk.Transfer (stageBackingChain, transferImageBetweenNodes)
import Corvus.Handlers.Resolve (ResolveError (..), resolveErrorMessage, resolveNode, validateName)
import Corvus.Handlers.Scheduler (pickNodeForDisk, pickNodeForExistingDisk)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Node.Image (ImageInfo (..), ImageResult (..), detectFormatFromPath)
import Corvus.Protocol
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (runSqlPool)
import System.FilePath (takeExtension, takeFileName, (</>))

import Corvus.Handlers.Disk.Placement (nodeBasePathFor, withSelectedDiskNode)

-- | Create a qcow2 overlay backed by an existing disk image
handleDiskCreateOverlay :: ServerState -> T.Text -> Int64 -> Maybe Int64 -> Maybe T.Text -> Bool -> IO Response
handleDiskCreateOverlay state name baseDiskId mResize optDirPath ephemeral = runServerLogging state $ do
  logInfoN $ "Creating overlay '" <> name <> "' backed by disk " <> T.pack (show baseDiskId)

  case sanitizeDiskName name of
    Left err -> do
      logWarnN $ "Invalid overlay name: " <> err
      pure $ RespError err
    Right safeName -> do
      -- The overlay must sit on the same node as its backing image
      -- — qemu can't open a backing chain across hosts. Pick from
      -- the base's existing placements rather than first-online.
      mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey baseDiskId :: DiskImageId)
      case mNid of
        Left err -> pure $ RespError err
        Right nid -> do
          mBaseDisk <- liftIO $ runSqlPool (get (toSqlKey baseDiskId :: DiskImageId)) (ssDbPool state)
          case mBaseDisk of
            Nothing -> pure RespDiskNotFound
            Just baseDisk -> do
              rwDrives <-
                liftIO $
                  runSqlPool
                    ( selectList
                        [ M.DriveDiskImageId ==. Just (toSqlKey baseDiskId)
                        , M.DriveReadOnly ==. False
                        ]
                        []
                    )
                    (ssDbPool state)
              if not (null rwDrives)
                then do
                  let vmIds = map (fromSqlKey . driveVmId . entityVal) rwDrives
                  logWarnN $ "Base image is attached read-write to VMs: " <> T.pack (show vmIds)
                  pure $ RespError "Cannot use as base: image is attached read-write to VM(s)"
                else do
                  reservedId <- liftIO $ runSqlPool reserveImageId (ssDbPool state)
                  basePath <- liftIO $ nodeBasePathFor state nid
                  let overlayFileName = T.unpack safeName <> ".qcow2"
                  overlayFilePath <- liftIO $ resolveDiskFilePath reservedId basePath optDirPath overlayFileName
                  let pool = ssDbPool state
                      baseKey = toSqlKey baseDiskId :: DiskImageId
                  baseFilePath <- liftIO $ resolveDiskPath pool (ssQemuConfig state) baseKey nid
                  result <- liftIO $ createOverlayViaAgent state nid overlayFilePath baseFilePath (diskImageFormat baseDisk)
                  case result of
                    ImageError err -> do
                      logWarnN $ "Failed to create overlay: " <> err
                      pure $ RespError err
                    ImageSuccess -> do
                      resized <- case mResize of
                        Nothing -> pure ImageSuccess
                        Just size -> liftIO $ resizeImageViaAgent state nid overlayFilePath (fromIntegral size)
                      case resized of
                        ImageSuccess -> do
                          actualSize <- liftIO $ getImageSizeViaAgent state nid overlayFilePath
                          now <- liftIO getCurrentTime
                          let storedOverlay = makeRelativeToBase basePath overlayFilePath
                          diskId <-
                            liftIO $
                              runSqlPool
                                ( do
                                    dkey <-
                                      publishImageWithId
                                        reservedId
                                        DiskImage
                                          { diskImageName = safeName
                                          , diskImageFormat = FormatQcow2
                                          , diskImageSize = actualSize
                                          , diskImageCreatedAt = now
                                          , diskImageBackingImageId = Just (toSqlKey baseDiskId)
                                          , diskImageEphemeral = ephemeral
                                          }
                                    recordDiskImageNode dkey nid storedOverlay
                                    pure dkey
                                )
                                (ssDbPool state)
                          logInfoN $ "Created overlay with ID: " <> T.pack (show $ fromSqlKey diskId)
                          pure $ RespDiskCreated $ fromSqlKey diskId
                        ImageError err -> pure $ RespError err
                        ImageFormatNotSupported err -> pure $ RespFormatNotSupported err
                        ImageNotFound -> pure $ RespError "Derived image vanished before publication"
                    ImageFormatNotSupported err -> pure $ RespFormatNotSupported err
                    ImageNotFound -> pure $ RespError "Base image file not found"

-- | Clone a disk image
handleDiskClone :: ServerState -> Text -> Int64 -> Maybe Int64 -> Maybe Text -> Bool -> IO Response
handleDiskClone state name baseDiskId mResize optionalPath ephemeral = runServerLogging state $ do
  logInfoN $ "Cloning disk image " <> T.pack (show baseDiskId) <> " to '" <> name <> "'"

  case sanitizeDiskName name of
    Left err -> do
      logWarnN $ "Invalid disk name: " <> err
      pure $ RespError err
    Right safeName -> do
      -- Clone reads the source file with qemu-img convert and writes
      -- the destination on the same node, so the picker must land
      -- on a node where the source already lives.
      mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey baseDiskId :: DiskImageId)
      case mNid of
        Left err -> pure $ RespError err
        Right nid -> do
          mBaseDisk <- liftIO $ runSqlPool (get (toSqlKey baseDiskId :: DiskImageId)) (ssDbPool state)
          case mBaseDisk of
            Nothing -> pure RespDiskNotFound
            Just baseDisk -> do
              -- Check if any attached VM is running or paused
              runningVms <- liftIO $ runSqlPool (getRunningAttachedVms baseDiskId) (ssDbPool state)
              if not (null runningVms)
                then pure RespVmMustBeStopped
                else do
                  reservedId <- liftIO $ runSqlPool reserveImageId (ssDbPool state)
                  basePath <- liftIO $ nodeBasePathFor state nid
                  let pool = ssDbPool state
                      baseKey = toSqlKey baseDiskId :: DiskImageId
                  srcPath <- liftIO $ resolveDiskPath pool (ssQemuConfig state) baseKey nid
                  let srcFileName = takeFileName srcPath
                      ext = takeExtension srcFileName
                      cloneFileName = T.unpack safeName <> ext
                  destPath <- liftIO $ resolveDiskFilePath reservedId basePath optionalPath cloneFileName
                  result <- liftIO $ cloneImageViaAgent state nid srcPath destPath (diskImageFormat baseDisk)
                  case result of
                    ImageError err -> do
                      logWarnN $ "Failed to clone image: " <> err
                      pure $ RespError err
                    ImageNotFound -> pure $ RespError "Source image file not found"
                    ImageSuccess -> do
                      resized <- case mResize of
                        Nothing -> pure ImageSuccess
                        Just size -> liftIO $ resizeImageViaAgent state nid destPath (fromIntegral size)
                      case resized of
                        ImageSuccess -> do
                          actualSize <- liftIO $ getImageSizeViaAgent state nid destPath
                          now <- liftIO getCurrentTime
                          let storedDest = makeRelativeToBase basePath destPath
                          newDiskId <-
                            liftIO $
                              runSqlPool
                                ( do
                                    dId <-
                                      publishImageWithId
                                        reservedId
                                        DiskImage
                                          { diskImageName = safeName
                                          , diskImageFormat = diskImageFormat baseDisk
                                          , diskImageSize = actualSize
                                          , diskImageCreatedAt = now
                                          , diskImageBackingImageId = diskImageBackingImageId baseDisk
                                          , diskImageEphemeral = ephemeral
                                          }
                                    recordDiskImageNode dId nid storedDest
                                    -- Clone snapshots as well
                                    baseSnapshots <- selectList [SnapshotDiskImageId ==. toSqlKey baseDiskId] []
                                    forM_ baseSnapshots $ \snapEntity -> do
                                      let snap = entityVal snapEntity
                                      insert snap {snapshotDiskImageId = dId}
                                    pure dId
                                )
                                (ssDbPool state)
                          logInfoN $ "Cloned disk image with ID: " <> T.pack (show $ fromSqlKey newDiskId)
                          pure $ RespDiskCreated $ fromSqlKey newDiskId
                        ImageError err -> pure $ RespError err
                        ImageFormatNotSupported err -> pure $ RespFormatNotSupported err
                        ImageNotFound -> pure $ RespError "Derived image vanished before publication"
                    ImageFormatNotSupported err -> pure $ RespFormatNotSupported err

data DiskCreateOverlay = DiskCreateOverlay
  { dcoName :: Text
  , dcoBaseDiskId :: Int64
  , dcoResize :: Maybe Int64
  , dcoPath :: Maybe Text
  , dcoEphemeral :: Bool
  }

instance Action DiskCreateOverlay where
  actionSubsystem _ = SubDisk
  actionCommand _ = "overlay"
  actionEntityName = Just . dcoName
  actionExecute ctx a = handleDiskCreateOverlay (acState ctx) (dcoName a) (dcoBaseDiskId a) (dcoResize a) (dcoPath a) (dcoEphemeral a)

data DiskClone = DiskClone
  { dclName :: Text
  , dclBaseDiskId :: Int64
  , dclResize :: Maybe Int64
  , dclPath :: Maybe Text
  , dclEphemeral :: Bool
  }

instance Action DiskClone where
  actionSubsystem _ = SubDisk
  actionCommand _ = "clone"
  actionEntityName = Just . dclName
  actionExecute ctx a = handleDiskClone (acState ctx) (dclName a) (dclBaseDiskId a) (dclResize a) (dclPath a) (dclEphemeral a)
