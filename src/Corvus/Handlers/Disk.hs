{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Disk image management handlers.
-- Handles disk image CRUD operations, snapshots, and attach/detach.
module Corvus.Handlers.Disk
  ( -- * Action types
    DiskCreate (..)
  , DiskCreateOverlay (..)
  , DiskRegister (..)
  , DiskImportUrl (..)
  , DiskDelete (..)
  , DiskResize (..)
  , DiskClone (..)
  , DiskRefresh (..)
  , SnapshotCreate (..)
  , SnapshotDelete (..)
  , SnapshotRollback (..)
  , SnapshotMerge (..)
  , DiskAttach (..)
  , DiskDetachByDisk (..)
  , DiskRebase (..)
  , DiskImportAction (..)

    -- * Disk image handlers
  , handleDiskCreate
  , handleDiskCreateOverlay
  , handleDiskRegister
  , handleDiskImportUrl
  , handleDiskImportCopy
  , handleDiskDelete
  , handleDiskResize
  , handleDiskList
  , handleDiskShow
  , handleDiskClone
  , handleDiskRebase
  , handleDiskRefresh

    -- * Snapshot handlers
  , handleSnapshotCreate
  , handleSnapshotDelete
  , handleSnapshotRollback
  , handleSnapshotMerge
  , handleSnapshotList

    -- * Attach/detach handlers
  , handleDiskAttach
  , handleDiskDetach
  )
where

import Corvus.Action

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Handlers.Disk.Agent
  ( cloneImageViaAgent
  , createImageViaAgent
  , createOverlayViaAgent
  , deleteImageViaAgent
  , getImageInfoViaAgent
  , getImageSizeMbViaAgent
  , resizeImageViaAgent
  )
import Corvus.Handlers.Disk.Attach (DiskAttach (..), DiskDetachByDisk (..), handleDiskAttach, handleDiskDetach)
import Corvus.Handlers.Disk.Db (deleteDiskAndSnapshots, deleteDiskImageNodeRow, diskImageNodeFilePathFor, getAttachedVms, getDiskImageInfo, getOverlayIds, getRunningAttachedVms, listDiskImageNodes, listDiskImages, recordDiskImageNode)
import Corvus.Handlers.Disk.Import (DiskImportAction (..), DiskImportUrl (..), handleDiskImportCopy, handleDiskImportUrl)
import Corvus.Handlers.Disk.Path (makeRelativeToBase, resolveDiskFilePath, resolveDiskFilePathPure, resolveDiskPath, sanitizeDiskName)
import Corvus.Handlers.Disk.Rebase (DiskRebase (..), handleDiskRebase)
import Corvus.Handlers.Disk.Snapshot (SnapshotCreate (..), SnapshotDelete (..), SnapshotMerge (..), SnapshotRollback (..), handleSnapshotCreate, handleSnapshotDelete, handleSnapshotList, handleSnapshotMerge, handleSnapshotRollback)
import Corvus.Handlers.Resolve (validateName)
import Corvus.Handlers.Scheduler (pickNodeForDisk)
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
import Database.Persist.Postgresql (runSqlPool)
import System.FilePath (takeExtension, takeFileName, (</>))

--------------------------------------------------------------------------------
-- Disk Image Handlers
--------------------------------------------------------------------------------

-- | Create a new disk image
handleDiskCreate :: ServerState -> Text -> DriveFormat -> Int64 -> Maybe Text -> IO Response
handleDiskCreate state name format sizeMb mPath = runServerLogging state $ do
  logInfoN $ "Creating disk image: " <> name <> " (" <> T.pack (show sizeMb) <> " MB)"

  -- Sanitize the name to prevent path traversal attacks
  case sanitizeDiskName name of
    Left err -> do
      logWarnN $ "Invalid disk name: " <> err
      pure $ RespError err
    Right safeName -> do
      -- TODO(multi-node Phase 3): refine pickNodeForDisk with
      -- DiskImageNode-aware placement instead of first-online-node.
      mNid <- liftIO $ pickNodeForDisk state
      case mNid of
        Left err -> pure $ RespError err
        Right nid -> do
          -- Generate file path using sanitized name
          basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
          let fileName = T.unpack safeName <> "." <> T.unpack (enumToText format)
          filePath <- liftIO $ resolveDiskFilePath basePath mPath fileName

          -- Create the actual image file
          result <- liftIO $ createImageViaAgent state nid filePath format sizeMb
          case result of
            ImageError err -> do
              logWarnN $ "Failed to create image: " <> err
              pure $ RespError err
            ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
            ImageNotFound -> pure $ RespError "Unexpected error during creation"
            ImageSuccess -> do
              -- Store in database with original name for display, but sanitized path
              now <- liftIO getCurrentTime
              let storedPath = makeRelativeToBase basePath filePath
              diskId <-
                liftIO $
                  runSqlPool
                    ( do
                        dkey <-
                          insert
                            DiskImage
                              { diskImageName = safeName
                              , diskImageFormat = format
                              , diskImageSizeMb = Just (fromIntegral sizeMb)
                              , diskImageCreatedAt = now
                              , diskImageBackingImageId = Nothing
                              }
                        recordDiskImageNode dkey nid storedPath
                        pure dkey
                    )
                    (ssDbPool state)
              logInfoN $ "Created disk image with ID: " <> T.pack (show $ fromSqlKey diskId)
              pure $ RespDiskCreated $ fromSqlKey diskId

-- | Register an existing disk image file.
-- Format and size are auto-detected via qemu-img info.
-- If format is provided, it is used instead of auto-detection.
handleDiskRegister
  :: ServerState
  -> Text
  -> Text
  -> Maybe DriveFormat
  -> Maybe Int64
  -> IO Response
handleDiskRegister state name filePath mFormat mBackingDiskId =
  case validateName "Disk image" name of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Registering disk image: " <> name <> " at " <> filePath

      -- TODO(multi-node Phase 3): refine pickNodeForDisk with
      -- DiskImageNode-aware placement instead of first-online-node.
      mNid <- liftIO $ pickNodeForDisk state
      case mNid of
        Left err -> pure $ RespError err
        Right nid -> do
          -- Normalize path: strip base directory prefix if applicable
          basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
          let storedPath = makeRelativeToBase basePath (T.unpack filePath)
              resolvedPath =
                if "/" `isPrefixOf` T.unpack storedPath
                  then T.unpack storedPath
                  else basePath </> T.unpack storedPath

          -- Detect format if not provided
          format <- case mFormat of
            Just f -> pure f
            Nothing -> do
              mInfo <- liftIO $ getImageInfoViaAgent state nid resolvedPath
              case mInfo of
                Right info -> pure $ iiFormat info
                Left err -> do
                  -- Fall back to extension-based detection
                  case detectFormatFromPath (T.pack resolvedPath) of
                    Just f -> pure f
                    Nothing -> do
                      logWarnN $ "Could not detect format for " <> T.pack resolvedPath <> ": " <> err
                      pure FormatRaw -- safe default

          -- Auto-detect size
          sizeMb <- liftIO $ getImageSizeMbViaAgent state nid resolvedPath

          -- Store in database
          now <- liftIO getCurrentTime
          mExisting <-
            liftIO $
              runSqlPool
                ( getBy (UniqueDiskImageName name)
                )
                (ssDbPool state)

          case mExisting of
            Just (Entity diskKey _) -> do
              -- Image already exists under this logical name —
              -- record its placement on this node so an operator
              -- replicating a file via rsync + `crv disk register
              -- --node N` ends up with a 'DiskImageNode' row for
              -- the new node without inserting a duplicate image.
              liftIO $
                runSqlPool
                  (recordDiskImageNode diskKey nid storedPath)
                  (ssDbPool state)
              logInfoN $ "Disk image already registered with ID: " <> T.pack (show $ fromSqlKey diskKey)
              pure $ RespDiskCreated $ fromSqlKey diskKey
            Nothing -> do
              result <-
                liftIO $
                  try $
                    runSqlPool
                      ( do
                          dkey <-
                            insert
                              DiskImage
                                { diskImageName = name
                                , diskImageFormat = format
                                , diskImageSizeMb = sizeMb
                                , diskImageCreatedAt = now
                                , diskImageBackingImageId = fmap toSqlKey mBackingDiskId
                                }
                          recordDiskImageNode dkey nid storedPath
                          pure dkey
                      )
                      (ssDbPool state)
              case result of
                Right diskId -> do
                  logInfoN $ "Registered disk image with ID: " <> T.pack (show $ fromSqlKey diskId)
                  pure $ RespDiskCreated $ fromSqlKey diskId
                Left (_err :: SomeException) -> do
                  -- Race: another thread inserted first. Recover
                  -- by reading the new row's key and recording
                  -- our placement against it.
                  mRetry <-
                    liftIO $
                      runSqlPool (getBy (UniqueDiskImageName name)) (ssDbPool state)
                  case mRetry of
                    Just (Entity diskKey _) -> do
                      liftIO $
                        runSqlPool
                          (recordDiskImageNode diskKey nid storedPath)
                          (ssDbPool state)
                      logInfoN $ "Disk image registered concurrently with ID: " <> T.pack (show $ fromSqlKey diskKey)
                      pure $ RespDiskCreated $ fromSqlKey diskKey
                    Nothing -> pure $ RespError $ "Failed to register disk image: " <> name

-- | Create a qcow2 overlay backed by an existing disk image
handleDiskCreateOverlay :: ServerState -> T.Text -> Int64 -> Maybe Int -> Maybe T.Text -> IO Response
handleDiskCreateOverlay state name baseDiskId mResizeMb optDirPath = runServerLogging state $ do
  logInfoN $ "Creating overlay '" <> name <> "' backed by disk " <> T.pack (show baseDiskId)

  case sanitizeDiskName name of
    Left err -> do
      logWarnN $ "Invalid overlay name: " <> err
      pure $ RespError err
    Right safeName -> do
      -- TODO(multi-node Phase 3): refine pickNodeForDisk with
      -- DiskImageNode-aware placement instead of first-online-node.
      mNid <- liftIO $ pickNodeForDisk state
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
                        [ M.DriveDiskImageId ==. toSqlKey baseDiskId
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
                  basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
                  let overlayFileName = T.unpack safeName <> ".qcow2"
                  overlayFilePath <- liftIO $ resolveDiskFilePath basePath optDirPath overlayFileName
                  -- The overlay must sit on the same node as its
                  -- backing image — qemu can't open a backing chain
                  -- across hosts. Look up the base's placement on
                  -- the target node; refuse if missing.
                  let pool = ssDbPool state
                      baseKey = toSqlKey baseDiskId :: DiskImageId
                  baseFilePath <- liftIO $ resolveDiskPath pool (ssQemuConfig state) baseKey nid
                  if null baseFilePath
                    then pure $ RespError $ "Base image '" <> diskImageName baseDisk <> "' is not present on the target node"
                    else do
                      result <- liftIO $ createOverlayViaAgent state nid overlayFilePath baseFilePath (diskImageFormat baseDisk)
                      case result of
                        ImageError err -> do
                          logWarnN $ "Failed to create overlay: " <> err
                          pure $ RespError err
                        _ -> do
                          now <- liftIO getCurrentTime
                          let storedOverlay = makeRelativeToBase basePath overlayFilePath
                          diskId <-
                            liftIO $
                              runSqlPool
                                ( do
                                    dkey <-
                                      insert
                                        DiskImage
                                          { diskImageName = safeName
                                          , diskImageFormat = FormatQcow2
                                          , diskImageSizeMb = diskImageSizeMb baseDisk
                                          , diskImageCreatedAt = now
                                          , diskImageBackingImageId = Just (toSqlKey baseDiskId)
                                          }
                                    recordDiskImageNode dkey nid storedOverlay
                                    pure dkey
                                )
                                (ssDbPool state)
                          -- Resize if requested
                          case mResizeMb of
                            Just newSize -> do
                              res <- liftIO $ resizeImageViaAgent state nid overlayFilePath (fromIntegral newSize)
                              case res of
                                ImageSuccess ->
                                  liftIO $ runSqlPool (update diskId [DiskImageSizeMb =. Just newSize]) (ssDbPool state)
                                _ -> logWarnN "Failed to resize overlay after creation"
                            Nothing -> pure ()
                          logInfoN $ "Created overlay with ID: " <> T.pack (show $ fromSqlKey diskId)
                          pure $ RespDiskCreated $ fromSqlKey diskId

-- | Clone a disk image
handleDiskClone :: ServerState -> Text -> Int64 -> Maybe Int -> Maybe Text -> IO Response
handleDiskClone state name baseDiskId mResizeMb optionalPath = runServerLogging state $ do
  logInfoN $ "Cloning disk image " <> T.pack (show baseDiskId) <> " to '" <> name <> "'"

  case sanitizeDiskName name of
    Left err -> do
      logWarnN $ "Invalid disk name: " <> err
      pure $ RespError err
    Right safeName -> do
      -- TODO(multi-node Phase 3): refine pickNodeForDisk with
      -- DiskImageNode-aware placement instead of first-online-node.
      mNid <- liftIO $ pickNodeForDisk state
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
                  basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
                  let pool = ssDbPool state
                      baseKey = toSqlKey baseDiskId :: DiskImageId
                  srcPath <- liftIO $ resolveDiskPath pool (ssQemuConfig state) baseKey nid
                  if null srcPath
                    then pure $ RespError $ "Source image '" <> diskImageName baseDisk <> "' is not present on the target node"
                    else do
                      let srcFileName = takeFileName srcPath
                          ext = takeExtension srcFileName
                          cloneFileName = T.unpack safeName <> ext
                      destPath <- liftIO $ resolveDiskFilePath basePath optionalPath cloneFileName
                      result <- liftIO $ cloneImageViaAgent state nid srcPath destPath
                      case result of
                        ImageError err -> do
                          logWarnN $ "Failed to clone image: " <> err
                          pure $ RespError err
                        ImageNotFound -> pure $ RespError "Source image file not found"
                        _ -> do
                          now <- liftIO getCurrentTime
                          let storedDest = makeRelativeToBase basePath destPath
                          newDiskId <-
                            liftIO $
                              runSqlPool
                                ( do
                                    dId <-
                                      insert
                                        DiskImage
                                          { diskImageName = safeName
                                          , diskImageFormat = diskImageFormat baseDisk
                                          , diskImageSizeMb = diskImageSizeMb baseDisk
                                          , diskImageCreatedAt = now
                                          , diskImageBackingImageId = diskImageBackingImageId baseDisk
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
                          -- Resize if requested
                          case mResizeMb of
                            Just newSize -> do
                              res <- liftIO $ resizeImageViaAgent state nid destPath (fromIntegral newSize)
                              case res of
                                ImageSuccess ->
                                  liftIO $ runSqlPool (update newDiskId [DiskImageSizeMb =. Just newSize]) (ssDbPool state)
                                _ -> logWarnN "Failed to resize clone after creation"
                            Nothing -> pure ()
                          logInfoN $ "Cloned disk image with ID: " <> T.pack (show $ fromSqlKey newDiskId)
                          pure $ RespDiskCreated $ fromSqlKey newDiskId

-- | Refresh a disk image's size by querying qemu-img info
handleDiskRefresh :: ServerState -> Int64 -> IO Response
handleDiskRefresh state diskId = runServerLogging state $ do
  logInfoN $ "Refreshing disk image size: " <> T.pack (show diskId)
  mNid <- liftIO $ pickNodeForDisk state
  case mNid of
    Left err -> pure $ RespError err
    Right nid -> do
      mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
      case mDisk of
        Nothing -> pure RespDiskNotFound
        Just _disk -> do
          let key = toSqlKey diskId :: DiskImageId
          resolvedPath <- liftIO $ resolveDiskPath (ssDbPool state) (ssQemuConfig state) key nid
          mSize <- liftIO $ getImageSizeMbViaAgent state nid resolvedPath
          case mSize of
            Nothing -> pure $ RespError "Could not determine disk image size"
            Just newSize -> do
              liftIO $
                runSqlPool
                  (update (toSqlKey diskId :: DiskImageId) [DiskImageSizeMb =. Just newSize])
                  (ssDbPool state)
              logInfoN $ "Updated size to " <> T.pack (show newSize) <> " MB"
              pure RespDiskOk

-- | Delete a disk image. Walks every 'DiskImageNode' placement
-- and asks each node's nodeagent to delete the on-disk file, then
-- drops the join rows + the logical image.
handleDiskDelete :: ServerState -> Int64 -> IO Response
handleDiskDelete state diskId = runServerLogging state $ do
  logInfoN $ "Deleting disk image: " <> T.pack (show diskId)

  let pool = ssDbPool state
      key = toSqlKey diskId :: DiskImageId
  mDisk <- liftIO $ runSqlPool (get key) pool
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just _disk -> do
      attachedVms <- liftIO $ runSqlPool (getAttachedVms diskId) pool
      if not (null attachedVms)
        then pure $ RespDiskInUse attachedVms
        else do
          overlayIds <- liftIO $ runSqlPool (getOverlayIds diskId) pool
          if not (null overlayIds)
            then pure $ RespDiskHasOverlays overlayIds
            else do
              basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
              placements <- liftIO $ runSqlPool (listDiskImageNodes key) pool
              -- Delete the on-disk file on every node we've ever
              -- recorded for this image. Per-node failures are
              -- logged but don't abort the rest of the cleanup —
              -- the operator can re-run @crv disk delete@ after
              -- the failing node comes back, and the DB drop at
              -- the end is idempotent.
              forM_ placements $ \(Entity _ row) -> do
                let nid = diskImageNodeNodeId row
                    storedPath = T.unpack (diskImageNodeFilePath row)
                    resolved =
                      if "/" `isPrefixOf` storedPath
                        then storedPath
                        else basePath </> storedPath
                result <- liftIO $ deleteImageViaAgent state nid resolved
                case result of
                  ImageError err ->
                    logWarnN $
                      "Failed to delete file on node "
                        <> T.pack (show (fromSqlKey nid))
                        <> ": "
                        <> err
                  ImageNotFound ->
                    logWarnN $
                      "Image file already gone on node "
                        <> T.pack (show (fromSqlKey nid))
                  _ -> pure ()
                liftIO $
                  runSqlPool (deleteDiskImageNodeRow key nid) pool
              liftIO $ runSqlPool (deleteDiskAndSnapshots diskId) pool
              logInfoN $ "Deleted disk image: " <> T.pack (show diskId)
              pure RespDiskOk

-- | Resize a disk image (VM must be stopped). Resizes on every
-- node that hosts a placement, then updates the logical size.
handleDiskResize :: ServerState -> Int64 -> Int64 -> IO Response
handleDiskResize state diskId newSizeMb = runServerLogging state $ do
  logInfoN $ "Resizing disk image " <> T.pack (show diskId) <> " to " <> T.pack (show newSizeMb) <> " MB"

  mNid <- liftIO $ pickNodeForDisk state
  case mNid of
    Left err -> pure $ RespError err
    Right nid -> do
      mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
      case mDisk of
        Nothing -> pure RespDiskNotFound
        Just _disk -> do
          let key = toSqlKey diskId :: DiskImageId
          -- Check if any attached VM is running
          runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
          if not (null runningVms)
            then pure RespVmMustBeStopped
            else do
              -- Check if disk is used as backing image for overlays
              overlayIds <- liftIO $ runSqlPool (getOverlayIds diskId) (ssDbPool state)
              if not (null overlayIds)
                then pure $ RespDiskHasOverlays overlayIds
                else do
                  filePath <- liftIO $ resolveDiskPath (ssDbPool state) (ssQemuConfig state) key nid
                  result <- liftIO $ resizeImageViaAgent state nid filePath newSizeMb
                  case result of
                    ImageSuccess -> do
                      -- Update size in database
                      liftIO $
                        runSqlPool
                          (update (toSqlKey diskId :: DiskImageId) [M.DiskImageSizeMb =. Just (fromIntegral newSizeMb)])
                          (ssDbPool state)
                      logInfoN "Disk resized successfully"
                      pure RespDiskOk
                    ImageError err -> do
                      logWarnN $ "Failed to resize: " <> err
                      pure $ RespError err
                    ImageNotFound -> pure RespDiskNotFound
                    ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg

-- | List all disk images. The 'diiFilePath' field is rewritten to an
-- absolute path so clients (Makefile, integration scripts) can use it
-- directly without knowing the daemon's disk base.
handleDiskList :: ServerState -> IO Response
handleDiskList state = do
  disks <- runSqlPool listDiskImages (ssDbPool state)
  basePath <- getEffectiveBasePath (ssQemuConfig state)
  pure $ RespDiskList (map (absolutizeDiskFilePath basePath) disks)

-- | Show disk image details. As with 'handleDiskList', 'diiFilePath'
-- in the response is absolute.
handleDiskShow :: ServerState -> Int64 -> IO Response
handleDiskShow state diskId = do
  mInfo <- runSqlPool (getDiskImageInfo diskId) (ssDbPool state)
  case mInfo of
    Nothing -> pure RespDiskNotFound
    Just info -> do
      basePath <- getEffectiveBasePath (ssQemuConfig state)
      pure $ RespDiskInfo (absolutizeDiskFilePath basePath info)

-- | Promote each placement's 'dipFilePath' to an absolute path.
-- The DB stores paths relative to the daemon's base; clients
-- need the absolute form.
absolutizeDiskFilePath :: FilePath -> DiskImageInfo -> DiskImageInfo
absolutizeDiskFilePath basePath info =
  info {diiPlacements = map absolutizePlacement (diiPlacements info)}
  where
    absolutizePlacement p =
      let raw = T.unpack (dipFilePath p)
          absPath = if "/" `isPrefixOf` raw then raw else basePath </> raw
       in p {dipFilePath = T.pack absPath}

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

data DiskCreate = DiskCreate
  { dcrName :: Text
  , dcrFormat :: DriveFormat
  , dcrSizeMb :: Int64
  , dcrPath :: Maybe Text
  }

instance Action DiskCreate where
  actionSubsystem _ = SubDisk
  actionCommand _ = "create"
  actionEntityName = Just . dcrName
  actionExecute ctx a = handleDiskCreate (acState ctx) (dcrName a) (dcrFormat a) (dcrSizeMb a) (dcrPath a)

data DiskCreateOverlay = DiskCreateOverlay
  { dcoName :: Text
  , dcoBaseDiskId :: Int64
  , dcoResizeMb :: Maybe Int
  , dcoPath :: Maybe Text
  }

instance Action DiskCreateOverlay where
  actionSubsystem _ = SubDisk
  actionCommand _ = "overlay"
  actionEntityName = Just . dcoName
  actionExecute ctx a = handleDiskCreateOverlay (acState ctx) (dcoName a) (dcoBaseDiskId a) (dcoResizeMb a) (dcoPath a)

data DiskRegister = DiskRegister
  { drgName :: Text
  , drgPath :: Text
  , drgFormat :: Maybe DriveFormat
  , drgBackingDiskId :: Maybe Int64
  }

instance Action DiskRegister where
  actionSubsystem _ = SubDisk
  actionCommand _ = "register"
  actionEntityName = Just . drgName
  actionExecute ctx a = handleDiskRegister (acState ctx) (drgName a) (drgPath a) (drgFormat a) (drgBackingDiskId a)

newtype DiskDelete = DiskDelete {ddelDiskId :: Int64}

instance Action DiskDelete where
  actionSubsystem _ = SubDisk
  actionCommand _ = "delete"
  actionEntityId = Just . fromIntegral . ddelDiskId
  actionExecute ctx a = handleDiskDelete (acState ctx) (ddelDiskId a)

data DiskResize = DiskResize
  { drzDiskId :: Int64
  , drzNewSizeMb :: Int64
  }

instance Action DiskResize where
  actionSubsystem _ = SubDisk
  actionCommand _ = "resize"
  actionEntityId = Just . fromIntegral . drzDiskId
  actionExecute ctx a = handleDiskResize (acState ctx) (drzDiskId a) (drzNewSizeMb a)

data DiskClone = DiskClone
  { dclName :: Text
  , dclBaseDiskId :: Int64
  , dclResizeMb :: Maybe Int
  , dclPath :: Maybe Text
  }

instance Action DiskClone where
  actionSubsystem _ = SubDisk
  actionCommand _ = "clone"
  actionEntityName = Just . dclName
  actionExecute ctx a = handleDiskClone (acState ctx) (dclName a) (dclBaseDiskId a) (dclResizeMb a) (dclPath a)

newtype DiskRefresh = DiskRefresh {drfDiskId :: Int64}

instance Action DiskRefresh where
  actionSubsystem _ = SubDisk
  actionCommand _ = "refresh"
  actionEntityId = Just . fromIntegral . drfDiskId
  actionExecute ctx a = handleDiskRefresh (acState ctx) (drfDiskId a)
