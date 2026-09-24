{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Create disk-image handlers.
module Corvus.Handlers.Disk.Create
  ( DiskCreate (..)
  , DiskRegister (..)
  )
where

import Corvus.Action

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
  , getImageSizeMbViaAgent
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

-- | Create a new disk image. An empty/zero @nodeRefText@ means
-- "no explicit placement" — defer to 'pickNodeForDisk'.
handleDiskCreate :: ServerState -> Text -> DriveFormat -> Int64 -> Maybe Text -> Bool -> Text -> IO Response
handleDiskCreate state name format sizeMb mPath ephemeral nodeRefText = runServerLogging state $ do
  logInfoN $ "Creating disk image: " <> name <> " (" <> T.pack (show sizeMb) <> " MB)"
  case sanitizeDiskName name of
    Left err -> do
      logWarnN $ "Invalid disk name: " <> err
      pure $ RespError err
    Right safeName ->
      withSelectedDiskNode state nodeRefText $
        createDiskOnNode state safeName format sizeMb mPath ephemeral

createDiskOnNode state safeName format sizeMb mPath ephemeral nid = do
  basePath <- liftIO $ nodeBasePathFor state nid
  let fileName = T.unpack safeName <> "." <> T.unpack (enumToText format)
  filePath <- liftIO $ resolveDiskFilePath basePath mPath fileName
  result <- liftIO $ createImageViaAgent state nid filePath format sizeMb
  case result of
    ImageError err -> do
      logWarnN $ "Failed to create image: " <> err
      pure $ RespError err
    ImageFormatNotSupported msg -> pure $ RespFormatNotSupported msg
    ImageNotFound -> pure $ RespError "Unexpected error during creation"
    ImageSuccess -> do
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
                      , diskImageEphemeral = ephemeral
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
  -> Bool
  -> Text
  -- ^ node ref (name or id); empty / @"0"@ defers to the scheduler
  -> IO Response
handleDiskRegister state name filePath mFormat mBackingDiskId ephemeral nodeRefText =
  case validateName "Disk image" name of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Registering disk image: " <> name <> " at " <> filePath
      withSelectedDiskNode state nodeRefText $
        registerDiskOnNode state name filePath mFormat mBackingDiskId ephemeral

-- | Register a file on one selected node. The caller has already validated
-- the public name and resolved placement; this routine owns only image
-- inspection and persistence, including the concurrent-register recovery.
registerDiskOnNode state name filePath mFormat mBackingDiskId ephemeral nid = do
  basePath <- liftIO $ nodeBasePathFor state nid
  let storedPath = makeRelativeToBase basePath (T.unpack filePath)
      resolvedPath =
        if "/" `isPrefixOf` T.unpack storedPath
          then T.unpack storedPath
          else basePath </> T.unpack storedPath
  format <- resolveRegisteredFormat state nid resolvedPath mFormat
  sizeMb <- liftIO $ getImageSizeMbViaAgent state nid resolvedPath
  now <- liftIO getCurrentTime
  mExisting <-
    liftIO $
      runSqlPool
        (getBy (UniqueDiskImageName name))
        (ssDbPool state)
  case mExisting of
    Just (Entity diskKey _) -> do
      -- A logical image can have placements on several nodes. Record the new
      -- placement without attempting a duplicate image insert.
      recordRegisteredPlacement state diskKey nid storedPath "Disk image already registered with ID: "
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
                        , diskImageEphemeral = ephemeral
                        }
                  recordDiskImageNode dkey nid storedPath
                  pure dkey
              )
              (ssDbPool state)
      case result of
        Right diskId -> do
          logInfoN $ "Registered disk image with ID: " <> T.pack (show $ fromSqlKey diskId)
          pure $ RespDiskCreated $ fromSqlKey diskId
        Left (_err :: SomeException) -> recoverConcurrentRegistration storedPath
  where
    recoverConcurrentRegistration storedPath = do
      -- Race: another thread inserted first. Re-read its key and record this
      -- node's placement against that logical image.
      mRetry <-
        liftIO $
          runSqlPool (getBy (UniqueDiskImageName name)) (ssDbPool state)
      case mRetry of
        Just (Entity diskKey _) ->
          recordRegisteredPlacement state diskKey nid storedPath "Disk image registered concurrently with ID: "
        Nothing -> pure $ RespError $ "Failed to register disk image: " <> name

resolveRegisteredFormat state nid resolvedPath = \case
  Just format -> pure format
  Nothing -> do
    mInfo <- liftIO $ getImageInfoViaAgent state nid resolvedPath
    case mInfo of
      Right info -> pure $ iiFormat info
      Left err -> case detectFormatFromPath (T.pack resolvedPath) of
        Just format -> pure format
        Nothing -> do
          logWarnN $ "Could not detect format for " <> T.pack resolvedPath <> ": " <> err
          pure FormatRaw

recordRegisteredPlacement state diskKey nid storedPath message = do
  liftIO $
    runSqlPool
      (recordDiskImageNode diskKey nid storedPath)
      (ssDbPool state)
  logInfoN $ message <> T.pack (show $ fromSqlKey diskKey)
  pure $ RespDiskCreated $ fromSqlKey diskKey

data DiskCreate = DiskCreate
  { dcrName :: Text
  , dcrFormat :: DriveFormat
  , dcrSizeMb :: Int64
  , dcrPath :: Maybe Text
  , dcrEphemeral :: Bool
  , dcrNodeRef :: Text
  -- ^ Target node reference (name or numeric id). Empty / @"0"@
  -- defers to 'Corvus.Handlers.Scheduler.pickNodeForDisk'.
  }

instance Action DiskCreate where
  actionSubsystem _ = SubDisk
  actionCommand _ = "create"
  actionEntityName = Just . dcrName
  actionExecute ctx a = handleDiskCreate (acState ctx) (dcrName a) (dcrFormat a) (dcrSizeMb a) (dcrPath a) (dcrEphemeral a) (dcrNodeRef a)

data DiskRegister = DiskRegister
  { drgName :: Text
  , drgPath :: Text
  , drgFormat :: Maybe DriveFormat
  , drgBackingDiskId :: Maybe Int64
  , drgEphemeral :: Bool
  , drgNodeRef :: Text
  -- ^ Node that hosts the file. Empty / @"0"@ defers to
  -- 'Corvus.Handlers.Scheduler.pickNodeForDisk'.
  }

instance Action DiskRegister where
  actionSubsystem _ = SubDisk
  actionCommand _ = "register"
  actionEntityName = Just . drgName
  actionExecute ctx a = handleDiskRegister (acState ctx) (drgName a) (drgPath a) (drgFormat a) (drgBackingDiskId a) (drgEphemeral a) (drgNodeRef a)
