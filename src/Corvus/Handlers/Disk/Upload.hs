{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Upload disk-image handlers.
module Corvus.Handlers.Disk.Upload
  ( DiskUploadPlan (..)
  , DiskUploadFinalize (..)
  , prepareDiskUpload
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

-- | Resolved destination and reserved ID for a client-upload stream.
-- The completed image and its tags are published only by finalization.
data DiskUploadPlan = DiskUploadPlan
  { dupImageId :: !M.DiskImageId
  , dupName :: !Text
  , dupFormat :: !DriveFormat
  , dupEphemeral :: !Bool
  , dupNodeId :: !M.NodeId
  , dupFilePath :: !FilePath
  , dupStoredPath :: !Text
  }

prepareDiskUpload
  :: ServerState -> Text -> Text -> DriveFormat -> Maybe Text -> Bool -> Text -> IO (Either Text DiskUploadPlan)
prepareDiskUpload state clientName name format mPath ephemeral nodeRefText =
  case sanitizeDiskName name of
    Left err -> pure (Left err)
    Right safeName -> do
      let placeOn nid = do
            basePath <- nodeBasePathFor state nid
            reserved <- runAction state clientName (DiskUploadReserveId safeName)
            case reserved of
              RespDiskCreated rawId -> do
                let imageId = M.toSqlKey rawId
                defaultPath <- resolveDiskFilePath imageId basePath mPath (T.unpack safeName <> "." <> T.unpack (enumToText format))
                pure (Right (DiskUploadPlan imageId safeName format ephemeral nid defaultPath (makeRelativeToBase basePath defaultPath)))
              _ -> pure (Left "Failed to reserve an image ID for upload")
      -- Empty text or capnp's unset-EntityRef default ('byId 0')
      -- both mean "no explicit placement" — defer to the scheduler.
      if T.null nodeRefText || nodeRefText == "0"
        then do
          eNid <- pickNodeForDisk state
          case eNid of
            Left err -> pure (Left err)
            Right nid -> placeOn nid
        else do
          r <- resolveNode (Ref nodeRefText) (ssDbPool state)
          case r of
            Left re -> pure (Left (resolveErrorMessage re))
            Right nidRaw -> placeOn (M.toSqlKey nidRaw)

-- Reserving an upload ID is a mutation even though no image is exposed yet.
newtype DiskUploadReserveId = DiskUploadReserveId Text

instance Action DiskUploadReserveId where
  actionSubsystem _ = SubDisk
  actionCommand _ = "upload-reserve"
  actionEntityName (DiskUploadReserveId name) = Just name
  actionExecute ctx _ = do
    key <- runSqlPool reserveImageId (ssDbPool (acState ctx))
    pure (RespDiskCreated (fromSqlKey key))

newtype DiskUploadFinalize = DiskUploadFinalize {dufPlan :: DiskUploadPlan}

instance Action DiskUploadFinalize where
  actionSubsystem _ = SubDisk
  actionCommand _ = "upload"
  actionEntityName = Just . dupName . dufPlan
  actionExecute ctx a = handleDiskUploadFinalize (acState ctx) (dufPlan a)

handleDiskUploadFinalize :: ServerState -> DiskUploadPlan -> IO Response
handleDiskUploadFinalize state plan = runServerLogging state $ do
  inspected <- liftIO $ getImageInfoViaAgent state (dupNodeId plan) (dupFilePath plan)
  case inspected of
    Left err -> discard err
    Right info
      | iiFormat info /= dupFormat plan -> discard "Uploaded image format differs from requested format"
      | otherwise -> do
          now <- liftIO getCurrentTime
          liftIO $
            runSqlPool
              ( do
                  key <-
                    publishImageWithId
                      (dupImageId plan)
                      DiskImage
                        { diskImageName = dupName plan
                        , diskImageFormat = dupFormat plan
                        , diskImageSize = Just (iiVirtualSize info)
                        , diskImageCreatedAt = now
                        , diskImageBackingImageId = Nothing
                        , diskImageEphemeral = dupEphemeral plan
                        }
                  recordDiskImageNode key (dupNodeId plan) (dupStoredPath plan)
                  pure (RespDiskCreated (fromSqlKey key))
              )
              (ssDbPool state)
  where
    discard err = do
      _ <- liftIO $ deleteImageViaAgent state (dupNodeId plan) (dupFilePath plan)
      pure (RespError err)
