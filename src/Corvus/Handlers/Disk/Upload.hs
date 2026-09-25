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

-- | Resolved destination for a client-upload stream. Planning only reads
-- state; the Action below publishes the completed node-side file.
data DiskUploadPlan = DiskUploadPlan
  { dupName :: !Text
  , dupFormat :: !DriveFormat
  , dupEphemeral :: !Bool
  , dupNodeId :: !M.NodeId
  , dupFilePath :: !FilePath
  , dupStoredPath :: !Text
  , dupOverwrite :: !Bool
  }

prepareDiskUpload
  :: ServerState -> Text -> DriveFormat -> Maybe Text -> Bool -> Text -> Bool -> IO (Either Text DiskUploadPlan)
prepareDiskUpload state name format mPath ephemeral nodeRefText overwrite =
  case sanitizeDiskName name of
    Left err -> pure (Left err)
    Right safeName -> do
      let placeOn nid = do
            basePath <- nodeBasePathFor state nid
            let defaultPath = resolveDiskFilePathPure basePath mPath (T.unpack safeName <> "." <> T.unpack (enumToText format))
                mkPlan = DiskUploadPlan safeName format ephemeral nid
            existing <- runSqlPool (getBy (UniqueDiskImageName safeName)) (ssDbPool state)
            case existing of
              Nothing -> pure (Right (mkPlan defaultPath (makeRelativeToBase basePath defaultPath) False))
              Just (Entity diskKey _)
                | not overwrite -> pure (Left $ "disk image '" <> safeName <> "' already exists (set ifExists: overwrite to replace it)")
                | otherwise -> do
                    attached <- runSqlPool (getAttachedVms (fromSqlKey diskKey)) (ssDbPool state)
                    placements <- runSqlPool (listDiskImageNodes diskKey) (ssDbPool state)
                    case placements of
                      [Entity _ placement]
                        | null attached && diskImageNodeNodeId placement == nid -> do
                            let stored = diskImageNodeFilePath placement
                                raw = T.unpack stored
                                path = if "/" `isPrefixOf` raw then raw else basePath </> raw
                            case mPath of
                              Just p
                                | resolveDiskFilePathPure basePath (Just p) (takeFileName path) /= path ->
                                    pure (Left "overwrite uses the existing disk path; omit path or supply the same path")
                              _ -> pure (Right (mkPlan path stored True))
                      _ -> pure (Left "overwrite requires an unattached disk with exactly one placement on the target node")
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

newtype DiskUploadFinalize = DiskUploadFinalize {dufPlan :: DiskUploadPlan}

instance Action DiskUploadFinalize where
  actionSubsystem _ = SubDisk
  actionCommand _ = "upload"
  actionEntityName = Just . dupName . dufPlan
  actionExecute ctx a = handleDiskUploadFinalize (acState ctx) (dufPlan a)

handleDiskUploadFinalize :: ServerState -> DiskUploadPlan -> IO Response
handleDiskUploadFinalize state plan = runServerLogging state $ do
  sizeMb <- liftIO $ getImageSizeMbViaAgent state (dupNodeId plan) (dupFilePath plan)
  now <- liftIO getCurrentTime
  liftIO $
    runSqlPool
      ( do
          existing <- getBy (UniqueDiskImageName (dupName plan))
          key <- case existing of
            Nothing ->
              insert
                DiskImage
                  { diskImageName = dupName plan
                  , diskImageFormat = dupFormat plan
                  , diskImageSizeMb = sizeMb
                  , diskImageCreatedAt = now
                  , diskImageBackingImageId = Nothing
                  , diskImageEphemeral = dupEphemeral plan
                  }
            Just (Entity key _)
              | dupOverwrite plan -> do
                  update
                    key
                    [ DiskImageFormat =. dupFormat plan
                    , DiskImageSizeMb =. sizeMb
                    , DiskImageEphemeral =. dupEphemeral plan
                    ]
                  pure key
              | otherwise -> fail "disk image appeared while upload was in progress"
          recordDiskImageNode key (dupNodeId plan) (dupStoredPath plan)
          pure (RespDiskCreated (fromSqlKey key))
      )
      (ssDbPool state)
