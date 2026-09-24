{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Query disk-image handlers.
module Corvus.Handlers.Disk.Query
  ( handleDiskList
  , handleDiskShow
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

-- | List all disk images. The 'diiFilePath' field is rewritten to an
-- absolute path so clients (Makefile, integration scripts) can use it
-- directly without knowing the daemon's disk base.
handleDiskList :: ServerState -> IO Response
handleDiskList state = do
  disks <- runSqlPool listDiskImages (ssDbPool state)
  RespDiskList <$> mapM (absolutizeDiskFilePath state) disks

-- | Show disk image details. As with 'handleDiskList', 'diiFilePath'
-- in the response is absolute.
handleDiskShow :: ServerState -> Int64 -> IO Response
handleDiskShow state diskId = do
  mInfo <- runSqlPool (getDiskImageInfo diskId) (ssDbPool state)
  case mInfo of
    Nothing -> pure RespDiskNotFound
    Just info -> RespDiskInfo <$> absolutizeDiskFilePath state info

-- | Promote each placement's 'dipFilePath' to an absolute path.
-- The DB stores paths relative to the daemon's base; clients
-- need the absolute form.
absolutizeDiskFilePath :: ServerState -> DiskImageInfo -> IO DiskImageInfo
absolutizeDiskFilePath state info = do
  placements <- mapM absolutizePlacement (diiPlacements info)
  pure info {diiPlacements = placements}
  where
    absolutizePlacement p = do
      basePath <- nodeBasePathFor state (toSqlKey (nrId (dipNode p)) :: M.NodeId)
      let raw = T.unpack (dipFilePath p)
          absPath = if "/" `isPrefixOf` raw then raw else basePath </> raw
      pure p {dipFilePath = T.pack absPath}
