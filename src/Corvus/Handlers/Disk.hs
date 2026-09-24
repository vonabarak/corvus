-- | Public disk-image handler surface.
--
-- Mutating operations are represented by Action types; their implementation
-- helpers remain private to their focused owner modules.
module Corvus.Handlers.Disk
  ( DiskCreate (..)
  , DiskCreateOverlay (..)
  , DiskRegister (..)
  , DiskDelete (..)
  , DiskResize (..)
  , DiskClone (..)
  , DiskRefresh (..)
  , DiskCopy (..)
  , DiskMove (..)
  , SnapshotCreate (..)
  , SnapshotDelete (..)
  , SnapshotRollback (..)
  , SnapshotMerge (..)
  , DiskAttach (..)
  , DiskDetachByDisk (..)
  , DiskRebase (..)
  , DiskImportAction (..)
  , DiskUploadFinalize (..)
  , DiskUploadPlan (..)
  , handleDiskList
  , handleDiskShow
  , prepareDiskUpload
  , handleSnapshotList
  , computeDestPaths
  )
where

import Corvus.Handlers.Disk.Attach (DiskAttach (..), DiskDetachByDisk (..))
import Corvus.Handlers.Disk.Create (DiskCreate (..), DiskRegister (..))
import Corvus.Handlers.Disk.Derive (DiskClone (..), DiskCreateOverlay (..))
import Corvus.Handlers.Disk.Import (DiskImportAction (..))
import Corvus.Handlers.Disk.Maintenance (DiskDelete (..), DiskRefresh (..), DiskResize (..))
import Corvus.Handlers.Disk.Placement (DiskCopy (..), DiskMove (..), computeDestPaths)
import Corvus.Handlers.Disk.Query (handleDiskList, handleDiskShow)
import Corvus.Handlers.Disk.Rebase (DiskRebase (..))
import Corvus.Handlers.Disk.Snapshot (SnapshotCreate (..), SnapshotDelete (..), SnapshotMerge (..), SnapshotRollback (..), handleSnapshotList)
import Corvus.Handlers.Disk.Upload (DiskUploadFinalize (..), DiskUploadPlan (..), prepareDiskUpload)
