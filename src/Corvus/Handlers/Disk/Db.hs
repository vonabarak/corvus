{-# LANGUAGE OverloadedStrings #-}

-- | Shared database helpers for the disk subsystem.
--
-- Kept separate from 'Corvus.Handlers.Disk' so that the per-concern
-- submodules (Snapshot, Attach, Import, Rebase) can all import these
-- helpers without creating cycles through the umbrella module.
module Corvus.Handlers.Disk.Db
  ( -- * Attachment queries
    getAttachedVms
  , getRunningAttachedVms
  , getReadWriteAttachedVms

    -- * Overlay / backing chain
  , getOverlayIds
  , isCircularBacking
  , getBackingChainIds

    -- * Deletion
  , deleteDiskAndSnapshots

    -- * Listings
  , listDiskImages
  , getDiskImageInfo
  , getSnapshots

    -- * DiskImageNode placement
  , recordDiskImageNode
  , diskImageNodeFilePathFor
  , hasPlacementOnNode
  , listDiskImageNodes
  , deleteDiskImageNodeRow
  )
where

import Control.Monad (forM, forM_)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Protocol
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (SqlPersistT)

-- | Get VMs that have this disk attached, as 'NamedRef' values
-- (id + display name). "(deleted)" name signals the rare race
-- where a Drive row outlives its parent Vm row.
getAttachedVms :: Int64 -> SqlPersistT IO [NamedRef]
getAttachedVms diskId = do
  drives <- selectList [M.DriveDiskImageId ==. toSqlKey diskId] []
  let vmKeys = map (driveVmId . entityVal) drives
  forM vmKeys $ \vmKey -> do
    mVm <- get vmKey
    let name = maybe "(deleted)" vmName mVm
    pure NamedRef {nrId = fromSqlKey vmKey, nrName = name}

-- | Get VMs with active QEMU processes that have this disk attached.
-- Both running and paused VMs have live QEMU processes holding disk files open.
getRunningAttachedVms :: Int64 -> SqlPersistT IO [Int64]
getRunningAttachedVms diskId = do
  drives <- selectList [M.DriveDiskImageId ==. toSqlKey diskId] []
  let vmKeys = map (driveVmId . entityVal) drives
  activeVms <- selectList [M.VmId <-. vmKeys, M.VmStatus <-. [VmRunning, VmPaused]] []
  pure $ map (fromSqlKey . entityKey) activeVms

-- | Get VM (id, name) pairs that have this disk attached
-- read-write. Used by @crv disk copy@/@move@ to refuse moving an
-- in-use writable image without going through @vm migrate@.
getReadWriteAttachedVms :: Int64 -> SqlPersistT IO [(Int64, T.Text)]
getReadWriteAttachedVms diskId = do
  drives <-
    selectList
      [ M.DriveDiskImageId ==. toSqlKey diskId
      , M.DriveReadOnly ==. False
      ]
      []
  let vmKeys = map (driveVmId . entityVal) drives
  forM vmKeys $ \vmKey -> do
    mVm <- get vmKey
    let name = maybe "(deleted)" vmName mVm
    pure (fromSqlKey vmKey, name)

-- | Get overlays that reference this disk as a backing image, as
-- 'NamedRef' values.
getOverlayIds :: Int64 -> SqlPersistT IO [NamedRef]
getOverlayIds diskId = do
  overlays <- selectList [M.DiskImageBackingImageId ==. Just (toSqlKey diskId)] []
  pure $
    map
      (\(Entity key d) -> NamedRef {nrId = fromSqlKey key, nrName = diskImageName d})
      overlays

-- | Check if @newBackingId@ transitively depends on @diskId@ via the backing chain.
-- Returns 'True' if making @diskId@ backed by @newBackingId@ would create a cycle.
isCircularBacking :: Int64 -> Int64 -> SqlPersistT IO Bool
isCircularBacking diskId newBackingId
  | diskId == newBackingId = pure True
  | otherwise = walk newBackingId
  where
    walk currentId = do
      mDisk <- get (toSqlKey currentId :: DiskImageId)
      case mDisk of
        Nothing -> pure False
        Just disk -> case diskImageBackingImageId disk of
          Nothing -> pure False
          Just parentKey ->
            let parentId = fromSqlKey parentKey
             in if parentId == diskId then pure True else walk parentId

-- | Delete disk and its snapshots, including any cache entries the
-- snapshots feed.
deleteDiskAndSnapshots :: Int64 -> SqlPersistT IO ()
deleteDiskAndSnapshots diskId = do
  -- Cache rows reference Snapshot via foreign key. With cascade not
  -- declared in the schema, we explicitly clean up the cache rows
  -- belonging to this disk's snapshots before dropping the snapshots.
  snaps <- selectList [M.SnapshotDiskImageId ==. toSqlKey diskId] []
  forM_ snaps $ \(Entity sk _) ->
    deleteWhere [M.BuildCacheEntrySnapshotId ==. sk]
  deleteWhere [M.SnapshotDiskImageId ==. toSqlKey diskId]
  delete (toSqlKey diskId :: DiskImageId)

-- | Walk a disk image's backing chain and return every ancestor's
-- 'DiskImageId' in nearest-first order. The disk itself is *not*
-- included. Empty for non-overlay images. Stops if the chain
-- contains a missing parent (which would indicate a stale row).
getBackingChainIds :: Int64 -> SqlPersistT IO [DiskImageId]
getBackingChainIds diskId = go (toSqlKey diskId)
  where
    go key = do
      mDisk <- get key
      case mDisk >>= diskImageBackingImageId of
        Nothing -> pure []
        Just parent -> (parent :) <$> go parent

-- | Resolve placements for one disk: every 'DiskImageNode' row
-- joined with the matching 'Node' name so the DTO carries
-- human-friendly node labels instead of raw ids.
placementsFor :: DiskImageId -> SqlPersistT IO [DiskImagePlacement]
placementsFor diskId = do
  rows <- selectList [M.DiskImageNodeDiskImageId ==. diskId] []
  forM rows $ \(Entity _ row) -> do
    let nKey = diskImageNodeNodeId row
    mNode <- get nKey
    let nName = maybe "(deleted)" nodeName mNode
    pure
      DiskImagePlacement
        { dipNode = NamedRef {nrId = fromSqlKey nKey, nrName = nName}
        , dipFilePath = diskImageNodeFilePath row
        }

-- | Resolve the backing image of a disk image as a 'NamedRef'.
-- Returns 'Nothing' when the disk has no backing or when the
-- backing row has been deleted out from under it (rare race).
backingImageRef
  :: Maybe DiskImageId -> SqlPersistT IO (Maybe NamedRef)
backingImageRef Nothing = pure Nothing
backingImageRef (Just backingKey) = do
  mBacking <- get backingKey
  pure $ fmap (\b -> NamedRef {nrId = fromSqlKey backingKey, nrName = diskImageName b}) mBacking

-- | List all disk images with attachment info.
listDiskImages :: SqlPersistT IO [DiskImageInfo]
listDiskImages = do
  disks <- selectList [] [Asc M.DiskImageName]
  forM disks $ \(Entity key disk) -> do
    attachedVms <- getAttachedVms (fromSqlKey key)
    backing <- backingImageRef (diskImageBackingImageId disk)
    placements <- placementsFor key
    pure $
      DiskImageInfo
        { diiId = fromSqlKey key
        , diiName = diskImageName disk
        , diiPlacements = placements
        , diiFormat = diskImageFormat disk
        , diiSizeMb = diskImageSizeMb disk
        , diiCreatedAt = diskImageCreatedAt disk
        , diiAttachedTo = attachedVms
        , diiBackingImage = backing
        , diiEphemeral = diskImageEphemeral disk
        }

-- | Get disk image info for a single disk.
getDiskImageInfo :: Int64 -> SqlPersistT IO (Maybe DiskImageInfo)
getDiskImageInfo diskId = do
  mDisk <- get (toSqlKey diskId :: DiskImageId)
  case mDisk of
    Nothing -> pure Nothing
    Just disk -> do
      let key = toSqlKey diskId :: DiskImageId
      attachedVms <- getAttachedVms diskId
      backing <- backingImageRef (diskImageBackingImageId disk)
      placements <- placementsFor key
      pure $
        Just
          DiskImageInfo
            { diiId = diskId
            , diiName = diskImageName disk
            , diiPlacements = placements
            , diiFormat = diskImageFormat disk
            , diiSizeMb = diskImageSizeMb disk
            , diiCreatedAt = diskImageCreatedAt disk
            , diiAttachedTo = attachedVms
            , diiBackingImage = backing
            , diiEphemeral = diskImageEphemeral disk
            }

-- | Insert (or update) the 'DiskImageNode' row that locates a
-- logical disk image on a specific node. Idempotent — re-running
-- with the same @(diskId, nodeId)@ replaces the stored file path
-- (mirrors the @--force@-style behaviour @crv disk register@
-- already implies for re-registration).
recordDiskImageNode :: DiskImageId -> NodeId -> Text -> SqlPersistT IO ()
recordDiskImageNode diskId nodeId path = do
  existing <- getBy (M.UniqueDiskImageOnNode diskId nodeId)
  case existing of
    Just (Entity key _) ->
      update key [M.DiskImageNodeFilePath =. path]
    Nothing ->
      insert_
        M.DiskImageNode
          { diskImageNodeDiskImageId = diskId
          , diskImageNodeNodeId = nodeId
          , diskImageNodeFilePath = path
          }

-- | Look up the 'DiskImageNode' row for a (disk, node) pair, if
-- any. Returns 'Nothing' when the image isn't yet stored on the
-- requested node — call sites use this for the same-node attach
-- check.
diskImageNodeFor
  :: DiskImageId
  -> NodeId
  -> SqlPersistT IO (Maybe (Entity DiskImageNode))
diskImageNodeFor diskId nodeId =
  getBy (M.UniqueDiskImageOnNode diskId nodeId)

-- | Convenience: return just the file path for a (disk, node)
-- pair, or 'Nothing' when no row exists.
diskImageNodeFilePathFor :: DiskImageId -> NodeId -> SqlPersistT IO (Maybe Text)
diskImageNodeFilePathFor diskId nodeId = do
  r <- diskImageNodeFor diskId nodeId
  pure $ fmap (diskImageNodeFilePath . entityVal) r

-- | Convenience: 'True' if the (disk, node) pair already has a
-- 'DiskImageNode' placement row.
hasPlacementOnNode :: DiskImageId -> NodeId -> SqlPersistT IO Bool
hasPlacementOnNode diskId nodeId = do
  r <- diskImageNodeFor diskId nodeId
  pure $ case r of
    Just _ -> True
    Nothing -> False

-- | List every 'DiskImageNode' row for a logical image.
listDiskImageNodes :: DiskImageId -> SqlPersistT IO [Entity DiskImageNode]
listDiskImageNodes diskId =
  selectList [M.DiskImageNodeDiskImageId ==. diskId] []

-- | Remove a single (disk, node) placement row. Used by
-- 'handleDiskDelete' after the agent on that node confirms the
-- file is gone.
deleteDiskImageNodeRow :: DiskImageId -> NodeId -> SqlPersistT IO ()
deleteDiskImageNodeRow diskId nodeId =
  deleteWhere
    [ M.DiskImageNodeDiskImageId ==. diskId
    , M.DiskImageNodeNodeId ==. nodeId
    ]

-- | Get snapshots for a disk.
getSnapshots :: Int64 -> SqlPersistT IO [SnapshotInfo]
getSnapshots diskId = do
  snapshots <- selectList [M.SnapshotDiskImageId ==. toSqlKey diskId] [Asc M.SnapshotCreatedAt]
  pure $
    map
      ( \(Entity key snap) ->
          SnapshotInfo
            { sniId = fromSqlKey key
            , sniName = snapshotName snap
            , sniCreatedAt = snapshotCreatedAt snap
            , sniSizeMb = snapshotSizeMb snap
            , sniLive = snapshotLive snap
            , sniQuiesced = snapshotQuiesced snap
            , sniHasVmstate = snapshotHasVmstate snap
            }
      )
      snapshots
