{-# LANGUAGE OverloadedStrings #-}

-- | Orchestration helpers for the build-step cache.
--
-- This module bridges the pure layer in "Corvus.Build.Cache.Hash" /
-- "Corvus.Build.Cache.Store" with the I/O the build runner needs to
-- perform: enumerating the bake VM's writable disks, calling the
-- multi-disk live-snapshot agent helper, recording 'Snapshot' rows,
-- and rolling back to a cached step.
--
-- The actual policy decisions ("should we take a snapshot now?",
-- "did we find a usable cache prefix?") live in
-- "Corvus.Handlers.Build"; this module only carries them out.
module Corvus.Handlers.Build.Cache
  ( CacheDisk (..)
  , writableCacheDisks
  , snapshotCachedStep
  , rollbackToCachedStep
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import qualified Corvus.Build.Cache.Hash as H
import qualified Corvus.Build.Cache.Store as Store
import Corvus.Handlers.Disk.Agent
  ( createSnapshotViaAgentLiveMany
  , createSnapshotViaAgentWithVmstate
  , rollbackSnapshotViaAgent
  )
import Corvus.Handlers.Disk.Db (listDiskImageNodes)
import Corvus.Handlers.Disk.Path (resolveDiskPath)
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Node.Image (ImageResult (..))
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Schema.Build (BuildCacheMode (..))
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (fromSqlKey, runSqlPool, toSqlKey)

-- | One writable disk in the bake VM, ready to be cached. The role
-- discriminates the per-step cache rows: \"artifact\" is the disk
-- the published artifact comes from; \"system\" is the bake VM's
-- own boot disk (only present for the from-scratch strategy, where
-- provisioner steps modify the bake VM itself and resume must
-- restore both).
data CacheDisk = CacheDisk
  { cdRole :: !Text
  , cdDiskImageId :: !DiskImageId
  , cdNodeId :: !NodeId
  , cdFilePath :: !FilePath
  }
  deriving (Show)

-- | Enumerate the bake VM's writable cache-worthy disks. Walks the
-- 'Drive' rows for the VM, joins each to its 'DiskImage', filters
-- to @format=qcow2@ + @readOnly=False@ + non-cdrom\/floppy interface,
-- and tags the disk whose id matches @artifactDiskId@ with role
-- \"artifact\" (everything else gets \"system\").
--
-- Returns @Left@ if any disk can't be resolved on a node (no
-- placement, missing path). The caller treats that as cache-fatal:
-- we'd rather skip caching this step than write a partial row.
writableCacheDisks
  :: ServerState
  -> Int64
  -- ^ bake VM id
  -> Int64
  -- ^ artifact disk id (its drive gets role \"artifact\")
  -> LoggingT IO (Either Text [CacheDisk])
writableCacheDisks state vmId artifactDiskId = do
  let pool = ssDbPool state
      vmKey = toSqlKey vmId :: VmId
  drives <- liftIO $ runSqlPool (selectList [M.DriveVmId ==. vmKey] []) pool
  results <- mapM resolveOne drives
  pure $ sequence (filter keep results)
  where
    keep (Right Nothing) = False
    keep _ = True
    resolveOne (Entity _ drive)
      | M.driveReadOnly drive = pure (Right Nothing)
      | not (interfaceUsable (M.driveInterface drive)) = pure (Right Nothing)
      | otherwise = do
          let diskKey = M.driveDiskImageId drive
          mDisk <- liftIO $ runSqlPool (get diskKey) (ssDbPool state)
          case mDisk of
            Nothing -> pure (Right Nothing)
            Just disk
              | M.diskImageFormat disk /= FormatQcow2 -> pure (Right Nothing)
              | otherwise -> do
                  placements <- liftIO $ runSqlPool (listDiskImageNodes diskKey) (ssDbPool state)
                  case placements of
                    [] ->
                      pure (Left ("disk " <> T.pack (show (fromSqlKey diskKey)) <> " has no recorded placement"))
                    (Entity _ row : _) -> do
                      let nid = M.diskImageNodeNodeId row
                      path <-
                        liftIO $
                          resolveDiskPath
                            (ssDbPool state)
                            (ssQemuConfig state)
                            diskKey
                            nid
                      let role =
                            if fromSqlKey diskKey == artifactDiskId
                              then "artifact"
                              else "system"
                      pure
                        ( Right
                            ( Just
                                CacheDisk
                                  { cdRole = role
                                  , cdDiskImageId = diskKey
                                  , cdNodeId = nid
                                  , cdFilePath = path
                                  }
                            )
                        )
    -- The list flattens Right Nothing entries out; this is the projection.
    sequence xs = concatMap collapse <$> traverse pickRight xs
      where
        pickRight (Left e) = Left e
        pickRight (Right v) = Right v
        collapse Nothing = []
        collapse (Just c) = [c]

-- | Interfaces eligible for caching. Anything that's a real
-- writable block device qualifies; the floppy / cdrom / pflash
-- interfaces are filtered out because the build doesn't write to
-- them and their qcow2 surface (when present) is not interesting
-- to the cache.
interfaceUsable :: DriveInterface -> Bool
interfaceUsable InterfaceVirtio = True
interfaceUsable InterfaceScsi = True
interfaceUsable InterfaceIde = True
interfaceUsable InterfaceSata = True
interfaceUsable InterfaceNvme = True
interfaceUsable _ = False

-- | Take one atomic multi-disk live snapshot covering every writable
-- disk for the bake VM, then write the matching 'Snapshot' +
-- 'BuildCacheEntry' rows. The snapshot name is derived from the chain
-- hash; per-disk uniqueness already covers collisions.
--
-- All disks must live on the same node (the bake VM's node). The
-- function asserts this; a cross-node spread is a daemon bug.
snapshotCachedStep
  :: ServerState
  -> BuildCacheMode
  -- ^ disk vs memory storage model
  -> Int64
  -- ^ bake VM id
  -> Text
  -- ^ pipelineKey
  -> Int
  -- ^ 1-based step index
  -> Text
  -- ^ chainHash through this step inclusive
  -> [CacheDisk]
  -- ^ every writable disk on the bake VM
  -> LoggingT IO (Either Text ())
snapshotCachedStep _ _ _ _ _ _ [] = pure (Right ())
snapshotCachedStep state mode vmId pipelineKey stepIdx chain disks = do
  -- Idempotency check: if a 'BuildCacheEntry' already exists for
  -- every required role on this (pipelineKey, chainHash), another
  -- concurrent runner already cached this step. Treat as success
  -- without writing anything new — the qcow2 snapshot is already
  -- on disk and the cache rows already point at it.
  existing <- liftIO $ Store.cacheStepRoles state pipelineKey chain
  let needRoles = map cdRole disks
      alreadyHave = all (`elem` existing) needRoles
  if alreadyHave
    then do
      logInfoN $
        "cache: step "
          <> T.pack (show stepIdx)
          <> " already cached by a concurrent run; skipping snapshot"
      pure (Right ())
    else takeSnapshotAndRecord
  where
    takeSnapshotAndRecord = case L.nub (map cdNodeId disks) of
      [nid] -> do
        let snapName = H.cacheSnapshotName chain
            paths = map cdFilePath disks
        logInfoN $
          "cache: step "
            <> T.pack (show stepIdx)
            <> " ("
            <> modeLabel mode
            <> ") snapshotting "
            <> T.pack (show (length disks))
            <> " disk(s) as "
            <> snapName
        (result, quiesced, hasVmstateFor) <- case mode of
          CacheModeDisk -> do
            (r, q) <-
              liftIO $
                createSnapshotViaAgentLiveMany
                  state
                  nid
                  paths
                  snapName
                  vmId
                  NOA.QuiesceRequire
            pure (r, q, const False)
          CacheModeMemory -> do
            -- Vmstate-aware path. The artifact disk carries the
            -- RAM dump; every disk (including the artifact) also
            -- gets a block snapshot under the same tag, atomically
            -- on QEMU's side via one `snapshot-save` job. No QGA
            -- freeze — vmstate captures the in-flight page cache
            -- and writeback queue, so freezing under a multi-
            -- second save would be harmful.
            case L.find (\d -> cdRole d == "artifact") disks of
              Nothing ->
                pure
                  ( ImageError
                      "cacheMode=memory needs an 'artifact'-role disk to carry vmstate"
                  , False
                  , const False
                  )
              Just carrier -> do
                r <-
                  liftIO $
                    createSnapshotViaAgentWithVmstate
                      state
                      nid
                      (cdFilePath carrier)
                      paths
                      snapName
                      vmId
                pure
                  ( r
                  , False -- not quiesced; vmstate doesn't fsfreeze
                  , \d -> cdDiskImageId d == cdDiskImageId carrier
                  )
        case result of
          ImageSuccess -> do
            rowResults <- liftIO $ insertRows quiesced snapName hasVmstateFor
            let snapPairs = [(cdRole d, snapId) | (d, snapId) <- zip disks rowResults]
            liftIO $
              Store.recordCacheStep state pipelineKey stepIdx chain vmId snapPairs
            pure (Right ())
          ImageError err -> do
            logWarnN $ "cache: snapshot failed: " <> err
            pure (Left ("cache snapshot: " <> err))
          ImageNotFound ->
            pure (Left "cache snapshot: disk not found")
          ImageFormatNotSupported msg ->
            pure (Left ("cache snapshot: " <> msg))
      nodeIds ->
        pure
          ( Left
              ( "cache: bake VM disks span "
                  <> T.pack (show (length nodeIds))
                  <> " nodes; cannot snapshot atomically"
              )
          )

    insertRows quiesced snapName hasVmstateFor = do
      now <- getCurrentTime
      mapM (insertOne now quiesced snapName hasVmstateFor) disks
    insertOne now quiesced snapName hasVmstateFor d =
      runSqlPool
        ( insert
            Snapshot
              { snapshotDiskImageId = cdDiskImageId d
              , snapshotName = snapName
              , snapshotCreatedAt = now
              , snapshotSizeMb = Nothing
              , snapshotLive = True
              , snapshotQuiesced = quiesced
              , snapshotHasVmstate = hasVmstateFor d
              }
            >>= \k -> pure (fromSqlKey k)
        )
        (ssDbPool state)

    modeLabel :: BuildCacheMode -> Text
    modeLabel CacheModeDisk = "disk-only"
    modeLabel CacheModeMemory = "memory+disk"

-- | Disk-mode cache rollback: roll every cached disk back to its
-- named snapshot via offline @qemu-img snapshot -a@. The bake VM
-- MUST be stopped — 'rollbackSnapshotViaAgent' takes the qcow2's
-- exclusive lock, which collides with a running QEMU.
--
-- Memory-mode resume does NOT go through this function. The
-- caller in @Corvus.Handlers.Build@ dispatches on
-- 'BuildCacheMode' BEFORE picking a rollback path and routes
-- memory-mode through @resumeMemoryCacheBakeVm@, which launches
-- QEMU paused and drives @snapshot-load@ via QMP.
rollbackToCachedStep
  :: ServerState
  -> [CacheDisk]
  -> Text
  -- ^ chainHash to roll back to
  -> LoggingT IO (Either Text ())
rollbackToCachedStep _ [] _ = pure (Right ())
rollbackToCachedStep state disks chain = do
  let snapName = H.cacheSnapshotName chain
  logInfoN $
    "cache: rolling back "
      <> T.pack (show (length disks))
      <> " disk(s) to "
      <> snapName
  go disks snapName
  where
    go [] _ = pure (Right ())
    go (d : rest) snapName = do
      r <-
        liftIO $
          rollbackSnapshotViaAgent state (cdNodeId d) (cdFilePath d) snapName
      case r of
        ImageSuccess -> go rest snapName
        ImageError err ->
          pure (Left ("cache rollback (disk " <> cdRole d <> "): " <> err))
        ImageNotFound ->
          pure (Left ("cache rollback (disk " <> cdRole d <> "): snapshot not found"))
        ImageFormatNotSupported msg ->
          pure (Left ("cache rollback: " <> msg))
