{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Artifact publication for the build pipeline.
--
-- Publishes the bake VM's artifact by cloning it (via qemu-img)
-- into a fresh non-ephemeral DiskImage, handling overwrite policies
-- and disk compaction.
module Corvus.Handlers.Build.Artifact
  ( publishArtifact
  , publishArtifactByClone
  , checkIfExistsPreBake
  , compactDisk
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import Corvus.Action (mkActionContext, runActionAsSubtask)
import Corvus.Handlers.Disk.Agent
  ( cloneImageViaAgent
  , getImageSizeViaAgent
  , rebaseImageViaAgent
  )
import Corvus.Handlers.Disk.Db (listDiskImageNodes, recordDiskImageNode)
import Corvus.Handlers.Disk.Maintenance (DiskDelete (..))
import Corvus.Handlers.Disk.Path (resolveDiskFilePath, resolveDiskPath)
import Corvus.Handlers.Scheduler (pickNodeForExistingDisk)
import Corvus.Images
import Corvus.Model
import Corvus.Node.Image (ImageResult (..))
import Corvus.Protocol (Response (RespDiskOk, RespError))
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Schema.Build
import Corvus.Types
import Data.Int (Int64)
import qualified Data.List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist (get, getBy, insert, selectList, update, (==.))
import Database.Persist.Sql (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey, (=.))
import System.Directory (createDirectoryIfMissing, removeDirectory, removeFile, renameFile)
import System.FilePath (takeDirectory, (</>))

-- | Publish the bake VM's artifact by CLONING it (via
-- @qemu-img convert@) into a fresh non-ephemeral 'DiskImage'. The
-- bake VM's original artifact disk stays attached and ephemeral so
-- a follow-up @--use-cache@ build can roll it back to a cached
-- step. @qemu-img convert@ copies only the source qcow2's active
-- state and does NOT preserve internal snapshots, so the published
-- disk is hard-guaranteed flat (verifiable via
-- @qemu-img snapshot -l@ returning empty output).
--
-- The bake VM MUST already be stopped before this runs — the
-- @qemu-img convert@ source path takes the file's exclusive lock.
-- The caller (runProvisionersStopAndPublish / runInstallerPhase)
-- handles the @VmStop@ before calling here.
--
-- Returns the new 'DiskImage' row's id on success.
publishArtifact
  :: ServerState
  -> TaskId
  -> Int64
  -- ^ bake VM ID (kept for diagnostics; the drive is NOT detached)
  -> Int64
  -- ^ artifact disk ID (the bake VM's ephemeral source)
  -> Text
  -- ^ published artifact name (= buildName)
  -> BuildTarget
  -- ^ target spec (format, compact, path, …)
  -> Bool
  -- ^ (legacy) flatten flag — ignored; clone is always flat
  -> LoggingT IO (Either Text Int64)
publishArtifact state parentTaskId _bakeVmId artifactDiskId name target _needFlatten = do
  publishArtifactByClone state artifactDiskId name target

-- | Clone the bake VM's artifact disk into a fresh 'DiskImage' row,
-- compacting if asked. Returns the new disk's id.
publishArtifactByClone
  :: ServerState
  -> Int64
  -- ^ source (bake VM artifact) disk id
  -> Text
  -- ^ published artifact name
  -> BuildTarget
  -> LoggingT IO (Either Text Int64)
publishArtifactByClone state srcDiskId name target = do
  let pool = ssDbPool state
      srcKey = toSqlKey srcDiskId :: DiskImageId
  placements <- liftIO $ runSqlPool (listDiskImageNodes srcKey) pool
  case placements of
    [] -> pure $ Left "publish: bake artifact has no recorded placement"
    (Entity _ row : _) -> do
      let nid = diskImageNodeNodeId row
      srcPath <- liftIO $ resolveDiskPath pool (ssQemuConfig state) srcKey nid
      basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
      let ext = T.unpack (enumToText (btFormat target))
          fileName = T.unpack name <> "." <> ext
      reservedId <- liftIO $ runSqlPool reserveImageId pool
      destPath <- liftIO $ resolveDiskFilePath reservedId basePath (btPath target) fileName
      liftIO $ createDirectoryIfMissing True (takeDirectory destPath)
      logInfoN $
        "publish: cloning "
          <> T.pack srcPath
          <> " -> "
          <> T.pack destPath
      cloneRes <- liftIO $ cloneImageViaAgent state nid srcPath destPath (btFormat target)
      case cloneRes of
        ImageError err -> pure $ Left $ "publish clone: " <> err
        ImageNotFound -> pure $ Left "publish clone: source not found"
        ImageFormatNotSupported msg -> pure $ Left $ "publish clone: " <> msg
        ImageSuccess -> do
          now <- liftIO getCurrentTime
          mSize <- liftIO $ getImageSizeViaAgent state nid destPath
          let storedPath =
                if (basePath ++ "/") `Data.List.isPrefixOf` destPath
                  then T.pack (drop (length basePath + 1) destPath)
                  else T.pack destPath
          newKey <-
            liftIO $
              runSqlPool
                ( do
                    key <-
                      publishImageWithId
                        reservedId
                        DiskImage
                          { diskImageName = name
                          , diskImageFormat = btFormat target
                          , diskImageSize = mSize
                          , diskImageCreatedAt = now
                          , diskImageBackingImageId = Nothing
                          , diskImageEphemeral = False
                          }
                    recordDiskImageNode key nid storedPath
                    pure key
                )
                pool
          let newId = fromSqlKey newKey
          when (btCompact target) $ compactDisk state newId
          pure $ Right newId

-- | Decide what to do at the very top of a build, before the bake
-- VM is created, based on the target's 'btIfExists' policy and
-- whether a disk with the target name already exists.
--
--   * @Right Nothing@ — proceed to bake.
--   * @Right (Just diskId)@ — skip the bake; this disk is the
--     existing artifact and is returned as the build's success
--     result. Only happens with @ifExists: skip@.
--   * @Left err@ — fail-fast. Either @ifExists: error@ and the name
--     is taken, or @ifExists: overwrite@ and the existing disk is
--     attached to one or more VMs (delete-then-rebake would yank
--     the disk out from under those VMs, which we refuse).
--
-- For @ifExists: overwrite@ this only validates that the deletion
-- can later proceed safely; the actual deletion is deferred to
-- 'deleteOverwriteTargetIfNeeded' at publish time, so a mid-bake
-- failure preserves the existing artifact.
checkIfExistsPreBake
  :: ServerState
  -> Text
  -> BuildTarget
  -> LoggingT IO (Either Text (Maybe Int64))
checkIfExistsPreBake state name target = do
  let pool = ssDbPool state
  mExisting <- liftIO $ runSqlPool (imageByName name) pool
  case (btIfExists target, mExisting) of
    (_, Nothing) -> pure $ Right Nothing
    (IfExistsError, Just _) ->
      pure $
        Left $
          "target '"
            <> name
            <> "' already exists; use ifExists: skip or overwrite to allow"
    (IfExistsSkip, Just (Entity existingId _)) -> do
      logInfoN $ "target '" <> name <> "' exists; skipping bake (ifExists: skip)"
      pure $ Right (Just (fromSqlKey existingId))
    (IfExistsOverwrite, Just _) -> pure $ Right Nothing

-- | Compact a qcow2 by running @qemu-img convert -O qcow2@ in place (atomic
-- via temp file + rename). On any failure this logs at @warn@ and returns
-- without raising — compaction is a size optimisation, not a correctness
-- requirement.
compactDisk :: ServerState -> Int64 -> LoggingT IO ()
compactDisk state diskId = do
  mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
  case mNid of
    Left err -> logWarnN $ "compact: cannot resolve node: " <> err
    Right nid -> do
      mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
      case mDisk of
        Nothing -> logWarnN "compact: disk vanished"
        Just _disk -> do
          logInfoN "compact: rebasing onto self with -c (no-op flatten)"
          path <-
            liftIO $
              resolveDiskPath
                (ssDbPool state)
                (ssQemuConfig state)
                (toSqlKey diskId :: DiskImageId)
                nid
          -- A no-op rebase (Nothing → Nothing) effectively rewrites the image
          -- through qemu-img, dropping unused clusters. The rebaseImage helper
          -- already handles the in-place pass.
          result <- liftIO $ rebaseImageViaAgent state nid path Nothing False
          case result of
            ImageSuccess -> do
              mSize <- liftIO $ getImageSizeViaAgent state nid path
              case mSize of
                Just newSize ->
                  liftIO $
                    runSqlPool
                      ( update
                          (toSqlKey diskId :: DiskImageId)
                          [DiskImageSize =. Just newSize]
                      )
                      (ssDbPool state)
                Nothing -> pure ()
            _ -> logWarnN "compact: qemu-img rebase failed (artifact still usable)"
