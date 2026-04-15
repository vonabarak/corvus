{-# LANGUAGE OverloadedStrings #-}

-- | Rebase and flatten disk overlays.
--
-- Rebase changes an overlay's backing image pointer; flatten merges the
-- backing contents into the overlay and clears the pointer. Both operate
-- on qcow2 only, require the attached VMs to be stopped, and refresh the
-- disk's stored size afterwards because content changes.
--
-- The @unsafe@ flag is passed through to @qemu-img rebase -u@: it skips
-- the data-transformation pass and just rewrites the backing pointer.
-- Only safe when the old and new backing images have identical contents.
module Corvus.Handlers.Disk.Rebase
  ( DiskRebase (..)
  , handleDiskRebase
  )
where

import Corvus.Action
import Corvus.Handlers.Disk.Db (getRunningAttachedVms, isCircularBacking)
import Corvus.Handlers.Disk.Path (resolveDiskPath)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Model
import Corvus.Protocol
import Corvus.Qemu.Image
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)

-- | Rebase an overlay to a different backing image, or flatten (remove backing).
handleDiskRebase :: ServerState -> Int64 -> Maybe Int64 -> Bool -> IO Response
handleDiskRebase state diskId mNewBackingId unsafe = runServerLogging state $ do
  logInfoN $ "Rebasing disk image: " <> T.pack (show diskId)

  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> pure RespDiskNotFound
    Just disk -> do
      -- Must be qcow2
      if diskImageFormat disk /= FormatQcow2
        then pure $ RespFormatNotSupported "Rebase requires qcow2 format"
        else case diskImageBackingImageId disk of
          Nothing -> pure $ RespError "Disk is not an overlay"
          Just _ -> do
            -- Must not be attached to running/paused VMs
            runningVms <- liftIO $ runSqlPool (getRunningAttachedVms diskId) (ssDbPool state)
            if not (null runningVms)
              then pure RespVmMustBeStopped
              else do
                overlayPath <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
                case mNewBackingId of
                  -- Flatten: remove backing
                  Nothing -> do
                    logInfoN "Flattening overlay (removing backing)"
                    result <- liftIO $ rebaseImage overlayPath Nothing unsafe
                    case result of
                      ImageSuccess -> do
                        liftIO $
                          runSqlPool
                            (update (toSqlKey diskId :: DiskImageId) [DiskImageBackingImageId =. Nothing])
                            (ssDbPool state)
                        -- Refresh size since flatten merges backing data
                        mSize <- liftIO $ getImageSizeMb overlayPath
                        case mSize of
                          Just newSize ->
                            liftIO $
                              runSqlPool
                                (update (toSqlKey diskId :: DiskImageId) [DiskImageSizeMb =. Just newSize])
                                (ssDbPool state)
                          Nothing -> pure ()
                        logInfoN "Overlay flattened successfully"
                        pure RespDiskOk
                      ImageNotFound -> pure $ RespError "Overlay file not found on disk"
                      ImageError err -> do
                        logWarnN $ "Failed to flatten overlay: " <> err
                        pure $ RespError err
                      _ -> pure $ RespError "Unexpected rebase result"
                  -- Rebase to new backing
                  Just newBackingId -> do
                    mNewBacking <- liftIO $ runSqlPool (get (toSqlKey newBackingId :: DiskImageId)) (ssDbPool state)
                    case mNewBacking of
                      Nothing -> pure $ RespError "New backing image not found"
                      Just newBacking -> do
                        -- Check circular dependency
                        circular <- liftIO $ runSqlPool (isCircularBacking diskId newBackingId) (ssDbPool state)
                        if circular
                          then pure $ RespError "Circular backing dependency detected"
                          else do
                            newBackingPath <- liftIO $ resolveDiskPath (ssQemuConfig state) newBacking
                            logInfoN $ "Rebasing to new backing: " <> T.pack newBackingPath
                            result <- liftIO $ rebaseImage overlayPath (Just (newBackingPath, diskImageFormat newBacking)) unsafe
                            case result of
                              ImageSuccess -> do
                                liftIO $
                                  runSqlPool
                                    (update (toSqlKey diskId :: DiskImageId) [DiskImageBackingImageId =. Just (toSqlKey newBackingId)])
                                    (ssDbPool state)
                                -- Refresh size
                                mSize <- liftIO $ getImageSizeMb overlayPath
                                case mSize of
                                  Just newSize ->
                                    liftIO $
                                      runSqlPool
                                        (update (toSqlKey diskId :: DiskImageId) [DiskImageSizeMb =. Just newSize])
                                        (ssDbPool state)
                                  Nothing -> pure ()
                                logInfoN "Overlay rebased successfully"
                                pure RespDiskOk
                              ImageNotFound -> pure $ RespError "Overlay file not found on disk"
                              ImageError err -> do
                                logWarnN $ "Failed to rebase overlay: " <> err
                                pure $ RespError err
                              _ -> pure $ RespError "Unexpected rebase result"

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

data DiskRebase = DiskRebase
  { drbDiskId :: Int64
  , drbNewBackingId :: Maybe Int64
  , drbUnsafe :: Bool
  }

instance Action DiskRebase where
  actionSubsystem _ = SubDisk
  actionCommand (DiskRebase _ Nothing _) = "flatten"
  actionCommand (DiskRebase _ (Just _) _) = "rebase"
  actionEntityId = Just . fromIntegral . drbDiskId
  actionExecute ctx a = handleDiskRebase (acState ctx) (drbDiskId a) (drbNewBackingId a) (drbUnsafe a)
