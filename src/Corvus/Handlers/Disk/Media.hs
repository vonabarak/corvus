{-# LANGUAGE OverloadedStrings #-}

-- | Eject and change the media of a CD-ROM drive.
--
-- A CD-ROM drive attached with @media=cdrom@ can swap media even while
-- the VM is running: the agent-side session methods ('NOA.vmEjectMedia'
-- \/ 'NOA.vmChangeMedia') perform the runtime capability check via
-- QMP @query-block@ and then issue @eject@ / @blockdev-change-medium@
-- against the legacy @-drive id=drive-<N>@ backend. The daemon
-- updates the @drive@ row only after a successful QMP round-trip, so
-- a QMP failure never desynchronizes the database from the running
-- QEMU process. For a stopped VM only the database is touched; the
-- empty tray (or new media) takes effect at the next boot because the
-- QEMU command line is generated from the @drive@ row.
module Corvus.Handlers.Disk.Media
  ( -- * Action types
    MediaEject (..)
  , MediaChange (..)

    -- * Handlers
  , handleMediaEject
  , handleMediaChange
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN)
import Corvus.Action
import Corvus.Handlers.Disk.Db (diskImageNodeFilePathFor)
import Corvus.Handlers.Disk.Path (resolveDiskPath)
import Corvus.Model
import qualified Corvus.NodeAgentClient as NOA
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol
import Corvus.Types (ServerState (..), runServerLogging)
import Data.Int (Int64)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (runSqlPool)

-- | Eject the media of drive @driveId@ (a @drive@ row id).
handleMediaEject :: ServerState -> Int64 -> IO Response
handleMediaEject state driveId = runServerLogging state $ do
  logInfoN $ "Ejecting media of drive " <> T.pack (show driveId)

  -- Load the drive row
  mDrive <- liftIO $ runSqlPool (get (toSqlKey driveId :: DriveId)) (ssDbPool state)
  case mDrive of
    Nothing -> pure RespDriveNotFound
    Just drive -> do
      -- Load the owning VM
      mVm <- liftIO $ runSqlPool (get (driveVmId drive)) (ssDbPool state)
      case mVm of
        Nothing -> pure RespVmNotFound
        Just vm ->
          if driveMedia drive /= Just MediaCdrom
            then
              pure $
                RespError $
                  "Drive "
                    <> T.pack (show driveId)
                    <> " is not a removable CD-ROM; only drives attached with --media cdrom support eject"
            else
              if isNothing (driveDiskImageId drive)
                then
                  pure $
                    RespError $
                      "Drive "
                        <> T.pack (show driveId)
                        <> " has no media inserted (already ejected)"
                else
                  if vmStatus vm `elem` [VmStarting, VmRunning, VmPaused]
                    then do
                      let vmId = fromSqlKey (driveVmId drive)
                      logInfoN $ "VM is " <> enumToText (vmStatus vm) <> ", ejecting via QMP"
                      -- QMP first, DB second: on failure the running
                      -- VM keeps its media and the row is untouched.
                      outer <- liftIO $ withVmNodeAgent state vmId $ \nac ->
                        NOA.vmEjectMedia nac vmId driveId
                      case outer of
                        Left err -> pure $ RespError err
                        Right r -> case r of
                          Right () -> do
                            liftIO $
                              runSqlPool (update (toSqlKey driveId :: DriveId) [DriveDiskImageId =. Nothing]) (ssDbPool state)
                            logInfoN "Media ejected"
                            pure RespDiskOk
                          Left e -> pure $ RespError $ "vmEjectMedia: " <> T.pack (show e)
                    else do
                      -- Stopped VM: DB-only update; the tray is empty at
                      -- the next boot because the QEMU command line
                      -- omits `file=`/`format=` for media-less drives.
                      liftIO $
                        runSqlPool (update (toSqlKey driveId :: DriveId) [DriveDiskImageId =. Nothing]) (ssDbPool state)
                      logInfoN "Drive media ejected (database only; takes effect at next boot)"
                      pure RespDiskOk

-- | Change the media of drive @driveId@ to disk image @newDiskId@.
handleMediaChange :: ServerState -> Int64 -> Int64 -> IO Response
handleMediaChange state driveId newDiskId = runServerLogging state $ do
  logInfoN $
    "Changing media of drive "
      <> T.pack (show driveId)
      <> " to disk "
      <> T.pack (show newDiskId)

  -- Load the drive row
  mDrive <- liftIO $ runSqlPool (get (toSqlKey driveId :: DriveId)) (ssDbPool state)
  case mDrive of
    Nothing -> pure RespDriveNotFound
    Just drive -> do
      let vmId = fromSqlKey (driveVmId drive)
      -- Load the owning VM
      mVm <- liftIO $ runSqlPool (get (driveVmId drive)) (ssDbPool state)
      case mVm of
        Nothing -> pure RespVmNotFound
        Just vm ->
          if driveMedia drive /= Just MediaCdrom
            then
              pure $
                RespError $
                  "Drive "
                    <> T.pack (show driveId)
                    <> " is not a removable CD-ROM; only drives attached with --media cdrom support media change"
            else do
              -- Load the new disk image
              mDisk <- liftIO $ runSqlPool (get (toSqlKey newDiskId :: DiskImageId)) (ssDbPool state)
              case mDisk of
                Nothing -> pure RespDiskNotFound
                Just disk -> do
                  -- Same-node invariant: the image must be present on
                  -- the VM's node (mirror of 'handleDiskAttach').
                  mPlacement <-
                    liftIO $
                      runSqlPool
                        ( diskImageNodeFilePathFor
                            (toSqlKey newDiskId)
                            (vmNodeId vm)
                        )
                        (ssDbPool state)
                  case mPlacement of
                    Nothing ->
                      pure $
                        RespError $
                          "Disk image '"
                            <> diskImageName disk
                            <> "' is not present on node "
                            <> T.pack (show (fromSqlKey (vmNodeId vm)))
                            <> " where VM '"
                            <> vmName vm
                            <> "' lives"
                    Just _ -> do
                      -- Uniqueness: one drive per (VM, image) pair.
                      mExisting <-
                        liftIO $
                          runSqlPool
                            ( getBy
                                ( UniqueDrive
                                    (toSqlKey vmId)
                                    (Just (toSqlKey newDiskId))
                                )
                            )
                            (ssDbPool state)
                      if isJust mExisting
                        then pure $ RespError "Disk is already attached to this VM"
                        else
                          if vmStatus vm `elem` [VmStarting, VmRunning, VmPaused]
                            then do
                              logInfoN $ "VM is " <> enumToText (vmStatus vm) <> ", changing media via QMP"
                              filePath <-
                                liftIO $
                                  resolveDiskPath
                                    (ssDbPool state)
                                    (ssQemuConfig state)
                                    (toSqlKey newDiskId)
                                    (vmNodeId vm)
                              let fmtTxt = enumToText (diskImageFormat disk)
                              -- QMP first, DB second: on failure the
                              -- running VM keeps its current media and
                              -- the row is untouched.
                              outer <- liftIO $ withVmNodeAgent state vmId $ \nac ->
                                NOA.vmChangeMedia nac vmId driveId (T.pack filePath) fmtTxt
                              case outer of
                                Left err -> pure $ RespError err
                                Right r -> case r of
                                  Right () -> do
                                    liftIO $
                                      runSqlPool (update (toSqlKey driveId :: DriveId) [DriveDiskImageId =. Just (toSqlKey newDiskId)]) (ssDbPool state)
                                    logInfoN "Media changed"
                                    pure RespDiskOk
                                  Left e -> pure $ RespError $ "vmChangeMedia: " <> T.pack (show e)
                            else do
                              -- Stopped VM: DB-only update; the new
                              -- media is picked up at the next boot.
                              liftIO $
                                runSqlPool (update (toSqlKey driveId :: DriveId) [DriveDiskImageId =. Just (toSqlKey newDiskId)]) (ssDbPool state)
                              logInfoN "Drive media changed (database only; takes effect at next boot)"
                              pure RespDiskOk

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

newtype MediaEject = MediaEject
  { meDriveId :: Int64
  }

instance Action MediaEject where
  actionSubsystem _ = SubDisk
  actionCommand _ = "media eject"
  actionEntityId = Just . fromIntegral . meDriveId
  actionExecute ctx a = handleMediaEject (acState ctx) (meDriveId a)

data MediaChange = MediaChange
  { mcDriveId :: Int64
  , mcDiskId :: Int64
  }

instance Action MediaChange where
  actionSubsystem _ = SubDisk
  actionCommand _ = "media change"
  actionEntityId = Just . fromIntegral . mcDriveId
  actionExecute ctx a = handleMediaChange (acState ctx) (mcDriveId a) (mcDiskId a)
