{-# LANGUAGE OverloadedStrings #-}

-- | DSL primitives for assertions (Then phase).
-- Provides functions to verify database state and responses.
module Test.DSL.Then
  ( -- * Response assertions
    responseIs,
    responseMatches,
    responseIsSuccess,
    responseIsVmNotFound,
    responseIsVmStateChanged,
    responseIsInvalidTransition,
    responseIsDiskCreated,
    responseIsDiskNotFound,
    responseIsDiskInUse,
    responseIsDiskHasOverlays,
    responseIsSnapshotCreated,
    responseIsSnapshotNotFound,

    -- * Shared directory response assertions
    thenSharedDirListIsEmpty,
    thenSharedDirListHasCount,
    thenSharedDirAdded,
    thenSharedDirOk,
    thenSharedDirNotFound,
    thenSharedDirError,
    thenVmNotFound,

    -- * Database state assertions
    vmExists,
    vmNotExists,
    vmHasStatus,
    vmHasPid,
    vmCount,
    diskImageExists,
    diskImageNotExists,
    diskImageCount,
    driveExists,
    driveNotExists,
    driveExistsForVm,
    driveCountForVm,
    snapshotExists,
    snapshotNotExists,
    snapshotCountForDisk,

    -- * General assertions
    shouldBeTrue,
    shouldBeFalse,
    shouldEqual,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Corvus.Client.Rpc (DiskResult (..), SharedDirResult (..), SnapshotResult (..), VmActionResult (..), VmCreateResult (..), VmDeleteResult (..))
import Corvus.Model
import Corvus.Protocol
import Data.Int (Int64)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (toSqlKey)
import Test.DSL.Core (TestM, getLastResponse, runDb)
import Test.Hspec.Expectations (shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------
-- Response Assertions
--------------------------------------------------------------------------------

-- | Assert that the last response matches a predicate
responseIs :: (Response -> Bool) -> TestM ()
responseIs predicate = do
  mResp <- getLastResponse
  case mResp of
    Nothing -> liftIO $ fail "No response captured"
    Just resp ->
      liftIO $ resp `shouldSatisfy` predicate

-- | Assert that the last response matches an expected value
responseMatches :: Response -> TestM ()
responseMatches expected = do
  mResp <- getLastResponse
  case mResp of
    Nothing -> liftIO $ fail "No response captured"
    Just resp -> liftIO $ resp `shouldBe` expected

-- | Assert response indicates success (generic)
responseIsSuccess :: TestM ()
responseIsSuccess = responseIs isSuccessResponse
  where
    isSuccessResponse RespPong = True
    isSuccessResponse (RespVmStateChanged _) = True
    isSuccessResponse (RespVmList _) = True
    isSuccessResponse (RespVmDetails _) = True
    isSuccessResponse (RespDiskList _) = True
    isSuccessResponse (RespDiskInfo _) = True
    isSuccessResponse (RespDiskCreated _) = True
    isSuccessResponse (RespDiskAttached _) = True
    isSuccessResponse RespDiskOk = True
    isSuccessResponse (RespSnapshotList _) = True
    isSuccessResponse (RespSnapshotCreated _) = True
    isSuccessResponse RespSnapshotOk = True
    isSuccessResponse _ = False

-- | Assert response is VM not found
responseIsVmNotFound :: TestM ()
responseIsVmNotFound = responseMatches RespVmNotFound

-- | Assert response is VM state changed
responseIsVmStateChanged :: TestM ()
responseIsVmStateChanged = responseIs isStateChanged
  where
    isStateChanged (RespVmStateChanged _) = True
    isStateChanged _ = False

-- | Assert response is invalid transition
responseIsInvalidTransition :: TestM ()
responseIsInvalidTransition = responseIs isInvalidTransition
  where
    isInvalidTransition (RespInvalidTransition _ _) = True
    isInvalidTransition _ = False

-- | Assert response is disk created
responseIsDiskCreated :: TestM ()
responseIsDiskCreated = responseIs isDiskCreated
  where
    isDiskCreated (RespDiskCreated _) = True
    isDiskCreated _ = False

-- | Assert response is disk not found
responseIsDiskNotFound :: TestM ()
responseIsDiskNotFound = responseMatches RespDiskNotFound

-- | Assert response is disk in use
responseIsDiskInUse :: TestM ()
responseIsDiskInUse = responseIs isDiskInUse
  where
    isDiskInUse (RespDiskInUse _) = True
    isDiskInUse _ = False

-- | Assert response is disk has overlays
responseIsDiskHasOverlays :: TestM ()
responseIsDiskHasOverlays = responseIs isDiskHasOverlays
  where
    isDiskHasOverlays (RespDiskHasOverlays _) = True
    isDiskHasOverlays _ = False

-- | Assert response is snapshot created
responseIsSnapshotCreated :: TestM ()
responseIsSnapshotCreated = responseIs isSnapshotCreated
  where
    isSnapshotCreated (RespSnapshotCreated _) = True
    isSnapshotCreated _ = False

-- | Assert response is snapshot not found
responseIsSnapshotNotFound :: TestM ()
responseIsSnapshotNotFound = responseMatches RespSnapshotNotFound

--------------------------------------------------------------------------------
-- VM Database Assertions
--------------------------------------------------------------------------------

-- | Assert that a VM exists in the database
vmExists :: Int64 -> TestM ()
vmExists vmId = do
  mVm <- runDb $ get (toSqlKey vmId :: Key Vm)
  liftIO $ mVm `shouldSatisfy` isJust

-- | Assert that a VM does not exist in the database
vmNotExists :: Int64 -> TestM ()
vmNotExists vmId = do
  mVm <- runDb $ get (toSqlKey vmId :: Key Vm)
  liftIO $ mVm `shouldSatisfy` isNothing

-- | Assert that a VM has a specific status
vmHasStatus :: Int64 -> VmStatus -> TestM ()
vmHasStatus vmId expectedStatus = do
  mVm <- runDb $ get (toSqlKey vmId :: Key Vm)
  case mVm of
    Nothing -> liftIO $ fail $ "VM " ++ show vmId ++ " not found"
    Just vm -> liftIO $ vmStatus vm `shouldBe` expectedStatus

-- | Assert that a VM has a PID (is running)
vmHasPid :: Int64 -> TestM ()
vmHasPid vmId = do
  mVm <- runDb $ get (toSqlKey vmId :: Key Vm)
  case mVm of
    Nothing -> liftIO $ fail $ "VM " ++ show vmId ++ " not found"
    Just vm -> liftIO $ vmPid vm `shouldSatisfy` isJust

-- | Assert the total number of VMs in the database
vmCount :: Int -> TestM ()
vmCount expected = do
  vms <- runDb $ selectList ([] :: [Filter Vm]) []
  liftIO $ length vms `shouldBe` expected

--------------------------------------------------------------------------------
-- Disk Image Database Assertions
--------------------------------------------------------------------------------

-- | Assert that a disk image exists
diskImageExists :: Int64 -> TestM ()
diskImageExists diskId = do
  mDisk <- runDb $ get (toSqlKey diskId :: Key DiskImage)
  liftIO $ mDisk `shouldSatisfy` isJust

-- | Assert that a disk image does not exist
diskImageNotExists :: Int64 -> TestM ()
diskImageNotExists diskId = do
  mDisk <- runDb $ get (toSqlKey diskId :: Key DiskImage)
  liftIO $ mDisk `shouldSatisfy` isNothing

-- | Assert the total number of disk images
diskImageCount :: Int -> TestM ()
diskImageCount expected = do
  disks <- runDb $ selectList ([] :: [Filter DiskImage]) []
  liftIO $ length disks `shouldBe` expected

--------------------------------------------------------------------------------
-- Drive Database Assertions
--------------------------------------------------------------------------------

-- | Assert that a drive exists
driveExists :: Int64 -> TestM ()
driveExists driveId = do
  mDrive <- runDb $ get (toSqlKey driveId :: Key Drive)
  liftIO $ mDrive `shouldSatisfy` isJust

-- | Assert that a drive does not exist
driveNotExists :: Int64 -> TestM ()
driveNotExists driveId = do
  mDrive <- runDb $ get (toSqlKey driveId :: Key Drive)
  liftIO $ mDrive `shouldSatisfy` isNothing

-- | Assert that a drive exists for a specific VM
driveExistsForVm :: Int64 -> Int64 -> TestM ()
driveExistsForVm vmId diskImageId = do
  drives <-
    runDb $
      selectList
        [ DriveVmId ==. toSqlKey vmId,
          DriveDiskImageId ==. toSqlKey diskImageId
        ]
        []
  liftIO $ drives `shouldSatisfy` (not . null)

-- | Assert the number of drives for a VM
driveCountForVm :: Int64 -> Int -> TestM ()
driveCountForVm vmId expected = do
  drives <- runDb $ selectList [DriveVmId ==. toSqlKey vmId] []
  liftIO $ length drives `shouldBe` expected

--------------------------------------------------------------------------------
-- Snapshot Database Assertions
--------------------------------------------------------------------------------

-- | Assert that a snapshot exists
snapshotExists :: Int64 -> TestM ()
snapshotExists snapshotId = do
  mSnapshot <- runDb $ get (toSqlKey snapshotId :: Key Snapshot)
  liftIO $ mSnapshot `shouldSatisfy` isJust

-- | Assert that a snapshot does not exist
snapshotNotExists :: Int64 -> TestM ()
snapshotNotExists snapshotId = do
  mSnapshot <- runDb $ get (toSqlKey snapshotId :: Key Snapshot)
  liftIO $ mSnapshot `shouldSatisfy` isNothing

-- | Assert the number of snapshots for a disk
snapshotCountForDisk :: Int64 -> Int -> TestM ()
snapshotCountForDisk diskId expected = do
  snapshots <-
    runDb $
      selectList [SnapshotDiskImageId ==. toSqlKey diskId] []
  liftIO $ length snapshots `shouldBe` expected

--------------------------------------------------------------------------------
-- General Assertions
--------------------------------------------------------------------------------

-- | Assert that a boolean value is true
shouldBeTrue :: Bool -> TestM ()
shouldBeTrue value = liftIO $ value `shouldBe` True

-- | Assert that a boolean value is false
shouldBeFalse :: Bool -> TestM ()
shouldBeFalse value = liftIO $ value `shouldBe` False

-- | Assert equality
shouldEqual :: (Show a, Eq a) => a -> a -> TestM ()
shouldEqual actual expected = liftIO $ actual `shouldBe` expected

--------------------------------------------------------------------------------
-- Shared Directory Response Assertions
--------------------------------------------------------------------------------

-- | Assert shared directory list is empty
thenSharedDirListIsEmpty :: SharedDirResult -> TestM ()
thenSharedDirListIsEmpty (SharedDirListResult []) = pure ()
thenSharedDirListIsEmpty other = liftIO $ fail $ "Expected empty shared dir list, got: " <> show other

-- | Assert shared directory list has expected count
thenSharedDirListHasCount :: SharedDirResult -> Int -> TestM ()
thenSharedDirListHasCount (SharedDirListResult dirs) expected =
  liftIO $ length dirs `shouldBe` expected
thenSharedDirListHasCount other _ =
  liftIO $ fail $ "Expected shared dir list, got: " <> show other

-- | Assert shared directory was added
thenSharedDirAdded :: SharedDirResult -> TestM ()
thenSharedDirAdded (SharedDirAdded _) = pure ()
thenSharedDirAdded other =
  liftIO $ fail $ "Expected shared dir added, got: " <> show other

-- | Assert shared directory operation was successful
thenSharedDirOk :: SharedDirResult -> TestM ()
thenSharedDirOk SharedDirOk = pure ()
thenSharedDirOk other =
  liftIO $ fail $ "Expected shared dir ok, got: " <> show other

-- | Assert shared directory not found
thenSharedDirNotFound :: SharedDirResult -> TestM ()
thenSharedDirNotFound SharedDirNotFound = pure ()
thenSharedDirNotFound other =
  liftIO $ fail $ "Expected shared dir not found, got: " <> show other

-- | Assert shared directory error with message
thenSharedDirError :: SharedDirResult -> Text -> TestM ()
thenSharedDirError (SharedDirError msg) expected =
  liftIO $ msg `shouldSatisfy` T.isInfixOf expected
thenSharedDirError other _ =
  liftIO $ fail $ "Expected shared dir error, got: " <> show other

-- | Assert VM not found (generic result)
thenVmNotFound :: (Show a) => a -> TestM ()
thenVmNotFound result = case show result of
  s | "VmNotFound" `T.isInfixOf` T.pack s -> pure ()
  _ -> liftIO $ fail $ "Expected VM not found, got: " <> show result
