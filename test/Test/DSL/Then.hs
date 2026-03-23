{-# LANGUAGE OverloadedStrings #-}

-- | DSL primitives for assertions (Then phase).
-- Provides functions to verify database state and responses.
module Test.DSL.Then
  ( -- * Response assertions
    responseIs
  , responseMatches
  , responseIsSuccess
  , responseIsVmNotFound
  , responseIsVmStateChanged
  , responseIsInvalidTransition
  , responseIsDiskCreated
  , responseIsDiskNotFound
  , responseIsDiskInUse
  , responseIsDiskHasOverlays
  , responseIsSnapshotCreated
  , responseIsSnapshotNotFound

    -- * Shared directory response assertions
  , thenSharedDirListIsEmpty
  , thenSharedDirListHasCount
  , thenSharedDirAdded
  , thenSharedDirOk
  , thenSharedDirNotFound
  , thenSharedDirError
  , thenVmNotFound

    -- * Network interface response assertions
  , thenNetIfAdded
  , thenNetIfOk
  , thenNetIfNotFound
  , thenNetIfVmNotFound
  , thenNetIfListHasCount
  , thenNetIfListIsEmpty
  , thenNetIfError

    -- * SSH key response assertions
  , thenSshKeyCreated
  , thenSshKeyOk
  , thenSshKeyNotFound
  , thenSshKeyVmNotFound
  , thenSshKeyInUse
  , thenSshKeyListHasCount
  , thenSshKeyListIsEmpty
  , thenSshKeyError

    -- * VM edit response assertions
  , thenVmEdited
  , thenVmEditNotFound
  , thenVmEditMustBeStopped

    -- * VM create/delete response assertions
  , thenVmCreated
  , thenVmCreateError
  , thenVmDeleted
  , thenVmDeleteNotFound
  , thenVmDeleteRunning

    -- * Database state assertions
  , vmExists
  , vmNotExists
  , vmHasStatus
  , vmHasPid
  , vmCount
  , diskImageExists
  , diskImageNotExists
  , diskImageCount
  , driveExists
  , driveNotExists
  , driveExistsForVm
  , driveCountForVm
  , snapshotExists
  , snapshotNotExists
  , snapshotCountForDisk

    -- * General assertions
  , shouldBeTrue
  , shouldBeFalse
  , shouldEqual
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Corvus.Client.Rpc (DiskResult (..), NetIfResult (..), SharedDirResult (..), SnapshotResult (..), SshKeyResult (..), VmActionResult (..), VmCreateResult (..), VmDeleteResult (..), VmEditResult (..))
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
        [ DriveVmId ==. toSqlKey vmId
        , DriveDiskImageId ==. toSqlKey diskImageId
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

--------------------------------------------------------------------------------
-- Network Interface Response Assertions
--------------------------------------------------------------------------------

-- | Assert network interface was added
thenNetIfAdded :: NetIfResult -> TestM ()
thenNetIfAdded (NetIfAdded _) = pure ()
thenNetIfAdded other =
  liftIO $ fail $ "Expected net-if added, got: " <> show other

-- | Assert network interface operation was successful
thenNetIfOk :: NetIfResult -> TestM ()
thenNetIfOk NetIfOk = pure ()
thenNetIfOk other =
  liftIO $ fail $ "Expected net-if ok, got: " <> show other

-- | Assert network interface not found
thenNetIfNotFound :: NetIfResult -> TestM ()
thenNetIfNotFound NetIfNotFound = pure ()
thenNetIfNotFound other =
  liftIO $ fail $ "Expected net-if not found, got: " <> show other

-- | Assert VM not found for network interface operation
thenNetIfVmNotFound :: NetIfResult -> TestM ()
thenNetIfVmNotFound NetIfVmNotFound = pure ()
thenNetIfVmNotFound other =
  liftIO $ fail $ "Expected net-if VM not found, got: " <> show other

-- | Assert network interface list has expected count
thenNetIfListHasCount :: NetIfResult -> Int -> TestM ()
thenNetIfListHasCount (NetIfListResult netIfs) expected =
  liftIO $ length netIfs `shouldBe` expected
thenNetIfListHasCount other _ =
  liftIO $ fail $ "Expected net-if list, got: " <> show other

-- | Assert network interface list is empty
thenNetIfListIsEmpty :: NetIfResult -> TestM ()
thenNetIfListIsEmpty (NetIfListResult []) = pure ()
thenNetIfListIsEmpty other = liftIO $ fail $ "Expected empty net-if list, got: " <> show other

-- | Assert network interface error with message
thenNetIfError :: NetIfResult -> Text -> TestM ()
thenNetIfError (NetIfError msg) expected =
  liftIO $ msg `shouldSatisfy` T.isInfixOf expected
thenNetIfError other _ =
  liftIO $ fail $ "Expected net-if error, got: " <> show other

--------------------------------------------------------------------------------
-- SSH Key Response Assertions
--------------------------------------------------------------------------------

-- | Assert SSH key was created
thenSshKeyCreated :: SshKeyResult -> TestM ()
thenSshKeyCreated (SshKeyCreated _) = pure ()
thenSshKeyCreated other =
  liftIO $ fail $ "Expected ssh-key created, got: " <> show other

-- | Assert SSH key operation was successful
thenSshKeyOk :: SshKeyResult -> TestM ()
thenSshKeyOk SshKeyOk = pure ()
thenSshKeyOk other =
  liftIO $ fail $ "Expected ssh-key ok, got: " <> show other

-- | Assert SSH key not found
thenSshKeyNotFound :: SshKeyResult -> TestM ()
thenSshKeyNotFound SshKeyNotFound = pure ()
thenSshKeyNotFound other =
  liftIO $ fail $ "Expected ssh-key not found, got: " <> show other

-- | Assert VM not found for SSH key operation
thenSshKeyVmNotFound :: SshKeyResult -> TestM ()
thenSshKeyVmNotFound SshKeyVmNotFound = pure ()
thenSshKeyVmNotFound other =
  liftIO $ fail $ "Expected ssh-key VM not found, got: " <> show other

-- | Assert SSH key is in use
thenSshKeyInUse :: SshKeyResult -> TestM ()
thenSshKeyInUse (SshKeyInUse _) = pure ()
thenSshKeyInUse other =
  liftIO $ fail $ "Expected ssh-key in use, got: " <> show other

-- | Assert SSH key list has expected count
thenSshKeyListHasCount :: SshKeyResult -> Int -> TestM ()
thenSshKeyListHasCount (SshKeyListResult keys) expected =
  liftIO $ length keys `shouldBe` expected
thenSshKeyListHasCount other _ =
  liftIO $ fail $ "Expected ssh-key list, got: " <> show other

-- | Assert SSH key list is empty
thenSshKeyListIsEmpty :: SshKeyResult -> TestM ()
thenSshKeyListIsEmpty (SshKeyListResult []) = pure ()
thenSshKeyListIsEmpty other = liftIO $ fail $ "Expected empty ssh-key list, got: " <> show other

-- | Assert SSH key error with message
thenSshKeyError :: SshKeyResult -> Text -> TestM ()
thenSshKeyError (SshKeyError msg) expected =
  liftIO $ msg `shouldSatisfy` T.isInfixOf expected
thenSshKeyError other _ =
  liftIO $ fail $ "Expected ssh-key error, got: " <> show other

--------------------------------------------------------------------------------
-- VM Edit Response Assertions
--------------------------------------------------------------------------------

-- | Assert VM was edited
thenVmEdited :: VmEditResult -> TestM ()
thenVmEdited VmEdited = pure ()
thenVmEdited other =
  liftIO $ fail $ "Expected VM edited, got: " <> show other

-- | Assert VM edit not found
thenVmEditNotFound :: VmEditResult -> TestM ()
thenVmEditNotFound VmEditNotFound = pure ()
thenVmEditNotFound other =
  liftIO $ fail $ "Expected VM edit not found, got: " <> show other

-- | Assert VM edit must be stopped
thenVmEditMustBeStopped :: VmEditResult -> TestM ()
thenVmEditMustBeStopped VmEditMustBeStopped = pure ()
thenVmEditMustBeStopped other =
  liftIO $ fail $ "Expected VM edit must be stopped, got: " <> show other

--------------------------------------------------------------------------------
-- VM Create/Delete Response Assertions
--------------------------------------------------------------------------------

-- | Assert VM was created
thenVmCreated :: VmCreateResult -> TestM ()
thenVmCreated (VmCreated _) = pure ()
thenVmCreated other =
  liftIO $ fail $ "Expected VM created, got: " <> show other

-- | Assert VM create error
thenVmCreateError :: VmCreateResult -> TestM ()
thenVmCreateError (VmCreateError _) = pure ()
thenVmCreateError other =
  liftIO $ fail $ "Expected VM create error, got: " <> show other

-- | Assert VM was deleted
thenVmDeleted :: VmDeleteResult -> TestM ()
thenVmDeleted VmDeleted = pure ()
thenVmDeleted other =
  liftIO $ fail $ "Expected VM deleted, got: " <> show other

-- | Assert VM delete not found
thenVmDeleteNotFound :: VmDeleteResult -> TestM ()
thenVmDeleteNotFound VmDeleteNotFound = pure ()
thenVmDeleteNotFound other =
  liftIO $ fail $ "Expected VM delete not found, got: " <> show other

-- | Assert VM delete running
thenVmDeleteRunning :: VmDeleteResult -> TestM ()
thenVmDeleteRunning VmDeleteRunning = pure ()
thenVmDeleteRunning other =
  liftIO $ fail $ "Expected VM delete running, got: " <> show other
