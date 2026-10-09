{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | DSL primitives for assertions (Then phase).
--
-- The legacy result sums ('VmActionResult', 'DiskResult', ...) are
-- gone in Phase 5; what remains here are the Response-shaped
-- assertions and the database-state assertions. The
-- result-sum-shaped @then*@ helpers used by the old test suite were
-- dropped along with their types — call sites either use
-- 'responseIs' directly or assert on database state.
module Test.DSL.Then
  ( -- * Response assertions
    responseIs
  , responseIsVmNotFound
  , responseIsDiskNotFound
  , responseIsDiskInUse
  , responseIsDiskHasOverlays
  , responseIsSnapshotNotFound

    -- * Database state assertions
  , vmExists
  , vmNotExists
  , vmHasStatus
  , vmHasTpm
  , vmCount
  , diskImageExists
  , diskImageNotExists
  , diskImageCount
  , driveExistsForVm
  , driveCountForVm
  , driveMediaIs

    -- * Task database assertions
  , taskCount
  , getLastTask
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Corvus.Model
import Corvus.Protocol
import Data.Int (Int64)
import Data.Maybe (isJust, isNothing)
import Database.Persist
import Test.DSL.Core (TestM, getLastResponse, runDb)
import Test.Hspec.Expectations (shouldBe, shouldSatisfy)

--------------------------------------------------------------------------------
-- Response Assertions
--------------------------------------------------------------------------------

responseIs :: (Response -> Bool) -> TestM ()
responseIs predicate = do
  mResp <- getLastResponse
  case mResp of
    Nothing -> liftIO $ fail "No response captured"
    Just resp ->
      unless (predicate resp) $
        liftIO $
          fail $
            "Response did not match predicate. Got: " <> show resp

responseIsVmNotFound :: TestM ()
responseIsVmNotFound = responseIs (== RespVmNotFound)

responseIsDiskNotFound :: TestM ()
responseIsDiskNotFound = responseIs (== RespDiskNotFound)

responseIsDiskInUse :: TestM ()
responseIsDiskInUse = responseIs $ \case
  RespDiskInUse _ -> True
  _ -> False

responseIsDiskHasOverlays :: TestM ()
responseIsDiskHasOverlays = responseIs $ \case
  RespDiskHasOverlays _ -> True
  _ -> False

responseIsSnapshotNotFound :: TestM ()
responseIsSnapshotNotFound = responseIs (== RespSnapshotNotFound)

--------------------------------------------------------------------------------
-- Database State Assertions
--------------------------------------------------------------------------------

vmExists :: Int64 -> TestM ()
vmExists vmId = do
  mVm <- runDb $ get (toSqlKey vmId :: VmId)
  liftIO $ mVm `shouldSatisfy` isJust

vmNotExists :: Int64 -> TestM ()
vmNotExists vmId = do
  mVm <- runDb $ get (toSqlKey vmId :: VmId)
  liftIO $ mVm `shouldSatisfy` isNothing

vmHasStatus :: Int64 -> VmStatus -> TestM ()
vmHasStatus vmId expectedStatus = do
  mVm <- runDb $ get (toSqlKey vmId :: VmId)
  case mVm of
    Nothing -> liftIO $ fail $ "VM not found: " <> show vmId
    Just vm -> liftIO $ vmStatus vm `shouldBe` expectedStatus

vmHasTpm :: Int64 -> Bool -> TestM ()
vmHasTpm vmId expected = do
  mVm <- runDb $ get (toSqlKey vmId :: VmId)
  case mVm of
    Nothing -> liftIO $ fail $ "VM not found: " <> show vmId
    Just vm -> liftIO $ vmTpm vm `shouldBe` expected

vmCount :: Int -> TestM ()
vmCount expectedCount = do
  cnt <- runDb $ count ([] :: [Filter Vm])
  liftIO $ cnt `shouldBe` expectedCount

diskImageExists :: Int64 -> TestM ()
diskImageExists diskId = do
  mDisk <- runDb $ get (toSqlKey diskId :: DiskImageId)
  liftIO $ mDisk `shouldSatisfy` isJust

diskImageNotExists :: Int64 -> TestM ()
diskImageNotExists diskId = do
  mDisk <- runDb $ get (toSqlKey diskId :: DiskImageId)
  liftIO $ mDisk `shouldSatisfy` isNothing

diskImageCount :: Int -> TestM ()
diskImageCount expectedCount = do
  cnt <- runDb $ count ([] :: [Filter DiskImage])
  liftIO $ cnt `shouldBe` expectedCount

driveExistsForVm :: Int64 -> Int64 -> TestM ()
driveExistsForVm vmId diskImageId = do
  let vmKey = toSqlKey vmId :: VmId
      diskKey = toSqlKey diskImageId :: DiskImageId
  drives <- runDb $ selectList [DriveVmId ==. vmKey, DriveDiskImageId ==. Just diskKey] []
  liftIO $ length drives `shouldSatisfy` (> 0)

driveCountForVm :: Int64 -> Int -> TestM ()
driveCountForVm vmId expectedCount = do
  let vmKey = toSqlKey vmId :: VmId
  cnt <- runDb $ count [DriveVmId ==. vmKey]
  liftIO $ cnt `shouldBe` expectedCount

-- | Assert that drive @driveId@'s @driveDiskImageId@ equals
-- @mDiskImageId@ ('Nothing' for an ejected / empty tray).
driveMediaIs :: Int64 -> Maybe Int64 -> TestM ()
driveMediaIs driveId mDiskImageId = do
  mDrive <- runDb $ get (toSqlKey driveId :: DriveId)
  case mDrive of
    Nothing -> liftIO $ fail $ "Drive not found: " <> show driveId
    Just d -> liftIO $ fmap fromSqlKey (driveDiskImageId d) `shouldBe` mDiskImageId

--------------------------------------------------------------------------------
-- Task Assertions
--------------------------------------------------------------------------------

taskCount :: Int -> TestM ()
taskCount expectedCount = do
  cnt <- runDb $ count ([] :: [Filter Task])
  liftIO $ cnt `shouldBe` expectedCount

getLastTask :: TestM (Maybe (Entity Task))
getLastTask = do
  rs <- runDb $ selectList ([] :: [Filter Task]) [Desc TaskStartedAt, LimitTo 1]
  pure $ case rs of
    (t : _) -> Just t
    [] -> Nothing
