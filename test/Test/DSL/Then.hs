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

    -- * Database state assertions
  , vmExists
  , vmNotExists
  , diskImageHasPath
  , vmHasStatus
  , vmCount
  , diskImageExists
  , diskImageNotExists
  , diskImageHasBacking
  , diskImageCount
  , driveExists
  , driveNotExists
  , driveExistsForVm
  , driveCountForVm
  , snapshotExists
  , snapshotNotExists
  , snapshotCountForDisk

    -- * Task database assertions
  , taskCount
  , taskExists
  , taskNotExists
  , subtaskCount
  , subtaskExists
  , getLastTask

    -- * General assertions
  , shouldBeTrue
  , shouldBeFalse
  , shouldEqual
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Corvus.Model
import Corvus.Protocol
import Data.Int (Int64)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql (fromSqlKey, toSqlKey)
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

responseMatches :: Response -> TestM ()
responseMatches expected = do
  mResp <- getLastResponse
  case mResp of
    Nothing -> liftIO $ fail "No response captured"
    Just resp -> liftIO $ resp `shouldBe` expected

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

responseIsVmNotFound :: TestM ()
responseIsVmNotFound = responseIs (== RespVmNotFound)

responseIsVmStateChanged :: VmStatus -> TestM ()
responseIsVmStateChanged status = responseIs (== RespVmStateChanged status)

responseIsInvalidTransition :: TestM ()
responseIsInvalidTransition = responseIs $ \case
  RespInvalidTransition _ _ -> True
  _ -> False

responseIsDiskCreated :: TestM ()
responseIsDiskCreated = responseIs $ \case
  RespDiskCreated _ -> True
  _ -> False

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

responseIsSnapshotCreated :: TestM ()
responseIsSnapshotCreated = responseIs $ \case
  RespSnapshotCreated _ -> True
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

-- | Assert that the disk has at least one 'DiskImageNode'
-- placement whose stored 'filePath' matches @expectedPath@. The
-- match is exact against the stored value (the daemon normalises
-- paths under @basePath@ to a relative form before insert, so
-- callers should pass the same relative form).
diskImageHasPath :: Int64 -> Text -> TestM ()
diskImageHasPath diskId expectedPath = do
  mDisk <- runDb $ get (toSqlKey diskId :: DiskImageId)
  case mDisk of
    Nothing -> liftIO $ fail $ "Disk image not found: " <> show diskId
    Just _ -> do
      placements <-
        runDb $
          selectList
            [DiskImageNodeDiskImageId ==. toSqlKey diskId]
            []
      let paths = map (diskImageNodeFilePath . entityVal) placements
      liftIO $
        unless (expectedPath `elem` paths) $
          fail $
            "Disk "
              <> show diskId
              <> " has no placement with path "
              <> show expectedPath
              <> "; placements: "
              <> show paths

vmHasStatus :: Int64 -> VmStatus -> TestM ()
vmHasStatus vmId expectedStatus = do
  mVm <- runDb $ get (toSqlKey vmId :: VmId)
  case mVm of
    Nothing -> liftIO $ fail $ "VM not found: " <> show vmId
    Just vm -> liftIO $ vmStatus vm `shouldBe` expectedStatus

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

diskImageHasBacking :: Int64 -> Int64 -> TestM ()
diskImageHasBacking diskId expectedBackingId = do
  mDisk <- runDb $ get (toSqlKey diskId :: DiskImageId)
  case mDisk of
    Nothing -> liftIO $ fail $ "Disk image not found: " <> show diskId
    Just d -> liftIO $ fmap fromSqlKey (diskImageBackingImageId d) `shouldBe` Just expectedBackingId

diskImageCount :: Int -> TestM ()
diskImageCount expectedCount = do
  cnt <- runDb $ count ([] :: [Filter DiskImage])
  liftIO $ cnt `shouldBe` expectedCount

driveExists :: Int64 -> TestM ()
driveExists driveId = do
  mDrive <- runDb $ get (toSqlKey driveId :: DriveId)
  liftIO $ mDrive `shouldSatisfy` isJust

driveNotExists :: Int64 -> TestM ()
driveNotExists driveId = do
  mDrive <- runDb $ get (toSqlKey driveId :: DriveId)
  liftIO $ mDrive `shouldSatisfy` isNothing

driveExistsForVm :: Int64 -> Int64 -> TestM ()
driveExistsForVm vmId diskImageId = do
  let vmKey = toSqlKey vmId :: VmId
      diskKey = toSqlKey diskImageId :: DiskImageId
  drives <- runDb $ selectList [DriveVmId ==. vmKey, DriveDiskImageId ==. diskKey] []
  liftIO $ length drives `shouldSatisfy` (> 0)

driveCountForVm :: Int64 -> Int -> TestM ()
driveCountForVm vmId expectedCount = do
  let vmKey = toSqlKey vmId :: VmId
  cnt <- runDb $ count [DriveVmId ==. vmKey]
  liftIO $ cnt `shouldBe` expectedCount

snapshotExists :: Int64 -> TestM ()
snapshotExists snapshotId = do
  mSnapshot <- runDb $ get (toSqlKey snapshotId :: SnapshotId)
  liftIO $ mSnapshot `shouldSatisfy` isJust

snapshotNotExists :: Int64 -> TestM ()
snapshotNotExists snapshotId = do
  mSnapshot <- runDb $ get (toSqlKey snapshotId :: SnapshotId)
  liftIO $ mSnapshot `shouldSatisfy` isNothing

snapshotCountForDisk :: Int64 -> Int -> TestM ()
snapshotCountForDisk diskId expectedCount = do
  let diskKey = toSqlKey diskId :: DiskImageId
  cnt <- runDb $ count [SnapshotDiskImageId ==. diskKey]
  liftIO $ cnt `shouldBe` expectedCount

--------------------------------------------------------------------------------
-- Task Assertions
--------------------------------------------------------------------------------

taskCount :: Int -> TestM ()
taskCount expectedCount = do
  cnt <- runDb $ count ([] :: [Filter Task])
  liftIO $ cnt `shouldBe` expectedCount

taskExists :: Int64 -> TestM ()
taskExists taskId = do
  mTask <- runDb $ get (toSqlKey taskId :: TaskId)
  liftIO $ mTask `shouldSatisfy` isJust

taskNotExists :: Int64 -> TestM ()
taskNotExists taskId = do
  mTask <- runDb $ get (toSqlKey taskId :: TaskId)
  liftIO $ mTask `shouldSatisfy` isNothing

subtaskCount :: Int64 -> Int -> TestM ()
subtaskCount parentId expectedCount = do
  let parentKey = toSqlKey parentId :: TaskId
  cnt <- runDb $ count [TaskParent ==. Just parentKey]
  liftIO $ cnt `shouldBe` expectedCount

subtaskExists :: Int64 -> TestM ()
subtaskExists parentId = do
  let parentKey = toSqlKey parentId :: TaskId
  ts <- runDb $ selectList [TaskParent ==. Just parentKey] []
  liftIO $ length ts `shouldSatisfy` (> 0)

getLastTask :: TestM (Maybe (Entity Task))
getLastTask = do
  rs <- runDb $ selectList ([] :: [Filter Task]) [Desc TaskStartedAt, LimitTo 1]
  pure $ case rs of
    (t : _) -> Just t
    [] -> Nothing

--------------------------------------------------------------------------------
-- General Assertions
--------------------------------------------------------------------------------

shouldBeTrue :: Bool -> TestM ()
shouldBeTrue b = liftIO $ b `shouldBe` True

shouldBeFalse :: Bool -> TestM ()
shouldBeFalse b = liftIO $ b `shouldBe` False

shouldEqual :: (Eq a, Show a) => a -> a -> TestM ()
shouldEqual a b = liftIO $ a `shouldBe` b

-- Silence unused-import warnings while we still pin a couple of Util
-- imports for downstream test specs.
_t :: T.Text
_t = T.empty
