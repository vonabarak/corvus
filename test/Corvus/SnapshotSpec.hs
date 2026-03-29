{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Corvus.SnapshotSpec (spec) where

import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "snapshot list" $ do
    testCase "returns empty list for disk with no snapshots" $ do
      given $ do
        _ <- insertDiskImage "test-disk" "test.qcow2" FormatQcow2
        pure ()
      when_ $ snapshotList 1
      then_ $ responseIs $ \case
        RespSnapshotList [] -> True
        _ -> False

    testCase "returns all snapshots for a disk" $ do
      given $ do
        diskId <- insertDiskImage "test-disk" "test.qcow2" FormatQcow2
        _ <- insertSnapshot diskId "snap1"
        _ <- insertSnapshot diskId "snap2"
        pure ()
      when_ $ snapshotList 1
      then_ $ responseIs $ \case
        RespSnapshotList snaps -> length snaps == 2
        _ -> False

    testCase "fails for non-existent disk" $ do
      when_ $ snapshotList 999
      then_ responseIsDiskNotFound

  describe "snapshot delete" $ do
    testCase "fails for non-existent disk" $ do
      when_ $ snapshotDelete 999 1
      then_ responseIsDiskNotFound

    testCase "fails for non-existent snapshot" $ do
      given $ do
        _ <- insertDiskImage "test-disk" "test.qcow2" FormatQcow2
        pure ()
      when_ $ snapshotDelete 1 999
      then_ responseIsSnapshotNotFound

    testCase "fails for raw format disk" $ do
      given $ do
        _ <- insertDiskImage "raw-disk" "test.raw" FormatRaw
        pure ()
      when_ $ snapshotDelete 1 1
      then_ $ responseIs $ \case
        RespFormatNotSupported _ -> True
        _ -> False

  describe "snapshot rollback" $ do
    testCase "fails for non-existent disk" $ do
      when_ $ snapshotRollback 999 1
      then_ responseIsDiskNotFound

    testCase "fails for non-existent snapshot" $ do
      given $ do
        _ <- insertDiskImage "test-disk" "test.qcow2" FormatQcow2
        pure ()
      when_ $ snapshotRollback 1 999
      then_ responseIsSnapshotNotFound

    testCase "fails for raw format disk" $ do
      given $ do
        _ <- insertDiskImage "raw-disk" "test.raw" FormatRaw
        pure ()
      when_ $ snapshotRollback 1 1
      then_ $ responseIs $ \case
        RespFormatNotSupported _ -> True
        _ -> False

  describe "snapshot merge" $ do
    testCase "fails for non-existent disk" $ do
      when_ $ snapshotMerge 999 1
      then_ responseIsDiskNotFound

    testCase "fails for non-existent snapshot" $ do
      given $ do
        _ <- insertDiskImage "test-disk" "test.qcow2" FormatQcow2
        pure ()
      when_ $ snapshotMerge 1 999
      then_ responseIsSnapshotNotFound

    testCase "fails for raw format disk" $ do
      given $ do
        _ <- insertDiskImage "raw-disk" "test.raw" FormatRaw
        pure ()
      when_ $ snapshotMerge 1 1
      then_ $ responseIs $ \case
        RespFormatNotSupported _ -> True
        _ -> False
