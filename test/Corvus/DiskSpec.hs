{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Corvus.DiskSpec (spec) where

import Test.Prelude

spec :: Spec
spec = withTestDb $ do
  describe "disk list" $ do
    testCase "returns empty list when no disks exist" $ do
      when_ diskList
      then_ $ responseIs $ \case
        RespDiskList [] -> True
        _ -> False

    testCase "returns all disk images" $ do
      given $ do
        _ <- insertDiskImage "disk1" "path1.qcow2" FormatQcow2
        _ <- insertDiskImage "disk2" "path2.qcow2" FormatQcow2
        _ <- insertDiskImage "disk3" "path3.raw" FormatRaw
        pure ()
      when_ diskList
      then_ $ responseIs $ \case
        RespDiskList disks -> length disks == 3
        _ -> False

  describe "disk show" $ do
    testCase "returns disk details for existing disk" $ do
      given $ do
        _ <- insertDiskImage "test-disk" "test.qcow2" FormatQcow2
        pure ()
      when_ $ diskShow 1
      then_ $ responseIs $ \case
        RespDiskInfo _ -> True
        _ -> False

    testCase "returns not found for non-existent disk" $ do
      when_ $ diskShow 999
      then_ responseIsDiskNotFound

  describe "disk delete" $ do
    testCase "fails for non-existent disk" $ do
      when_ $ diskDelete 999
      then_ responseIsDiskNotFound

    testCase "fails when disk is attached to a VM" $ do
      given $ do
        vmId <- insertVm "test-vm" VmStopped
        diskId <- insertDiskImage "attached-disk" "test.qcow2" FormatQcow2
        _ <- attachDrive vmId diskId InterfaceVirtio
        pure ()
      when_ $ diskDelete 1
      then_ responseIsDiskInUse

  describe "disk resize" $ do
    testCase "fails for non-existent disk" $ do
      when_ $ diskResize 999 20480
      then_ responseIsDiskNotFound

  describe "disk attach" $ do
    testCase "attaches disk to stopped VM" $ do
      given $ do
        vmId <- insertVm "test-vm" VmStopped
        diskId <- insertDiskImage "test-disk" "test.qcow2" FormatQcow2
        pure (vmId, diskId)
      when_ $ diskAttach 1 1 InterfaceVirtio (Just MediaDisk)
      then_ $ do
        responseIs $ \case
          RespDiskAttached _ -> True
          _ -> False
        driveExistsForVm 1 1

    testCase "fails for non-existent VM" $ do
      given $ do
        _ <- insertDiskImage "test-disk" "test.qcow2" FormatQcow2
        pure ()
      when_ $ diskAttach 999 1 InterfaceVirtio (Just MediaDisk)
      then_ $ responseIs $ \case
        RespVmNotFound -> True
        _ -> False

    testCase "fails for non-existent disk" $ do
      given $ do
        _ <- insertVm "test-vm" VmStopped
        pure ()
      when_ $ diskAttach 1 999 InterfaceVirtio (Just MediaDisk)
      then_ responseIsDiskNotFound

  describe "disk detach" $ do
    testCase "detaches drive from stopped VM" $ do
      given $ do
        vmId <- insertVm "test-vm" VmStopped
        diskId <- insertDiskImage "test-disk" "test.qcow2" FormatQcow2
        attachDrive vmId diskId InterfaceVirtio
      when_ $ diskDetach 1 1
      then_ $ do
        responseIs $ \case
          RespDiskOk -> True
          _ -> False
        driveNotExists 1

    testCase "fails when drive does not exist (no VM or drive)" $ do
      when_ $ diskDetach 999 1
      then_ $ responseIs $ \case
        RespDriveNotFound -> True
        _ -> False

    testCase "fails when drive does not exist (VM exists)" $ do
      given $ do
        _ <- insertVm "test-vm" VmStopped
        pure ()
      when_ $ diskDetach 1 999
      then_ $ responseIs $ \case
        RespDriveNotFound -> True
        _ -> False
