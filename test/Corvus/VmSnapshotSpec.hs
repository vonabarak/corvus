{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unit coverage for the VM-scoped snapshot Actions in
-- "Corvus.Handlers.Vm.Snapshot". Exercises the parts that don't
-- need a live agent / QEMU:
--
-- * Listing: aggregates sibling rows into one @VmSnapshotInfo@ per
--   @(vm, name)@; only rows that have a carrier (@hasVmstate=True@)
--   surface.
-- * Validation: not-running rejection on create, missing-VM /
--   missing-snapshot on rollback + delete.
-- * Uniqueness: create rejects a name that already exists on any
--   of the VM's drives, before the agent is contacted.
module Corvus.VmSnapshotSpec (spec) where

import Corvus.Protocol (NamedRef (..), VmSnapshotInfo (..))
import qualified Data.Text as T
import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  describe "vm snapshot list" $ do
    testCase "returns empty list for a VM with no snapshots" $ do
      given $ do
        _ <- insertVm "vm-empty" VmStopped
        pure ()
      when_ $ vmSnapshotList 1
      then_ $ responseIs $ \case
        RespVmSnapshotList [] -> True
        _ -> False

    testCase "groups sibling rows into one VmSnapshotInfo per name" $ do
      given $ do
        vmId <- insertVm "vm-multi" VmRunning
        d1 <- insertDiskImage "boot" "boot.qcow2" FormatQcow2
        d2 <- insertDiskImage "data" "data.qcow2" FormatQcow2
        _ <- attachDrive vmId d1 InterfaceVirtio
        _ <- attachDrive vmId d2 InterfaceVirtio
        -- One vmstate snapshot named "cp1" across both disks;
        -- boot is the carrier.
        _ <- insertSnapshotWithVmstate d1 "cp1" True
        _ <- insertSnapshotWithVmstate d2 "cp1" False
        pure ()
      when_ $ vmSnapshotList 1
      then_ $ responseIs $ \case
        RespVmSnapshotList [info] ->
          vsiName info == "cp1"
            && vsiDiskCount info == 2
            && nrName (vsiCarrierDisk info) == "boot"
        _ -> False

    testCase "ignores snapshot names that have no vmstate carrier" $ do
      given $ do
        vmId <- insertVm "vm-disk-only" VmRunning
        d <- insertDiskImage "boot" "boot.qcow2" FormatQcow2
        _ <- attachDrive vmId d InterfaceVirtio
        -- Disk-only snapshot — not surfaced by the VM-scoped list.
        _ <- insertSnapshot d "disk-only"
        pure ()
      when_ $ vmSnapshotList 1
      then_ $ responseIs $ \case
        RespVmSnapshotList [] -> True
        _ -> False

    testCase "fails for a non-existent VM" $ do
      when_ $ vmSnapshotList 999
      then_ responseIsVmNotFound

  describe "vm snapshot create validation" $ do
    testCase "rejects a stopped VM with a clear error message" $ do
      given $ do
        vmId <- insertVm "vm-stopped" VmStopped
        d <- insertDiskImage "boot" "boot.qcow2" FormatQcow2
        _ <- attachDrive vmId d InterfaceVirtio
        pure ()
      when_ $ vmSnapshotCreate 1 "cp1"
      then_ $ responseIs $ \case
        RespError msg ->
          T.isInfixOf "not running" msg
            && T.isInfixOf "live QEMU" msg
        _ -> False

    testCase "rejects a duplicate snapshot name before any agent call" $ do
      given $ do
        vmId <- insertVm "vm-dupe" VmRunning
        d1 <- insertDiskImage "boot" "boot.qcow2" FormatQcow2
        d2 <- insertDiskImage "data" "data.qcow2" FormatQcow2
        _ <- attachDrive vmId d1 InterfaceVirtio
        _ <- attachDrive vmId d2 InterfaceVirtio
        -- Existing snapshot under "cp1" on disk d2 — should block
        -- a new VM-scoped "cp1" before the daemon spawns a task.
        _ <- insertSnapshot d2 "cp1"
        pure ()
      when_ $ vmSnapshotCreate 1 "cp1"
      then_ $ responseIs $ \case
        RespError msg -> T.isInfixOf "already exists" msg
        _ -> False

    testCase "fails for a non-existent VM" $ do
      when_ $ vmSnapshotCreate 999 "cp1"
      then_ responseIsVmNotFound

    testCase "rejects when VM has no writable qcow2 drives" $ do
      given $ do
        _ <- insertVm "vm-no-drives" VmRunning
        pure ()
      when_ $ vmSnapshotCreate 1 "cp1"
      then_ $ responseIs $ \case
        RespError msg -> T.isInfixOf "no writable qcow2" msg
        _ -> False

  describe "vm snapshot rollback validation" $ do
    testCase "fails for a non-existent VM" $ do
      when_ $ vmSnapshotRollback 999 "cp1"
      then_ responseIsVmNotFound

    testCase "fails for a snapshot that doesn't exist for this VM" $ do
      given $ do
        vmId <- insertVm "vm-no-snap" VmRunning
        d <- insertDiskImage "boot" "boot.qcow2" FormatQcow2
        _ <- attachDrive vmId d InterfaceVirtio
        pure ()
      when_ $ vmSnapshotRollback 1 "missing"
      then_ responseIsSnapshotNotFound

  describe "vm snapshot delete validation" $ do
    testCase "rejects a non-running VM (vmstate delete needs live QMP)" $ do
      given $ do
        vmId <- insertVm "vm-stopped-del" VmStopped
        d <- insertDiskImage "boot" "boot.qcow2" FormatQcow2
        _ <- attachDrive vmId d InterfaceVirtio
        _ <- insertSnapshotWithVmstate d "cp1" True
        pure ()
      when_ $ vmSnapshotDelete 1 "cp1"
      then_ $ responseIs $ \case
        RespError msg -> T.isInfixOf "not running" msg
        _ -> False

    testCase "fails for a snapshot that doesn't exist for this VM" $ do
      given $ do
        vmId <- insertVm "vm-no-such-del" VmRunning
        d <- insertDiskImage "boot" "boot.qcow2" FormatQcow2
        _ <- attachDrive vmId d InterfaceVirtio
        pure ()
      when_ $ vmSnapshotDelete 1 "missing"
      then_ responseIsSnapshotNotFound

    testCase "fails for a non-existent VM" $ do
      when_ $ vmSnapshotDelete 999 "cp1"
      then_ responseIsVmNotFound
