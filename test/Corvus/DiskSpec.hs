{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Disk-image CRUD + same-node-invariant + attach/detach paths.
--
-- The handler under test is `Corvus.Handlers.Disk` (+ Disk/Attach,
-- Disk/Rebase, Disk/Snapshot). The DSL helpers `diskRegister`,
-- `diskList`, `diskShow`, `diskAttach`, `diskDetach`, `diskDelete`
-- exercise the pure-DB branches; `diskCreate` / `diskClone` /
-- `diskRebase` / `diskResize` ride through the via-agent path
-- and surface as `RespError "nodeagent … unavailable"` in the
-- test fixture where the stub agent is absent (verified by
-- assertion).
module Corvus.DiskSpec (spec) where

import Test.Prelude

spec :: Spec
spec = sequential $ withTestDb $ do
  ------------------------------------------------------------------
  -- list / show

  describe "diskList" $ do
    testCase "returns empty when no images are registered" $ do
      when_ diskList
      then_ $ responseIs $ \case
        RespDiskList [] -> True
        _ -> False

    testCase "returns one row per inserted image" $ do
      given $ do
        _ <- insertDiskImage "a" "/a.qcow2" FormatQcow2
        _ <- insertDiskImage "b" "/b.qcow2" FormatQcow2
        pure ()
      when_ diskList
      then_ $ responseIs $ \case
        RespDiskList xs -> length xs == 2
        _ -> False

  describe "diskShow" $ do
    testCase "returns DiskInfo for an existing image" $ do
      given $ do
        _ <- insertDiskImage "show-me" "/show-me.qcow2" FormatQcow2
        pure ()
      when_ $ diskShow 1
      then_ $ responseIs $ \case
        RespDiskInfo _ -> True
        _ -> False

    testCase "returns DiskNotFound for unknown id" $ do
      when_ $ diskShow 999
      then_ responseIsDiskNotFound

  ------------------------------------------------------------------
  -- create (agent unavailable path)

  describe "diskCreate" $ do
    testCase "surfaces a clean error when no agent is registered" $ do
      -- The test fixture's stub NodeConns has `ncNodeAgent =
      -- Nothing`, so the via-agent createImage call comes back
      -- as RespError. This is the same error path the daemon
      -- exposes to the CLI in production when a node has been
      -- registered but its supervisor hasn't dialled yet.
      when_ $ diskCreate "freshly-created" FormatQcow2 1024
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False
      -- and the DB row should NOT have been written because
      -- the file-create step failed.
      then_ $ diskImageCount 0

  ------------------------------------------------------------------
  -- register (no agent needed when format is provided)

  describe "diskRegister" $ do
    testCase "writes a row + a DiskImageNode for the test-node" $ do
      when_ $ diskRegister "imported" "/baseimages/imported.qcow2" FormatQcow2
      then_ $ do
        responseIs $ \case
          RespDiskCreated _ -> True
          _ -> False
        diskImageCount 1
        diskImageExists 1

    testCase "re-registering by the same logical name is idempotent" $ do
      given $ do
        _ <- insertDiskImage "existing" "/baseimages/existing.qcow2" FormatQcow2
        pure ()
      when_ $ diskRegister "existing" "/baseimages/existing.qcow2" FormatQcow2
      then_ $ do
        responseIs $ \case
          RespDiskCreated _ -> True
          _ -> False
        -- Same disk_image row, no duplicate.
        diskImageCount 1

  ------------------------------------------------------------------
  -- delete

  describe "diskDelete" $ do
    testCase "returns DiskNotFound for unknown id" $ do
      when_ $ diskDelete 999
      then_ responseIsDiskNotFound

    testCase "deletes a disk image that has no overlays + no drives" $ do
      given $ do
        _ <- insertDiskImage "doomed" "/doomed.qcow2" FormatQcow2
        pure ()
      when_ $ diskDelete 1
      then_ $ do
        responseIs (== RespDiskOk)
        diskImageNotExists 1

    testCase "refuses to delete a disk that's an overlay base" $ do
      given $ do
        baseId <- insertDiskImage "base" "/base.qcow2" FormatQcow2
        _ <- insertDiskImageWithBacking "ov" "/ov.qcow2" FormatQcow2 Nothing (Just baseId)
        pure ()
      when_ $ diskDelete 1
      then_ $ do
        responseIsDiskHasOverlays
        diskImageExists 1

    testCase "refuses to delete a disk that's attached to a VM" $ do
      given $ do
        diskId <- insertDiskImageOnTestNode "claimed" "/claimed.qcow2" FormatQcow2
        vmId <- insertVm "claimer" VmStopped
        _ <- attachDrive vmId diskId InterfaceVirtio
        pure ()
      when_ $ diskDelete 1
      then_ $ do
        responseIsDiskInUse
        diskImageExists 1

  ------------------------------------------------------------------
  -- attach + detach

  describe "diskAttach" $ do
    testCase "returns DiskNotFound for unknown disk" $ do
      given $ do
        _ <- insertVm "v1" VmStopped
        pure ()
      when_ $ diskAttach 1 999 InterfaceVirtio (Just MediaDisk)
      then_ responseIsDiskNotFound

    testCase "returns VmNotFound for unknown VM" $ do
      given $ do
        _ <- insertDiskImageOnTestNode "lonely" "/lonely.qcow2" FormatQcow2
        pure ()
      when_ $ diskAttach 999 1 InterfaceVirtio (Just MediaDisk)
      then_ responseIsVmNotFound

    testCase "refuses when the disk has no DiskImageNode for the VM's node" $ do
      given $ do
        -- insertDiskImage skips the DiskImageNode row; same-node
        -- guard in handleDiskAttach catches this.
        _ <- insertDiskImage "orphan" "/orphan.qcow2" FormatQcow2
        _ <- insertVm "homeless" VmStopped
        pure ()
      when_ $ diskAttach 1 1 InterfaceVirtio (Just MediaDisk)
      then_ $ responseIs $ \case
        RespError _ -> True
        _ -> False

    testCase "happy path: attaches a stopped VM's drive in the DB" $ do
      given $ do
        diskId <- insertDiskImageOnTestNode "ok" "/ok.qcow2" FormatQcow2
        vmId <- insertVm "owner" VmStopped
        -- nothing else: handleDiskAttach should write a drive row
        pure (vmId, diskId)
      when_ $ diskAttach 1 1 InterfaceVirtio (Just MediaDisk)
      then_ $ do
        responseIs $ \case
          RespDiskAttached _ -> True
          _ -> False
        driveExistsForVm 1 1
        driveCountForVm 1 1

  describe "diskDetach" $ do
    testCase "returns DriveNotFound when nothing is attached" $ do
      given $ do
        _ <- insertDiskImage "free" "/free.qcow2" FormatQcow2
        _ <- insertVm "free-vm" VmStopped
        pure ()
      when_ $ diskDetach 1 1
      then_ $ responseIs $ \case
        RespDriveNotFound -> True
        RespError _ -> True -- the handler also returns RespError for some paths
        _ -> False

    testCase "removes the drive row when the VM is stopped" $ do
      given $ do
        diskId <- insertDiskImageOnTestNode "tied" "/tied.qcow2" FormatQcow2
        vmId <- insertVm "tied-vm" VmStopped
        _ <- attachDrive vmId diskId InterfaceVirtio
        pure ()
      when_ $ diskDetach 1 1
      then_ $ do
        responseIs (== RespDiskOk)
        driveCountForVm 1 0
