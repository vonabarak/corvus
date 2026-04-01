{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Corvus.DiskSpec (spec) where

import Corvus.Handlers.Disk (detectFormatFromPath, makeRelativeToBase, resolveDiskFilePathPure, resolveDiskPath, sanitizeDiskName)
import Corvus.Model (DiskImage (..), DriveFormat (..))
import Corvus.Protocol (DiskImageInfo (..), Ref (..), Request (..), Response (..))
import Corvus.Qemu.Config (QemuConfig (..), defaultQemuConfig)
import Corvus.Qemu.Image (detectFormatFromUrl, isHttpUrl)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Test.DSL.Core (getTempDir)
import Test.Prelude

spec :: Spec
spec = sequential $ do
  describe "disk path resolution" $ do
    it "resolves relative path against base" $ do
      now <- getCurrentTime
      let config = defaultQemuConfig {qcBasePath = Just "/base/path"}
          disk =
            DiskImage
              { diskImageName = "test"
              , diskImageFilePath = "images/test.qcow2"
              , diskImageFormat = FormatQcow2
              , diskImageSizeMb = Nothing
              , diskImageCreatedAt = now
              , diskImageBackingImageId = Nothing
              }
      resolved <- resolveDiskPath config disk
      resolved `shouldBe` "/base/path/images/test.qcow2"

    it "returns absolute path as-is" $ do
      now <- getCurrentTime
      let config = defaultQemuConfig {qcBasePath = Just "/base/path"}
          disk =
            DiskImage
              { diskImageName = "test"
              , diskImageFilePath = "/other/path/test.qcow2"
              , diskImageFormat = FormatQcow2
              , diskImageSizeMb = Nothing
              , diskImageCreatedAt = now
              , diskImageBackingImageId = Nothing
              }
      resolved <- resolveDiskPath config disk
      resolved `shouldBe` "/other/path/test.qcow2"

  describe "makeRelativeToBase" $ do
    it "strips base directory prefix" $ do
      makeRelativeToBase "/base/path" "/base/path/test.qcow2" `shouldBe` "test.qcow2"

    it "strips base directory prefix with subdirectory" $ do
      makeRelativeToBase "/base/path" "/base/path/sub/test.qcow2" `shouldBe` "sub/test.qcow2"

    it "preserves path outside base directory" $ do
      makeRelativeToBase "/base/path" "/other/path/test.qcow2" `shouldBe` "/other/path/test.qcow2"

    it "does not match partial directory names" $ do
      makeRelativeToBase "/base/path" "/base/pathmore/test.qcow2" `shouldBe` "/base/pathmore/test.qcow2"

  describe "resolveDiskFilePathPure" $ do
    it "uses basePath/fileName when no path given" $
      resolveDiskFilePathPure "/base" Nothing "disk.qcow2"
        `shouldBe` "/base/disk.qcow2"

    it "treats trailing slash as directory and appends fileName" $
      resolveDiskFilePathPure "/base" (Just "subdir/") "disk.qcow2"
        `shouldBe` "/base/subdir/disk.qcow2"

    it "treats path without trailing slash as full file path" $
      resolveDiskFilePathPure "/base" (Just "custom.raw") "disk.qcow2"
        `shouldBe` "/base/custom.raw"

    it "resolves relative directory against basePath" $
      resolveDiskFilePathPure "/base" (Just "vms/") "disk.qcow2"
        `shouldBe` "/base/vms/disk.qcow2"

    it "resolves relative file path against basePath" $
      resolveDiskFilePathPure "/base" (Just "vms/my-disk.raw") "disk.qcow2"
        `shouldBe` "/base/vms/my-disk.raw"

    it "uses absolute directory as-is" $
      resolveDiskFilePathPure "/base" (Just "/data/vms/") "disk.qcow2"
        `shouldBe` "/data/vms/disk.qcow2"

    it "uses absolute file path as-is" $
      resolveDiskFilePathPure "/base" (Just "/data/my-disk.raw") "disk.qcow2"
        `shouldBe` "/data/my-disk.raw"

    it "handles just a slash as absolute root directory" $
      resolveDiskFilePathPure "/base" (Just "/") "disk.qcow2"
        `shouldBe` "/disk.qcow2"

  describe "URL detection" $ do
    it "detects HTTP URL" $ do
      isHttpUrl "http://example.com/image.qcow2" `shouldBe` True

    it "detects HTTPS URL" $ do
      isHttpUrl "https://example.com/image.qcow2" `shouldBe` True

    it "rejects local path" $ do
      isHttpUrl "/local/path/image.qcow2" `shouldBe` False

    it "rejects relative path" $ do
      isHttpUrl "relative/path.qcow2" `shouldBe` False

  describe "format detection from URL" $ do
    it "detects qcow2 format" $ do
      detectFormatFromUrl "https://example.com/image.qcow2" `shouldBe` Just FormatQcow2

    it "detects raw format from .img extension" $ do
      detectFormatFromUrl "https://example.com/image.img" `shouldBe` Just FormatRaw

    it "strips .xz and detects inner format" $ do
      detectFormatFromUrl "https://example.com/image.qcow2.xz" `shouldBe` Just FormatQcow2

    it "returns Nothing for unknown extension" $ do
      detectFormatFromUrl "https://example.com/image.iso" `shouldBe` Nothing

  describe "sanitizeDiskName" $ do
    it "accepts normal name" $ do
      sanitizeDiskName "my-disk" `shouldBe` Right "my-disk"

    it "accepts name with dots" $ do
      sanitizeDiskName "disk.qcow2" `shouldBe` Right "disk.qcow2"

    it "strips path traversal" $ do
      sanitizeDiskName "../../../etc/passwd" `shouldBe` Right "etcpasswd"

    it "strips null bytes" $ do
      sanitizeDiskName "disk\0name" `shouldBe` Right "diskname"

    it "strips slashes" $ do
      sanitizeDiskName "a/b\\c" `shouldBe` Right "abc"

    it "rejects name that becomes empty after sanitization" $ do
      sanitizeDiskName ".." `shouldSatisfy` isLeft

    it "rejects all-digit names" $ do
      sanitizeDiskName "123" `shouldSatisfy` isLeft

    it "rejects empty name" $ do
      sanitizeDiskName "" `shouldSatisfy` isLeft

  describe "detectFormatFromPath" $ do
    it "detects qcow2" $ do
      detectFormatFromPath "test.qcow2" `shouldBe` Just FormatQcow2

    it "detects raw" $ do
      detectFormatFromPath "test.raw" `shouldBe` Just FormatRaw

    it "detects img as raw" $ do
      detectFormatFromPath "test.img" `shouldBe` Just FormatRaw

    it "detects vmdk" $ do
      detectFormatFromPath "test.vmdk" `shouldBe` Just FormatVmdk

    it "detects vdi" $ do
      detectFormatFromPath "test.vdi" `shouldBe` Just FormatVdi

    it "detects vpc" $ do
      detectFormatFromPath "test.vpc" `shouldBe` Just FormatVpc

    it "detects vhd as vpc" $ do
      detectFormatFromPath "test.vhd" `shouldBe` Just FormatVpc

    it "detects vhdx" $ do
      detectFormatFromPath "test.vhdx" `shouldBe` Just FormatVhdx

    it "returns Nothing for unknown extension" $ do
      detectFormatFromPath "test.iso" `shouldBe` Nothing

  withTestDb $ do
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

    describe "disk overlay" $ do
      testCase "fails for non-existent base disk" $ do
        when_ $ diskCreateOverlay "overlay1" 999 Nothing
        then_ responseIsDiskNotFound

      testCase "fails when base is attached read-write to a VM" $ do
        given $ do
          _ <- insertVm "test-vm" VmStopped
          _ <- insertDiskImage "base-disk" "base.qcow2" FormatQcow2
          _ <- attachDrive 1 1 InterfaceVirtio
          pure ()
        when_ $ diskCreateOverlay "overlay1" 1 Nothing
        then_ $ responseIs $ \case
          RespError msg -> "read-write" `T.isInfixOf` msg
          _ -> False

      testCase "fails when disk has overlays (cannot delete base)" $ do
        given $ do
          _ <- insertDiskImage "base-disk" "base.qcow2" FormatQcow2
          _ <- insertDiskImageWithBacking "overlay-disk" "overlay.qcow2" FormatQcow2 Nothing (Just 1)
          pure ()
        when_ $ diskDelete 1
        then_ responseIsDiskHasOverlays

    describe "disk show backing info" $ do
      testCase "shows backing image for overlay disk" $ do
        given $ do
          _ <- insertDiskImage "base-disk" "base.qcow2" FormatQcow2
          _ <- insertDiskImageWithBacking "overlay-disk" "overlay.qcow2" FormatQcow2 Nothing (Just 1)
          pure ()
        when_ $ diskShow 2
        then_ $ responseIs $ \case
          RespDiskInfo info ->
            diiBackingImageId info == Just 1
              && diiBackingImageName info == Just "base-disk"
          _ -> False

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

      testCase "fails when VM does not exist" $ do
        when_ $ diskDetach 999 1
        then_ $ responseIs $ \case
          RespVmNotFound -> True
          _ -> False

      testCase "fails when disk does not exist" $ do
        given $ do
          _ <- insertVm "test-vm" VmStopped
          pure ()
        when_ $ diskDetach 1 999
        then_ $ responseIs $ \case
          RespDiskNotFound -> True
          _ -> False

    describe "overlay extra protections" $ do
      testCase "prevent attaching base image read-write when overlays exist" $ do
        given $ do
          _ <- insertVm "vm1" VmStopped
          _ <- insertDiskImage "base-disk" "base.qcow2" FormatQcow2
          _ <- insertDiskImageWithBacking "overlay-disk" "overlay.qcow2" FormatQcow2 Nothing (Just 1)
          pure ()
        -- Try to attach base-disk (ID 1) to vm1 (ID 1) read-write
        when_ $ diskAttach 1 1 InterfaceVirtio (Just MediaDisk)
        then_ responseIsDiskHasOverlays

      testCase "allow attaching base image read-only when overlays exist" $ do
        given $ do
          _ <- insertVm "vm1" VmStopped
          _ <- insertDiskImage "base-disk" "base.qcow2" FormatQcow2
          _ <- insertDiskImageWithBacking "overlay-disk" "overlay.qcow2" FormatQcow2 Nothing (Just 1)
          pure ()
        -- Try to attach base-disk (ID 1) to vm1 (ID 1) read-only
        when_ $ diskAttachReadOnly 1 1 InterfaceVirtio (Just MediaDisk)
        then_ responseIsSuccess
        then_ $ driveExistsForVm 1 1

      testCase "prevent resizing base image when overlays exist" $ do
        given $ do
          _ <- insertDiskImage "base-disk" "base.qcow2" FormatQcow2
          _ <- insertDiskImageWithBacking "overlay-disk" "overlay.qcow2" FormatQcow2 Nothing (Just 1)
          pure ()
        -- Try to resize base-disk (ID 1)
        when_ $ diskResize 1 20480
        then_ responseIsDiskHasOverlays

    describe "disk resize" $ do
      testCase "fails for disk attached to running VM" $ do
        given $ do
          vmId <- insertVm "running-vm" VmRunning
          diskId <- insertDiskImage "run-disk" "run.qcow2" FormatQcow2
          _ <- attachDrive vmId diskId InterfaceVirtio
          pure ()
        when_ $ diskResize 1 20480
        then_ $ responseIs $ \case
          RespVmMustBeStopped -> True
          _ -> False

    describe "disk attach extra" $ do
      testCase "fails for duplicate disk attachment to same VM" $ do
        given $ do
          vmId <- insertVm "test-vm" VmStopped
          diskId <- insertDiskImage "test-disk" "test.qcow2" FormatQcow2
          _ <- attachDrive vmId diskId InterfaceVirtio
          pure ()
        when_ $ diskAttach 1 1 InterfaceVirtio (Just MediaDisk)
        then_ $ responseIs $ \case
          RespError msg -> "already attached" `T.isInfixOf` msg
          _ -> False

    describe "disk clone" $ do
      testCase "fails for non-existent base disk" $ do
        when_ $ diskClone "clone1" 999 Nothing
        then_ responseIsDiskNotFound

    describe "disk refresh" $ do
      testCase "fails for non-existent disk" $ do
        when_ $ executeRequest (ReqDiskRefresh (Ref "999"))
        then_ $ responseIs $ \case
          RespDiskNotFound -> True
          _ -> False

    describe "disk register stores relative path" $ do
      testCase "strips base directory prefix from absolute path" $ do
        basePath <- getTempDir
        let absPath = T.pack $ basePath <> "/foo.qcow2"
        when_ $ diskRegister "test-disk" absPath FormatQcow2
        then_ $ do
          responseIsDiskCreated
          diskImageHasPath 1 "foo.qcow2"

      testCase "strips base directory prefix with subdirectory" $ do
        basePath <- getTempDir
        let absPath = T.pack $ basePath <> "/sub/foo.qcow2"
        when_ $ diskRegister "test-disk" absPath FormatQcow2
        then_ $ do
          responseIsDiskCreated
          diskImageHasPath 1 "sub/foo.qcow2"

      testCase "preserves path outside base directory" $ do
        when_ $ diskRegister "test-disk" "/other/dir/foo.qcow2" FormatQcow2
        then_ $ do
          responseIsDiskCreated
          diskImageHasPath 1 "/other/dir/foo.qcow2"

      testCase "preserves already-relative path" $ do
        when_ $ diskRegister "test-disk" "foo.qcow2" FormatQcow2
        then_ $ do
          responseIsDiskCreated
          diskImageHasPath 1 "foo.qcow2"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
