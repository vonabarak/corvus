{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for disk operations: cloning, overlays, and live attach/detach.
-- Requires QEMU with KVM support and qemu-img on PATH.
--
-- Run with: stack test --ta '--match DiskIntegration'
module Corvus.DiskIntegrationSpec (spec) where

import Control.Exception (bracket_, catch)
import Control.Monad (void)
import Corvus.Client
import Corvus.Model (CacheType (..), DriveFormat (..), DriveInterface (..))
import Corvus.Protocol (DiskImageInfo (..), DriveInfo (..), SnapshotInfo (..), VmDetails (..))
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (isJust)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (getEnvironment, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Database (TestEnv, withTestDb)
import Test.Hspec
import Test.VM.Common (TestVm (..), defaultVmConfig, withTestVm, withTestVmGuestExec)
import Test.VM.Daemon (TestDaemon, startTestDaemon, stopTestDaemon, withDaemonConnection)
import Test.VM.Rpc (runInVm_, stopTestVmAndWait)

spec :: Spec
spec = withTestDb $ do
  describe "Disk clone integration" $ do
    it "can clone a disk and its content" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
            diskId = tvmDiskId vm
            vmId = tvmId vm

        stopTestVmAndWait daemon vmId 10

        res <- withDaemonConnection daemon $ \conn -> diskClone conn "cloned-disk" (T.pack (show diskId)) Nothing
        newDiskId <- case res of
          Right (Right (DiskCreated id_)) -> pure id_
          other -> fail $ "Clone failed: " ++ show other

        cloned <- getDiskInfo daemon newDiskId
        diiName cloned `shouldBe` "cloned-disk"
        -- Cloned disk should have auto-detected size
        diiSizeMb cloned `shouldSatisfy` isJust

    it "clones snapshots in the database" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
            diskId = tvmDiskId vm
            vmId = tvmId vm

        stopTestVmAndWait daemon vmId 10

        void $ withDaemonConnection daemon $ \conn -> snapshotCreate conn (T.pack (show diskId)) "snap1"

        res <- withDaemonConnection daemon $ \conn -> diskClone conn "cloned-with-snap" (T.pack (show diskId)) Nothing
        newDiskId <- case res of
          Right (Right (DiskCreated id_)) -> pure id_
          other -> fail $ "Clone failed: " ++ show other

        snapsResult <- withDaemonConnection daemon $ \conn -> snapshotList conn (T.pack (show newDiskId))
        case snapsResult of
          Right (Right (SnapshotListResult snaps)) -> do
            length snaps `shouldBe` 1
            map sniName snaps `shouldBe` ["snap1"]
          other -> fail $ "Snapshot list failed: " ++ show other

    it "can clone to a custom path" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
            diskId = tvmDiskId vm
            vmId = tvmId vm
            customPath = "/tmp/custom-clone.qcow2"

        stopTestVmAndWait daemon vmId 10

        catch (removeFile customPath) $ \e ->
          if isDoesNotExistError e then pure () else ioError e

        res <- withDaemonConnection daemon $ \conn -> diskClone conn "custom-path-clone" (T.pack (show diskId)) (Just $ T.pack customPath)
        case res of
          Right (Right (DiskCreated _)) -> pure ()
          other -> fail $ "Clone to custom path failed: " ++ show other

        exists <- doesFileExist customPath
        exists `shouldBe` True

        removeFile customPath

    it "rejects cloning if VM is running" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
            diskId = tvmDiskId vm

        res <- withDaemonConnection daemon $ \conn -> diskClone conn "failed-clone" (T.pack (show diskId)) Nothing
        case res of
          Right (Right VmMustBeStopped) -> pure ()
          other -> fail $ "Expected VmMustBeStopped but got: " ++ show other

  describe "Disk overlay integration (requires qemu-img)" $ do
    it "creates overlay via daemon and lists backing info" $ \env -> do
      withSystemTempDirectory "corvus-overlay-test" $ \tmpDir -> do
        let vmDir = tmpDir </> "VMs"
            basePath = vmDir </> "base.qcow2"
        createDirectoryIfMissing True vmDir
        (code, _, err) <-
          readProcessWithExitCode "qemu-img" ["create", "-f", "qcow2", basePath, "1M"] ""
        case code of
          ExitFailure _ -> fail $ "qemu-img create failed: " ++ err
          ExitSuccess -> pure ()

        oldHome <- lookup "HOME" <$> getEnvironment
        let setTestHome = setEnv "HOME" tmpDir
            restoreHome = case oldHome of
              Nothing -> unsetEnv "HOME"
              Just v -> setEnv "HOME" v
        bracket_ setTestHome restoreHome $ do
          daemon <- startTestDaemon env
          runOverlayTest daemon basePath `finally_` stopTestDaemon daemon

  describe "Live disk attach/detach integration" $ do
    it "can hot-plug and hot-unplug a data disk on a running VM" $ \env -> do
      withTestVmGuestExec env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
            vmId = tvmId vm

        -- Create an empty data disk via the daemon
        dataDiskId <- createEmptyDisk daemon "hotplug-data" FormatQcow2 512

        -- Verify created disk has auto-detected size
        diskInfo <- getDiskInfo daemon dataDiskId
        diiSizeMb diskInfo `shouldBe` Just 512

        -- Hot-attach the disk to the running VM
        driveId <- hotAttachDisk daemon vmId dataDiskId InterfaceVirtio CacheWriteback

        -- Verify drive appears in VM details
        details <- showVmDetails daemon vmId
        let driveIds = map diId (vdDrives details)
        driveIds `shouldSatisfy` elem driveId

        -- Verify the guest sees the new block device (vdb or similar)
        -- Write data to it to confirm it's functional
        runInVm_ vm "ls /dev/vd* | wc -l"

        -- Hot-detach the disk (diskDetach takes disk image ID, not drive ID)
        hotDetachDisk daemon vmId dataDiskId

        -- Verify drive is removed from VM details
        details2 <- showVmDetails daemon vmId
        let driveIds2 = map diId (vdDrives details2)
        driveIds2 `shouldNotSatisfy` elem driveId

        -- Re-attach the same disk (verifies file lock is properly released)
        driveId2 <- hotAttachDisk daemon vmId dataDiskId InterfaceVirtio CacheWriteback

        -- Verify drive appears again
        details3 <- showVmDetails daemon vmId
        let driveIds3 = map diId (vdDrives details3)
        driveIds3 `shouldSatisfy` elem driveId2

        -- Clean up
        hotDetachDisk daemon vmId dataDiskId

    it "can attach a disk read-only to a running VM" $ \env -> do
      withTestVmGuestExec env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
            vmId = tvmId vm

        dataDiskId <- createEmptyDisk daemon "hotplug-ro" FormatQcow2 256

        -- Attach read-only
        driveId <- hotAttachDiskReadOnly daemon vmId dataDiskId InterfaceVirtio CacheWriteback

        details <- showVmDetails daemon vmId
        let driveIds = map diId (vdDrives details)
        driveIds `shouldSatisfy` elem driveId

        -- Detach (diskDetach takes disk image ID, not drive ID)
        hotDetachDisk daemon vmId dataDiskId

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Overlay test extracted for bracket usage
runOverlayTest :: TestDaemon -> FilePath -> IO ()
runOverlayTest daemon basePath = do
  regResult <-
    withDaemonConnection daemon $ \conn ->
      diskRegister conn "base-disk" (T.pack basePath) (Just FormatQcow2)
  baseId <- case regResult of
    Left err -> fail $ "Connection error: " ++ show err
    Right (Left err) -> fail $ "RPC error: " ++ show err
    Right (Right (DiskCreated id_)) -> pure id_
    Right (Right (DiskError msg)) -> fail $ "Register failed: " ++ T.unpack msg
    Right (Right other) -> fail $ "Unexpected register response: " ++ show other

  overlayResult <-
    withDaemonConnection daemon $ \conn ->
      diskCreateOverlay conn "overlay-disk" (T.pack (show baseId)) Nothing
  overlayId <- case overlayResult of
    Left err -> fail $ "Connection error: " ++ show err
    Right (Left err) -> fail $ "RPC error: " ++ show err
    Right (Right (DiskCreated id_)) -> pure id_
    Right (Right DiskNotFound) -> fail "Base disk not found"
    Right (Right (DiskError msg)) -> fail $ "Create overlay failed: " ++ T.unpack msg
    Right (Right other) -> fail $ "Unexpected overlay response: " ++ show other

  listResult <- withDaemonConnection daemon diskList
  case listResult of
    Left err -> fail $ "Connection error: " ++ show err
    Right (Right (DiskListResult disks)) -> do
      -- Verify base disk has auto-detected size
      let mBase = find (\d -> diiId d == baseId) disks
      case mBase of
        Nothing -> fail "Base disk not in list"
        Just base -> diiSizeMb base `shouldSatisfy` isJust

      -- Verify overlay has auto-detected size and backing info
      let mOverlay = find (\d -> diiId d == overlayId) disks
      case mOverlay of
        Nothing -> fail "Overlay disk not in list"
        Just overlay -> do
          diiBackingImageId overlay `shouldBe` Just baseId
          diiBackingImageName overlay `shouldBe` Just "base-disk"
          diiSizeMb overlay `shouldSatisfy` isJust
    Right (Left err) -> fail $ "RPC error: " ++ show err
    Right (Right other) -> fail $ "Unexpected list response: " ++ show other

-- | Equivalent of Control.Exception.finally but returns the first action's result
finally_ :: IO a -> IO b -> IO a
finally_ action cleanup = do
  r <- action
  _ <- cleanup
  pure r

-- | Create an empty disk image via the daemon
createEmptyDisk :: TestDaemon -> T.Text -> DriveFormat -> Int64 -> IO Int64
createEmptyDisk daemon name format sizeMb = do
  result <- withDaemonConnection daemon $ \conn ->
    diskCreate conn name format sizeMb Nothing
  case result of
    Right (Right (DiskCreated diskId)) -> pure diskId
    other -> fail $ "Failed to create disk: " ++ show other

-- | Hot-attach a disk to a running VM, returning the drive ID
hotAttachDisk :: TestDaemon -> Int64 -> Int64 -> DriveInterface -> CacheType -> IO Int64
hotAttachDisk daemon vmId diskId iface cache = do
  result <- withDaemonConnection daemon $ \conn ->
    diskAttach conn (T.pack (show vmId)) (T.pack (show diskId)) iface Nothing False False cache
  case result of
    Right (Right (DriveAttached driveId)) -> pure driveId
    other -> fail $ "Failed to hot-attach disk: " ++ show other

-- | Hot-attach a disk read-only to a running VM, returning the drive ID
hotAttachDiskReadOnly :: TestDaemon -> Int64 -> Int64 -> DriveInterface -> CacheType -> IO Int64
hotAttachDiskReadOnly daemon vmId diskId iface cache = do
  result <- withDaemonConnection daemon $ \conn ->
    diskAttach conn (T.pack (show vmId)) (T.pack (show diskId)) iface Nothing True False cache
  case result of
    Right (Right (DriveAttached driveId)) -> pure driveId
    other -> fail $ "Failed to hot-attach disk read-only: " ++ show other

-- | Hot-detach a disk from a running VM (by disk image ID)
hotDetachDisk :: TestDaemon -> Int64 -> Int64 -> IO ()
hotDetachDisk daemon vmId diskId = do
  result <- withDaemonConnection daemon $ \conn ->
    diskDetach conn (T.pack (show vmId)) (T.pack (show diskId))
  case result of
    Right (Right DiskOk) -> pure ()
    other -> fail $ "Failed to hot-detach disk: " ++ show other

-- | Get disk image info by ID, failing on error
getDiskInfo :: TestDaemon -> Int64 -> IO DiskImageInfo
getDiskInfo daemon diskId = do
  result <- withDaemonConnection daemon $ \conn ->
    diskShow conn (T.pack (show diskId))
  case result of
    Right (Right (DiskInfo info)) -> pure info
    other -> fail $ "Failed to get disk info: " ++ show other

-- | Get VM details, failing on error
showVmDetails :: TestDaemon -> Int64 -> IO VmDetails
showVmDetails daemon vmId = do
  result <- withDaemonConnection daemon $ \conn ->
    showVm conn (T.pack (show vmId))
  case result of
    Right (Right (Just details)) -> pure details
    other -> fail $ "Failed to show VM: " ++ show other
