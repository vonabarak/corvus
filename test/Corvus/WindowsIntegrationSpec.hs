{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for Windows VM guest agent support.
-- Verifies that Corvus can communicate with the QEMU Guest Agent on Windows VMs:
-- healthcheck pings, OS detection, and command execution via cmd.exe.
--
-- Requirements:
--   - Pre-built Windows test image at .test-images/windows-server-eval.qcow2
--     (build with: make test-image-windows)
--   - UEFI firmware (OVMF)
--   - QEMU with KVM support
--   - PostgreSQL for test database
--
-- Run with: stack test --test-arguments="--match WindowsIntegration"
module Corvus.WindowsIntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Corvus.Client (vmShow)
import Corvus.Client.Rpc (DiskResult (..), diskClone, diskCreateOverlay, diskRegister)
import Corvus.Model (CacheType (..), DriveFormat (..), DriveInterface (..), NetInterfaceType (..), VmStatus (..))
import Corvus.Protocol (VmDetails (..))
import Corvus.Qemu.GuestAgent (GuestOsInfo (..), guestGetOsInfo)
import Corvus.Types (ServerState (..))
import Data.Int (Int64)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time (UTCTime)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Database (TestEnv, withTestDb)
import Test.Hspec
import Test.VM.Common (findFreePort)
import Test.VM.Daemon (TestDaemon (..), withDaemonConnection, withTestDaemon)
import Test.VM.Rpc
  ( addVmDisk
  , addVmNetIf
  , createTestVmWithOptions
  , deleteTestVm
  , runViaGuestAgent
  , setCloudInitConfig
  , startTestVm
  , stopTestVmAndWait
  )

-- | Path to the pre-built Windows test image (relative to project root)
windowsImagePath :: FilePath
windowsImagePath = ".test-images" </> "windows-server-eval.qcow2"

-- | Run a test that requires the Windows test image.
-- Skips with pendingWith if the image does not exist.
withWindowsImage :: (FilePath -> IO ()) -> IO ()
withWindowsImage action = do
  projectRoot <- getCurrentDirectory
  let imagePath = projectRoot </> windowsImagePath
  exists <- doesFileExist imagePath
  if not exists
    then
      pendingWith $
        "Windows test image not found at "
          ++ imagePath
          ++ ". Build with: make test-image-windows"
    else action imagePath

spec :: Spec
spec = withTestDb $ do
  describe "Windows VM Integration" $ do
    it "guest agent healthcheck and exec work on Windows VM" $ \env -> do
      withWindowsImage $ \imagePath ->
        withTestDaemon env $ \daemon -> do
          -- Import the pre-built Windows image and create an overlay
          baseId <- registerDisk daemon "win-base" (T.pack imagePath) (Just FormatQcow2)
          diskId <- createOverlay daemon "win-test" baseId

          -- Set up UEFI firmware (OVMF_CODE read-only + OVMF_VARS clone)
          ovmfCodeId <- registerDisk daemon "win-ovmf-code" "/usr/share/edk2/OvmfX64/OVMF_CODE.fd" (Just FormatRaw)
          ovmfVarsTemplateId <- registerDisk daemon "win-ovmf-vars-tpl" "/usr/share/edk2/OvmfX64/OVMF_VARS.fd" (Just FormatRaw)
          ovmfVarsId <- cloneDisk daemon "win-ovmf-vars" ovmfVarsTemplateId

          -- Create VM: headless=False (SPICE graphics), guest-agent=True,
          -- cloud-init=False (Windows doesn't use NoCloud cloud-init)
          vmId <- createTestVmWithOptions daemon "test-win-vm" 2 4096 Nothing False True False

          -- Attach UEFI firmware first (pflash order matters: CODE then VARS)
          addVmDisk daemon vmId ovmfCodeId InterfacePflash CacheWriteback False True
          addVmDisk daemon vmId ovmfVarsId InterfacePflash CacheWriteback False False

          -- Add boot disk (virtio — the Windows image has virtio drivers)
          addVmDisk daemon vmId diskId InterfaceVirtio CacheWriteback False False

          -- Add user-mode network
          sshPort <- findFreePort
          let hostFwd = "hostfwd=tcp::" <> T.pack (show sshPort) <> "-:22"
          addVmNetIf daemon vmId NetUser hostFwd Nothing

          -- Start VM (async, Windows boot is slow)
          startTestVm daemon vmId

          -- Wait for healthcheck to appear (Windows boots slowly, allow 5 minutes)
          hc <- waitForHealthcheck daemon vmId 300
          hc `shouldSatisfy` const True

          -- Verify VM transitioned to "running" status
          mDetails <- withDaemonConnection daemon $ \conn -> vmShow conn (T.pack (show vmId))
          case mDetails of
            Right (Right (Just details)) ->
              vdStatus details `shouldBe` VmRunning
            other ->
              expectationFailure $ "Failed to get VM details: " ++ show other

          -- Test guest exec with Windows shell (cmd.exe /c, auto-detected)
          (code, stdout, _) <- runViaGuestAgent daemon vmId "echo windows-test-ok"
          code `shouldBe` ExitSuccess
          T.strip stdout `shouldSatisfy` T.isInfixOf "windows-test-ok"

          -- Verify OS detection reports Windows
          let qcfg = ssQemuConfig (tdState daemon)
              gaConns = ssGuestAgentConns (tdState daemon)
          osInfo <- guestGetOsInfo gaConns qcfg vmId
          case osInfo of
            Just info ->
              goiId info `shouldSatisfy` T.isPrefixOf "mswindows"
            Nothing ->
              expectationFailure "Failed to get OS info from guest agent"

          -- Cleanup
          stopTestVmAndWait daemon vmId 60
          deleteTestVm daemon vmId

    it "cloud-init user-data script and hostname are applied on Windows VM" $ \env -> do
      withWindowsImage $ \imagePath ->
        withTestDaemon env $ \daemon -> do
          -- Import the pre-built Windows image and create an overlay
          baseId <- registerDisk daemon "win-ci-base" (T.pack imagePath) (Just FormatQcow2)
          diskId <- createOverlay daemon "win-ci-test" baseId

          -- Set up UEFI firmware
          ovmfCodeId <- registerDisk daemon "win-ci-ovmf-code" "/usr/share/edk2/OvmfX64/OVMF_CODE.fd" (Just FormatRaw)
          ovmfVarsTemplateId <- registerDisk daemon "win-ci-ovmf-vars-tpl" "/usr/share/edk2/OvmfX64/OVMF_VARS.fd" (Just FormatRaw)
          ovmfVarsId <- cloneDisk daemon "win-ci-ovmf-vars" ovmfVarsTemplateId

          -- Create VM with cloud-init enabled
          vmId <- createTestVmWithOptions daemon "test-win-ci" 2 4096 Nothing False True True

          -- Set custom cloud-init user-data: a PowerShell script that creates a marker file
          setCloudInitConfig
            daemon
            vmId
            ( Just $
                T.unlines
                  [ "#ps1_sysnative"
                  , "Set-Content -Path 'C:\\cloud-init-marker.txt' -Value 'cloud-init-ok'"
                  , "net user Administrator corvus /y"
                  ]
            )
            Nothing
            False

          -- Attach UEFI firmware (pflash order: CODE then VARS)
          addVmDisk daemon vmId ovmfCodeId InterfacePflash CacheWriteback False True
          addVmDisk daemon vmId ovmfVarsId InterfacePflash CacheWriteback False False

          -- Add boot disk
          addVmDisk daemon vmId diskId InterfaceVirtio CacheWriteback False False

          -- Add user-mode network
          sshPort <- findFreePort
          let hostFwd = "hostfwd=tcp::" <> T.pack (show sshPort) <> "-:22"
          addVmNetIf daemon vmId NetUser hostFwd Nothing

          -- Start VM and wait for guest agent (Windows boots slowly)
          startTestVm daemon vmId
          _ <- waitForHealthcheck daemon vmId 300

          -- Poll for the marker file (cloudbase-init runs after boot, may take a while)
          stdout <- waitForMarkerFile daemon vmId 120
          T.strip stdout `shouldSatisfy` T.isInfixOf "cloud-init-ok"

          -- Verify cloud-init set the hostname from VM name
          (codeH, stdoutH, _) <- runViaGuestAgent daemon vmId "hostname"
          codeH `shouldBe` ExitSuccess
          T.strip stdoutH `shouldBe` "test-win-ci"

          -- Cleanup
          stopTestVmAndWait daemon vmId 60
          deleteTestVm daemon vmId

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Register a disk image and return its ID.
registerDisk :: TestDaemon -> T.Text -> T.Text -> Maybe DriveFormat -> IO Int64
registerDisk daemon name filePath mFormat = do
  result <- withDaemonConnection daemon $ \conn ->
    diskRegister conn name filePath mFormat Nothing
  case result of
    Right (Right (DiskCreated id')) -> pure id'
    Right (Left err) -> fail $ "RPC error registering disk: " <> show err
    Right (Right other) -> fail $ "Unexpected response: " <> show other
    Left err -> fail $ "Connection error: " <> show err

-- | Create a qcow2 overlay and return its ID.
createOverlay :: TestDaemon -> T.Text -> Int64 -> IO Int64
createOverlay daemon name baseId = do
  result <- withDaemonConnection daemon $ \conn ->
    diskCreateOverlay conn name (T.pack (show baseId)) Nothing
  case result of
    Right (Right (DiskCreated id')) -> pure id'
    Right (Left err) -> fail $ "RPC error creating overlay: " <> show err
    Right (Right other) -> fail $ "Unexpected response: " <> show other
    Left err -> fail $ "Connection error: " <> show err

-- | Clone a disk and return the clone's ID.
cloneDisk :: TestDaemon -> T.Text -> Int64 -> IO Int64
cloneDisk daemon name sourceId = do
  result <- withDaemonConnection daemon $ \conn ->
    diskClone conn name (T.pack (show sourceId)) Nothing
  case result of
    Right (Right (DiskCreated id')) -> pure id'
    Right (Left err) -> fail $ "RPC error cloning disk: " <> show err
    Right (Right other) -> fail $ "Unexpected response: " <> show other
    Left err -> fail $ "Connection error: " <> show err

-- | Poll until a marker file created by cloud-init user-data exists.
-- Retries every 5 seconds. Fails after timeout seconds.
waitForMarkerFile :: TestDaemon -> Int64 -> Int -> IO T.Text
waitForMarkerFile daemon vmId timeoutSec = go timeoutSec
  where
    go n
      | n <= 0 = fail $ "Cloud-init marker file not found after " <> show timeoutSec <> "s"
      | otherwise = do
          (code, stdout, _) <- runViaGuestAgent daemon vmId "type C:\\cloud-init-marker.txt"
          case code of
            ExitSuccess -> pure stdout
            _ -> do
              threadDelay 5000000
              go (n - 5)

-- | Poll until healthcheck timestamp is set. Fails after timeout seconds.
waitForHealthcheck :: TestDaemon -> Int64 -> Int -> IO UTCTime
waitForHealthcheck daemon vmId timeoutSec = go timeoutSec
  where
    go 0 = fail $ "Windows VM healthcheck not set after " <> show timeoutSec <> "s"
    go n = do
      mDetails <- withDaemonConnection daemon $ \conn -> vmShow conn (T.pack (show vmId))
      case mDetails of
        Right (Right (Just details))
          | Just hc <- vdHealthcheck details -> pure hc
        _ -> do
          threadDelay 2000000
          go (n - 1)
