{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for Windows VM guest agent support.
-- Verifies that Corvus can communicate with the QEMU Guest Agent on Windows VMs:
-- healthcheck pings, OS detection, and command execution via cmd.exe.
--
-- Requirements:
--   - Pre-built Windows test image at .test-images/windows-server-eval.qcow2
--     (build with: scripts/build-windows-test-image.sh)
--   - UEFI firmware (OVMF)
--   - QEMU with KVM support
--   - PostgreSQL for test database
--
-- Run with: stack test --test-arguments="--match WindowsIntegration"
module Corvus.WindowsIntegrationSpec (spec) where

import Control.Concurrent (threadDelay)
import Corvus.Client (showVm)
import Corvus.Client.Rpc (DiskResult (..), diskRegister)
import Corvus.Model (CacheType (..), DriveFormat (..), DriveInterface (..), NetInterfaceType (..), VmStatus (..))
import Corvus.Protocol (VmDetails (..))
import Corvus.Qemu.GuestAgent (GuestOsInfo (..), guestGetOsInfo)
import Corvus.Types (ssQemuConfig)
import Data.Int (Int64)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time (UTCTime)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (findFreePort)
import Test.VM.Daemon (TestDaemon (..), withDaemonConnection, withTestDaemon)
import Test.VM.Rpc
  ( addVmDisk
  , addVmNetIf
  , createTestVmWithOptions
  , deleteTestVm
  , runViaGuestAgent
  , startTestVm
  , stopTestVmAndWait
  )

-- | Path to the pre-built Windows test image (relative to project root)
windowsImagePath :: FilePath
windowsImagePath = ".test-images" </> "windows-server-eval.qcow2"

spec :: Spec
spec = withTestDb $ do
  describe "Windows VM Integration" $ do
    it "guest agent healthcheck and exec work on Windows VM" $ \env -> do
      projectRoot <- getCurrentDirectory
      let imagePath = projectRoot </> windowsImagePath
      exists <- doesFileExist imagePath
      if not exists
        then
          pendingWith $
            "Windows test image not found at "
              ++ imagePath
              ++ ". Build with: scripts/build-windows-test-image.sh"
        else withTestDaemon env $ \daemon -> do
          -- Import the pre-built Windows image
          diskId <- importWindowsDisk daemon (T.pack imagePath)

          -- Create VM: headless=False (SPICE graphics), guest-agent=True,
          -- cloud-init=False (Windows doesn't use NoCloud cloud-init)
          vmId <- createTestVmWithOptions daemon "test-win-vm" 2 4096 Nothing False True False

          -- Add boot disk
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
          mDetails <- withDaemonConnection daemon $ \conn -> showVm conn (T.pack (show vmId))
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
          osInfo <- guestGetOsInfo qcfg vmId
          case osInfo of
            Just info ->
              goiId info `shouldSatisfy` T.isPrefixOf "mswindows"
            Nothing ->
              expectationFailure "Failed to get OS info from guest agent"

          -- Cleanup
          stopTestVmAndWait daemon vmId 60
          deleteTestVm daemon vmId

-- | Import a Windows disk image via daemon RPC and return its ID.
importWindowsDisk :: TestDaemon -> T.Text -> IO Int64
importWindowsDisk daemon filePath = do
  result <- withDaemonConnection daemon $ \conn ->
    diskRegister conn "win-test-base" filePath (Just FormatQcow2)
  case result of
    Left err -> fail $ "Failed to connect: " <> show err
    Right (Left err) -> fail $ "RPC error: " <> show err
    Right (Right (DiskCreated id')) -> pure id'
    Right (Right other) -> fail $ "Failed to import disk: " <> show other

-- | Poll until healthcheck timestamp is set. Fails after timeout seconds.
waitForHealthcheck :: TestDaemon -> Int64 -> Int -> IO UTCTime
waitForHealthcheck daemon vmId timeoutSec = go timeoutSec
  where
    go 0 = fail $ "Windows VM healthcheck not set after " <> show timeoutSec <> "s"
    go n = do
      mDetails <- withDaemonConnection daemon $ \conn -> showVm conn (T.pack (show vmId))
      case mDetails of
        Right (Right (Just details))
          | Just hc <- vdHealthcheck details -> pure hc
        _ -> do
          threadDelay 2000000
          go (n - 1)
