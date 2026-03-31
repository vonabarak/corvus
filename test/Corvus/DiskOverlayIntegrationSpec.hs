{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for qcow2 disk overlay support.
-- These tests run against a real daemon and create actual overlay images
-- using qemu-img. Requires qemu-img on PATH.
--
-- Run with: stack test --ta '--match DiskOverlayIntegration'
module Corvus.DiskOverlayIntegrationSpec (spec) where

import Control.Exception (bracket_, finally)
import Corvus.Client (DiskResult (..), diskCreateOverlay, diskList, diskRegister)
import Corvus.Model (DriveFormat (..))
import Corvus.Protocol (DiskImageInfo (..))
import Data.List (find)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Daemon (TestDaemon, startTestDaemon, stopTestDaemon, withDaemonConnection)

spec :: Spec
spec = withTestDb $ do
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
          finally (runOverlayTest daemon basePath) (stopTestDaemon daemon)

runOverlayTest :: TestDaemon -> FilePath -> IO ()
runOverlayTest daemon basePath = do
  regResult <-
    withDaemonConnection daemon $ \conn ->
      diskRegister conn "base-disk" (T.pack basePath) FormatQcow2 (Just 1)
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
      length disks `shouldBe` 2
      let mOverlay = find (\d -> diiId d == overlayId) disks
      case mOverlay of
        Nothing -> fail "Overlay disk not in list"
        Just overlay -> do
          diiBackingImageId overlay `shouldBe` Just baseId
          diiBackingImageName overlay `shouldBe` Just "base-disk"
    Right (Left err) -> fail $ "RPC error: " ++ show err
    Right (Right other) -> fail $ "Unexpected list response: " ++ show other
