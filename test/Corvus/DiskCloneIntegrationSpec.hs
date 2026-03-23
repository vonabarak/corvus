{-# LANGUAGE OverloadedStrings #-}

module Corvus.DiskCloneIntegrationSpec (spec) where

import Control.Exception (catch)
import Control.Monad (void)
import Corvus.Client
import Corvus.Protocol (DiskImageInfo (..), SnapshotInfo (..))
import Data.List (find)
import qualified Data.Text as T
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Test.DSL.Daemon (stopTestVmAndWait)
import Test.Daemon (withDaemonConnection)
import Test.Database (TestEnv, withTestDb)
import Test.Hspec
import Test.VM.Common (TestVm (..), defaultVmConfig, withTestVm)

spec :: Spec
spec = withTestDb $ do
  describe "Disk cloning integration" $ do
    it "can clone a disk and its content" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
            diskId = tvmDiskId vm
            vmId = tvmId vm

        -- Must stop VM to clone
        stopTestVmAndWait daemon vmId 10

        -- Clone the disk
        res <- withDaemonConnection daemon $ \conn -> diskClone conn "cloned-disk" diskId Nothing
        newDiskId <- case res of
          Right (Right (DiskCreated id_)) -> pure id_
          other -> fail $ "Clone failed: " ++ show other

        -- Verify new disk exists and has correct info
        listResult <- withDaemonConnection daemon $ \conn -> diskList conn
        case listResult of
          Right (Right (DiskListResult disks)) -> do
            let mCloned = find (\d -> diiId d == newDiskId) disks
            case mCloned of
              Nothing -> fail "Cloned disk not found in list"
              Just cloned -> do
                diiName cloned `shouldBe` "cloned-disk"
          other -> fail $ "List failed: " ++ show other

    it "clones snapshots in the database" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
            diskId = tvmDiskId vm
            vmId = tvmId vm

        stopTestVmAndWait daemon vmId 10

        -- Create a snapshot
        void $ withDaemonConnection daemon $ \conn -> snapshotCreate conn diskId "snap1"

        -- Clone the disk
        res <- withDaemonConnection daemon $ \conn -> diskClone conn "cloned-with-snap" diskId Nothing
        newDiskId <- case res of
          Right (Right (DiskCreated id_)) -> pure id_
          other -> fail $ "Clone failed: " ++ show other

        -- Verify snapshots for the new disk
        snapsResult <- withDaemonConnection daemon $ \conn -> snapshotList conn newDiskId
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

        -- Remove if exists
        catch (removeFile customPath) $ \e ->
          if isDoesNotExistError e then pure () else ioError e

        res <- withDaemonConnection daemon $ \conn -> diskClone conn "custom-path-clone" diskId (Just $ T.pack customPath)
        case res of
          Right (Right (DiskCreated _)) -> pure ()
          other -> fail $ "Clone to custom path failed: " ++ show other

        exists <- doesFileExist customPath
        exists `shouldBe` True

        -- Cleanup
        removeFile customPath

    it "rejects cloning if VM is running" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
            diskId = tvmDiskId vm

        -- VM is running
        res <- withDaemonConnection daemon $ \conn -> diskClone conn "failed-clone" diskId Nothing
        case res of
          Right (Right VmMustBeStopped) -> pure ()
          other -> fail $ "Expected VmMustBeStopped but got: " ++ show other
