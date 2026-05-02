{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for the @crv build@ image-bake pipeline.
--
-- These spin up a real bake VM from the prebaked corvus-test image,
-- run a tiny shell provisioner inside it, and verify that the
-- resulting artifact disk:
--
--   * is registered in the daemon under the requested name,
--   * is no longer attached to the (deleted) bake VM,
--   * actually contains the marker file the provisioner wrote, and
--   * has a populated @\/etc\/corvus-build-info@.
--
-- Run with: stack test --test-arguments="--match BuildIntegration"
module Corvus.BuildIntegrationSpec (spec) where

import Control.Monad (void)
import Corvus.Client
  ( DiskResult (..)
  , VmCreateResult (..)
  , diskAttach
  , diskDelete
  , diskDetach
  , diskList
  , templateCreate
  , templateDelete
  , vmCreate
  , vmDelete
  , vmList
  )
import Corvus.Client.Rpc (BuildRpcResult (..), runBuild)
import Corvus.Model (CacheType (..), DriveInterface (..))
import Corvus.Protocol
  ( BuildOne (..)
  , BuildResult (..)
  , DiskImageInfo (..)
  , VmInfo (..)
  )
import Data.Int (Int64)
import qualified Data.List as List
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.Database (withTestDb)
import Test.Hspec
import Test.VM.Common (TestVm (..), defaultVmConfig, withTestVm)
import Test.VM.Daemon (TestDaemon, withDaemonConnection)
import Test.VM.Rpc (runViaGuestAgent, startTestVmSync, stopTestVmAndWait)

-- | Find a disk by a name prefix in the daemon's disk list.
findDiskByPrefix :: TestDaemon -> T.Text -> IO (Maybe DiskImageInfo)
findDiskByPrefix daemon prefix = do
  res <- withDaemonConnection daemon $ \conn -> diskList conn
  case res of
    Right (Right (DiskListResult ds)) ->
      pure $ List.find (T.isPrefixOf prefix . diiName) ds
    _ -> pure Nothing

-- | Find a disk by exact name.
findDiskByName :: TestDaemon -> T.Text -> IO (Maybe DiskImageInfo)
findDiskByName daemon name = do
  res <- withDaemonConnection daemon $ \conn -> diskList conn
  case res of
    Right (Right (DiskListResult ds)) ->
      pure $ List.find ((== name) . diiName) ds
    _ -> pure Nothing

-- | Assert no leftover @__build_*__@ VM in the daemon's VM list.
assertNoBuildOrphans :: TestDaemon -> IO ()
assertNoBuildOrphans daemon = do
  res <- withDaemonConnection daemon $ \conn -> vmList conn
  case res of
    Right (Right vms) ->
      any (T.isPrefixOf "__build_" . viName) vms `shouldBe` False
    other -> fail $ "vm list failed: " ++ show other

spec :: Spec
spec = withTestDb $ do
  describe "Build integration" $ do
    it "bakes an artifact and the marker file is present in the result" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm

        -- Stop the seed VM so we can reuse its base disk for the
        -- template we're about to register. The seed's boot disk is
        -- a per-VM overlay; the template's @overlay@ strategy needs
        -- the *registered* base. Discover its auto-generated name.
        stopTestVmAndWait daemon (tvmId vm) 30

        mBaseDisk <- findDiskByPrefix daemon "base-image"
        baseDiskName <- case mBaseDisk of
          Just d -> pure (diiName d)
          Nothing -> fail "expected a base-image-* disk in the test daemon"

        -- Register a minimal template that boots from an overlay over
        -- the base disk and has guestAgent enabled. The build will
        -- instantiate this template, run a one-line provisioner, and
        -- publish the flattened overlay as the artifact.
        let templateYaml =
              T.unlines
                [ "name: build-test-tpl"
                , "cpuCount: 2"
                , "ramMb: 1024"
                , "guestAgent: true"
                , "headless: true"
                , "drives:"
                , "  - diskImageName: \"" <> baseDiskName <> "\""
                , "    interface: \"virtio\""
                , "    strategy: \"overlay\""
                ]
        tCreate <- withDaemonConnection daemon $ \conn -> templateCreate conn templateYaml
        case tCreate of
          Right (Right _) -> pure ()
          other -> fail $ "template create failed: " ++ show other

        let artifactName :: T.Text
            artifactName = "build-test-artifact"
            buildYaml =
              T.unlines
                [ "builds:"
                , "  - name: build-test"
                , "    template: build-test-tpl"
                , "    target:"
                , "      name: " <> artifactName
                , "      compact: false"
                , "    strategy: overlay"
                , "    vm: { cpuCount: 2, ramMb: 1024 }"
                , "    provisioners:"
                , "      - shell: |"
                , "          set -eux"
                , "          mkdir -p /var/lib/corvus-test"
                , "          echo bake-marker > /var/lib/corvus-test/marker"
                , "    cleanup: always"
                ]

        putStrLn "[test] Running crv build (this boots a VM)..."
        buildResp <- withDaemonConnection daemon $ \conn -> runBuild conn buildYaml True (\_ -> pure ())
        case buildResp of
          Right (Right (BuildOk (BuildResult [bOne]))) -> do
            boName bOne `shouldBe` "build-test"
            boError bOne `shouldBe` Nothing
            boArtifactDiskId bOne `shouldSatisfy` \case
              Just _ -> True
              Nothing -> False
          other -> fail $ "build failed: " ++ show other

        -- Artifact must exist under the requested name.
        mArtifact <- findDiskByName daemon artifactName
        artifactDiskId <- case mArtifact of
          Just d -> pure (diiId d)
          Nothing -> fail $ "artifact disk '" ++ T.unpack artifactName ++ "' not found"

        -- The bake VM must be gone.
        assertNoBuildOrphans daemon

        -- Validate by booting a fresh VM from the artifact and reading
        -- both the marker file and /etc/corvus-build-info.
        verifyArtifactDisk daemon artifactDiskId

        -- Cleanup: delete the artifact and template.
        _ <- withDaemonConnection daemon $ \conn ->
          diskDelete conn (T.pack (show artifactDiskId))
        void $ withDaemonConnection daemon $ \conn ->
          templateDelete conn "build-test-tpl"

    it "fails cleanly when a provisioner exits non-zero (cleanup: always leaves no orphans)" $ \env -> do
      withTestVm env defaultVmConfig $ \vm -> do
        let daemon = tvmDaemon vm
        stopTestVmAndWait daemon (tvmId vm) 30

        mBase <- findDiskByPrefix daemon "base-image"
        baseDiskName <- case mBase of
          Just d -> pure (diiName d)
          Nothing -> fail "base-image disk missing"

        let templateYaml =
              T.unlines
                [ "name: build-fail-tpl"
                , "cpuCount: 2"
                , "ramMb: 1024"
                , "guestAgent: true"
                , "headless: true"
                , "drives:"
                , "  - diskImageName: \"" <> baseDiskName <> "\""
                , "    interface: \"virtio\""
                , "    strategy: \"overlay\""
                ]
        _ <- withDaemonConnection daemon $ \conn -> templateCreate conn templateYaml

        let buildYaml =
              T.unlines
                [ "builds:"
                , "  - name: failing-build"
                , "    template: build-fail-tpl"
                , "    target:"
                , "      name: should-not-exist"
                , "      compact: false"
                , "    strategy: overlay"
                , "    vm: { cpuCount: 2, ramMb: 1024 }"
                , "    provisioners:"
                , "      - shell: \"exit 7\""
                , "    cleanup: always"
                ]

        buildResp <- withDaemonConnection daemon $ \conn -> runBuild conn buildYaml True (\_ -> pure ())
        case buildResp of
          Right (Right (BuildOk (BuildResult [bOne]))) -> do
            boArtifactDiskId bOne `shouldBe` Nothing
            case boError bOne of
              Just msg ->
                (T.isInfixOf "shell" msg || T.isInfixOf "exited" msg)
                  `shouldBe` True
              Nothing -> expectationFailure "expected an error message on failed build"
          other -> fail $ "expected build failure response, got: " ++ show other

        -- No artifact registered.
        mArt <- findDiskByName daemon "should-not-exist"
        mArt `shouldBe` Nothing

        -- No build-* VM left behind.
        assertNoBuildOrphans daemon

        void $ withDaemonConnection daemon $ \conn ->
          templateDelete conn "build-fail-tpl"

-- | Boot a fresh VM that uses the artifact disk as its bootdisk and
-- assert the marker file plus the build-info file are present and well
-- formed. Tears the verifying VM down at the end.
verifyArtifactDisk :: TestDaemon -> Int64 -> IO ()
verifyArtifactDisk daemon artifactDiskId = do
  vmCreateResp <- withDaemonConnection daemon $ \conn ->
    vmCreate conn "build-verify-vm" 2 1024 Nothing True True False False
  newVmId <- case vmCreateResp of
    Right (Right (VmCreated vmId)) -> pure vmId
    other -> fail $ "verifier VM create failed: " ++ show other

  attachResp <- withDaemonConnection daemon $ \conn ->
    diskAttach
      conn
      (T.pack (show newVmId))
      (T.pack (show artifactDiskId))
      InterfaceVirtio
      Nothing
      False
      False
      CacheWriteback
  case attachResp of
    Right (Right (DriveAttached _)) -> pure ()
    other -> fail $ "attach artifact to verifier VM failed: " ++ show other

  putStrLn "[test] Booting verifier VM from the baked artifact..."
  startTestVmSync daemon newVmId

  (markerCode, markerOut, _) <-
    runViaGuestAgent daemon newVmId "cat /var/lib/corvus-test/marker"
  markerCode `shouldBe` ExitSuccess
  T.strip markerOut `shouldBe` "bake-marker"

  (infoCode, infoOut, _) <-
    runViaGuestAgent daemon newVmId "cat /etc/corvus-build-info"
  infoCode `shouldBe` ExitSuccess
  T.isInfixOf "build_name: build-test" infoOut `shouldBe` True
  T.isInfixOf "source_template: build-test-tpl" infoOut `shouldBe` True

  stopTestVmAndWait daemon newVmId 30

  -- Detach + delete the verifier VM, leaving the artifact intact.
  _ <- withDaemonConnection daemon $ \conn ->
    diskDetach conn (T.pack (show newVmId)) (T.pack (show artifactDiskId))
  _ <- withDaemonConnection daemon $ \conn ->
    vmDelete conn (T.pack (show newVmId)) False
  pure ()
