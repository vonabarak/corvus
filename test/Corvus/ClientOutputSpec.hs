{-# LANGUAGE OverloadedStrings #-}

module Corvus.ClientOutputSpec (spec) where

import Corvus.Model
import Corvus.Protocol
import Data.Aeson (ToJSON (..), Value (..), encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec

-- Fixed timestamp for deterministic tests
testTime :: UTCTime
testTime = posixSecondsToUTCTime 1700000000

spec :: Spec
spec = sequential $ do
  describe "JSON serialization of Protocol types" $ do
    describe "StatusInfo" $ do
      it "serializes with correct field names" $ do
        let info = StatusInfo 3600 5 "1.0.0" (Just 12345)
            val = toJSON info
        val
          `shouldBe` object
            [ "uptime" .= (3600 :: Int)
            , "connections" .= (5 :: Int)
            , "version" .= ("1.0.0" :: String)
            , "namespacePid" .= Just (12345 :: Int)
            ]

    describe "VmInfo" $ do
      it "serializes with correct field names and enum values" $ do
        let vm = VmInfo 1 "my-vm" VmRunning 4 2048 False False False Nothing False
            val = toJSON vm
        val
          `shouldBe` object
            [ "id" .= (1 :: Int)
            , "name" .= ("my-vm" :: String)
            , "status" .= ("running" :: String)
            , "cpuCount" .= (4 :: Int)
            , "ramMb" .= (2048 :: Int)
            , "headless" .= False
            , "guestAgent" .= False
            , "cloudInit" .= False
            , "healthcheck" .= Null
            , "autostart" .= False
            ]

      it "serializes stopped status correctly" $ do
        let vm = VmInfo 2 "test" VmStopped 1 512 False False False Nothing False
            json = encode vm
        BL.unpack json `shouldSatisfy` isInfixOf "\"stopped\""

    describe "DiskImageInfo" $ do
      it "serializes with all fields" $ do
        let disk = DiskImageInfo 1 "boot" "/path/boot.qcow2" FormatQcow2 (Just 10240) testTime [(1, "vm1"), (2, "vm2")] Nothing Nothing
            val = toJSON disk
        case val of
          Object obj -> do
            KM.lookup "id" obj `shouldBe` Just (Number 1)
            KM.lookup "name" obj `shouldBe` Just (String "boot")
            KM.lookup "format" obj `shouldBe` Just (String "qcow2")
            KM.lookup "attachedTo" obj `shouldSatisfy` isJust
          _ -> fail "Expected JSON object"

      it "serializes overlay backing info" $ do
        let disk = DiskImageInfo 2 "overlay" "/path/overlay.qcow2" FormatQcow2 Nothing testTime [] (Just 1) (Just "base")
            val = toJSON disk
        case val of
          Object obj -> do
            KM.lookup "backingImageId" obj `shouldBe` Just (Number 1)
            KM.lookup "backingImageName" obj `shouldBe` Just (String "base")
          _ -> fail "Expected JSON object"

    describe "SnapshotInfo" $ do
      it "serializes correctly" $ do
        let snap = SnapshotInfo 5 "before-upgrade" testTime (Just 100)
            val = toJSON snap
        case val of
          Object obj -> do
            KM.lookup "id" obj `shouldBe` Just (Number 5)
            KM.lookup "name" obj `shouldBe` Just (String "before-upgrade")
            KM.lookup "sizeMb" obj `shouldBe` Just (Number 100)
          _ -> fail "Expected JSON object"

    describe "SharedDirInfo" $ do
      it "serializes with cache enum" $ do
        let dir = SharedDirInfo 1 "/host/share" "myshare" CacheAuto False Nothing
            val = toJSON dir
        case val of
          Object obj -> do
            KM.lookup "cache" obj `shouldBe` Just (String "auto")
            KM.lookup "readOnly" obj `shouldBe` Just (Bool False)
          _ -> fail "Expected JSON object"

    describe "SshKeyInfo" $ do
      it "serializes with attached VMs" $ do
        let key = SshKeyInfo 1 "mykey" "ssh-ed25519 AAAA..." testTime [(1, "vm1"), (3, "vm3")]
            val = toJSON key
        case val of
          Object obj -> do
            KM.lookup "name" obj `shouldBe` Just (String "mykey")
            KM.lookup "attachedVms" obj `shouldSatisfy` isJust
          _ -> fail "Expected JSON object"

    describe "DriveInfo" $ do
      it "serializes interface and cache enums" $ do
        let drive = DriveInfo 1 10 "disk" InterfaceVirtio "/path/disk.qcow2" FormatQcow2 (Just MediaDisk) False CacheWriteback True
            val = toJSON drive
        case val of
          Object obj -> do
            KM.lookup "interface" obj `shouldBe` Just (String "virtio")
            KM.lookup "cacheType" obj `shouldBe` Just (String "writeback")
            KM.lookup "media" obj `shouldBe` Just (String "disk")
            KM.lookup "discard" obj `shouldBe` Just (Bool True)
          _ -> fail "Expected JSON object"

    describe "TemplateVmInfo" $ do
      it "serializes with optional description" $ do
        let t = TemplateVmInfo 1 "my-template" 2 1024 (Just "A test template") False False False
            val = toJSON t
        case val of
          Object obj -> do
            KM.lookup "name" obj `shouldBe` Just (String "my-template")
            KM.lookup "description" obj `shouldBe` Just (String "A test template")
          _ -> fail "Expected JSON object"

      it "serializes null description" $ do
        let t = TemplateVmInfo 1 "minimal" 1 512 Nothing False False False
            val = toJSON t
        case val of
          Object obj -> do
            KM.lookup "description" obj `shouldBe` Just Null
          _ -> fail "Expected JSON object"

    describe "List serialization" $ do
      it "empty list serializes to []" $ do
        encode ([] :: [VmInfo]) `shouldBe` "[]"

      it "VM list serializes as JSON array" $ do
        let vms = [VmInfo 1 "a" VmRunning 1 512 False False False Nothing False, VmInfo 2 "b" VmStopped 2 1024 False False False Nothing False]
            val = toJSON vms
        case val of
          Array arr -> length arr `shouldBe` 2
          _ -> fail "Expected JSON array"
