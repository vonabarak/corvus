{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Corvus.ProtocolSpec (spec) where

import Corvus.Model
import Corvus.Protocol
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isInfixOf)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Test.Hspec
import Test.QuickCheck

-- | Fixed timestamp for deterministic tests
testTime :: UTCTime
testTime = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime 0)

-- Arbitrary instances for enums
instance Arbitrary VmStatus where
  arbitrary = elements [VmStopped, VmStarting, VmRunning, VmStopping, VmPaused, VmError]

instance Arbitrary DriveFormat where
  arbitrary = elements [FormatQcow2, FormatRaw, FormatVmdk, FormatVdi, FormatVpc, FormatVhdx]

instance Arbitrary DriveInterface where
  arbitrary = elements [InterfaceVirtio, InterfaceIde, InterfaceScsi, InterfaceSata, InterfaceNvme, InterfacePflash]

instance Arbitrary CacheType where
  arbitrary = elements [CacheNone, CacheWriteback, CacheWritethrough, CacheDirectsync, CacheUnsafe]

instance Arbitrary DriveMedia where
  arbitrary = elements [MediaDisk, MediaCdrom]

instance Arbitrary NetInterfaceType where
  arbitrary = elements [NetUser, NetTap, NetBridge, NetMacvtap, NetVde, NetManaged]

instance Arbitrary SharedDirCache where
  arbitrary = elements [CacheAlways, CacheAuto, CacheNever]

instance Arbitrary TemplateCloneStrategy where
  arbitrary = elements [StrategyClone, StrategyOverlay, StrategyDirect]

instance Arbitrary TaskSubsystem where
  arbitrary = elements [SubVm, SubDisk, SubNetwork, SubSshKey, SubTemplate, SubSharedDir, SubSnapshot, SubSystem, SubApply]

instance Arbitrary TaskResult where
  arbitrary = elements [TaskRunning, TaskSuccess, TaskError]

instance Arbitrary Ref where
  arbitrary = Ref . T.pack <$> arbitrary

-- Arbitrary for response data types
instance Arbitrary StatusInfo where
  arbitrary = StatusInfo <$> arbitrary <*> arbitrary <*> pure "test" <*> arbitrary

instance Arbitrary VmInfo where
  arbitrary = VmInfo <$> arbitrary <*> pure "test-vm" <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> pure Nothing

-- Arbitrary for Request (subset - the most common ones)
instance Arbitrary Request where
  arbitrary =
    oneof
      [ pure ReqPing
      , pure ReqStatus
      , pure ReqShutdown
      , pure ReqListVms
      , ReqShowVm <$> arbitrary
      , ReqVmCreate "test" <$> arbitrary <*> arbitrary <*> pure Nothing <*> arbitrary <*> arbitrary <*> arbitrary
      , ReqVmDelete <$> arbitrary
      , ReqVmStart <$> arbitrary <*> arbitrary
      , ReqVmStop <$> arbitrary <*> arbitrary
      , ReqVmPause <$> arbitrary
      , ReqVmReset <$> arbitrary
      , ReqDiskCreate "disk" <$> arbitrary <*> arbitrary <*> pure Nothing
      , ReqDiskDelete <$> arbitrary
      , pure ReqDiskList
      , ReqDiskShow <$> arbitrary
      , ReqSnapshotCreate <$> arbitrary <*> pure "snap"
      , ReqSnapshotList <$> arbitrary
      , pure ReqSshKeyList
      , pure ReqTemplateList
      , pure ReqDiskList
      , pure ReqNetworkList
      , ReqDiskRefresh <$> arbitrary
      , pure (ReqNetworkCreate "net" "10.0.0.0/24" False False)
      , ReqNetworkDelete <$> arbitrary
      , ReqNetworkShow <$> arbitrary
      , ReqGuestExec <$> arbitrary <*> pure "echo ok"
      , ReqApply "sshKeys: []" <$> arbitrary
      , ReqTaskList <$> arbitrary <*> pure Nothing <*> pure Nothing
      , ReqTaskShow <$> arbitrary
      ]

spec :: Spec
spec = sequential $ do
  describe "Protocol binary encoding" $ do
    it "Request round-trips through encode/decode" $ property $ \(req :: Request) ->
      decodeMessage (encodeMessage req) == Right req

  describe "Protocol JSON encoding" $ do
    it "StatusInfo produces valid JSON with expected fields" $ do
      let info = StatusInfo 3600 5 "1.0" Nothing
          json = BL.unpack (encode info)
      json `shouldSatisfy` ("uptime" `isInfixOf`)
      json `shouldSatisfy` ("connections" `isInfixOf`)
      json `shouldSatisfy` ("version" `isInfixOf`)

    it "VmInfo produces valid JSON with expected fields" $ do
      let vm = VmInfo 1 "test" VmRunning 2 1024 False False False Nothing
          json = BL.unpack (encode vm)
      json `shouldSatisfy` ("name" `isInfixOf`)
      json `shouldSatisfy` ("status" `isInfixOf`)
      json `shouldSatisfy` ("running" `isInfixOf`)

    it "NetworkInfo produces valid JSON with expected fields" $ do
      let nw = NetworkInfo 1 "lab-net" "10.0.0.0/24" False False False Nothing testTime
          json = BL.unpack (encode nw)
      json `shouldSatisfy` ("name" `isInfixOf`)
      json `shouldSatisfy` ("subnet" `isInfixOf`)
      json `shouldSatisfy` ("lab-net" `isInfixOf`)

    it "DriveInfo produces valid JSON with enum values" $ do
      let drive = DriveInfo 1 10 "disk1" InterfaceVirtio "/path.qcow2" FormatQcow2 Nothing False CacheWriteback True
          json = BL.unpack (encode drive)
      json `shouldSatisfy` ("virtio" `isInfixOf`)
      json `shouldSatisfy` ("qcow2" `isInfixOf`)
      json `shouldSatisfy` ("writeback" `isInfixOf`)
      json `shouldSatisfy` ("discard" `isInfixOf`)
