module Corvus.WireSpec (spec) where

import qualified Capnp.Gen.Enums as E
import qualified Corvus.Model as M
import qualified Corvus.Protocol as P
import qualified Corvus.Protocol.Disk as PD
import qualified Corvus.Protocol.Node as PN
import qualified Corvus.Protocol.Task as PT
import qualified Corvus.Protocol.Vm as PV
import Corvus.Wire.Common
  ( EntityRef (..)
  , entityRefFromText
  , fromCapnpEntityRef
  , toCapnpEntityRef
  )
import Corvus.Wire.Enums
import Corvus.Wire.Errors (WireError (..))
import Corvus.Wire.Node (fromCapnpNodeDetails, fromCapnpNodeInfo, toCapnpNodeDetails, toCapnpNodeInfo)
import Corvus.Wire.Task (fromCapnpTaskInfo, toCapnpTaskInfo)
import Corvus.Wire.Time (nanosToUtcTime, nanosToUtcTimeMaybe, utcTimeToNanos, utcTimeToNanosMaybe)
import Corvus.Wire.Vm (fromCapnpVmStats, toCapnpVmStats)
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec

spec :: Spec
spec = do
  describe "Wire.Enums" $ do
    it "VmStatus round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpVmStatus (toCapnpVmStatus s) `shouldBe` Right s)
        [minBound .. maxBound :: M.VmStatus]
    it "DriveInterface round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpDriveInterface (toCapnpDriveInterface s) `shouldBe` Right s)
        [minBound .. maxBound :: M.DriveInterface]
    it "DriveFormat round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpDriveFormat (toCapnpDriveFormat s) `shouldBe` Right s)
        [minBound .. maxBound :: M.DriveFormat]
    it "DriveMedia round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpDriveMedia (toCapnpDriveMedia s) `shouldBe` Right s)
        [minBound .. maxBound :: M.DriveMedia]
    it "CacheType round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpCacheType (toCapnpCacheType s) `shouldBe` Right s)
        [minBound .. maxBound :: M.CacheType]
    it "NetInterfaceType round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpNetInterfaceType (toCapnpNetInterfaceType s) `shouldBe` Right s)
        [minBound .. maxBound :: M.NetInterfaceType]
    it "SharedDirCache round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpSharedDirCache (toCapnpSharedDirCache s) `shouldBe` Right s)
        [minBound .. maxBound :: M.SharedDirCache]
    it "TemplateCloneStrategy round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpTemplateCloneStrategy (toCapnpTemplateCloneStrategy s) `shouldBe` Right s)
        [minBound .. maxBound :: M.TemplateCloneStrategy]
    it "TaskSubsystem round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpTaskSubsystem (toCapnpTaskSubsystem s) `shouldBe` Right s)
        [minBound .. maxBound :: M.TaskSubsystem]
    it "TaskResult round-trips for every constructor" $
      mapM_
        (\s -> fromCapnpTaskResult (toCapnpTaskResult s) `shouldBe` Right s)
        [minBound .. maxBound :: M.TaskResult]

  describe "Wire.Enums additional constructors and unknown tags" $ do
    it "AudioDeviceModel round-trips every constructor" $ mapM_ (\x -> fromCapnpAudioDeviceModel (toCapnpAudioDeviceModel x) `shouldBe` Right x) [minBound .. maxBound :: M.AudioDeviceModel]
    it "NetworkDeviceModel round-trips every constructor" $ mapM_ (\x -> fromCapnpNetworkDeviceModel (toCapnpNetworkDeviceModel x) `shouldBe` Right x) [minBound .. maxBound :: M.NetworkDeviceModel]
    it "GraphicsAdapter round-trips every constructor" $ mapM_ (\x -> fromCapnpGraphicsAdapter (toCapnpGraphicsAdapter x) `shouldBe` Right x) [minBound .. maxBound :: M.GraphicsAdapter]
    it "AudioBackend round-trips every constructor" $ mapM_ (\x -> fromCapnpAudioBackend (toCapnpAudioBackend x) `shouldBe` Right x) [minBound .. maxBound :: M.AudioBackend]
    it "NodeAdminState round-trips every constructor" $ mapM_ (\x -> fromCapnpNodeAdminState (toCapnpNodeAdminState x) `shouldBe` Right x) [minBound .. maxBound :: M.NodeAdminState]
    it "UploadIfExists round-trips every policy" $ mapM_ (\x -> fromCapnpUploadIfExists (toCapnpUploadIfExists x) `shouldBe` Right x) [PD.UploadError, PD.UploadSkip, PD.UploadOverwrite, PD.UploadUpdate]
    it "rejects unknown VmStatus tags" $ fromCapnpVmStatus (E.VmStatus'unknown' 65535) `shouldBe` Left (WireUnknownEnum "VmStatus" 65535)
    it "rejects unknown DriveInterface tags" $ fromCapnpDriveInterface (E.DriveInterface'unknown' 65535) `shouldBe` Left (WireUnknownEnum "DriveInterface" 65535)
    it "rejects unknown DriveFormat tags" $ fromCapnpDriveFormat (E.DriveFormat'unknown' 65535) `shouldBe` Left (WireUnknownEnum "DriveFormat" 65535)
    it "rejects unknown DriveMedia tags" $ fromCapnpDriveMedia (E.DriveMedia'unknown' 65535) `shouldBe` Left (WireUnknownEnum "DriveMedia" 65535)
    it "rejects unknown CacheType tags" $ fromCapnpCacheType (E.CacheType'unknown' 65535) `shouldBe` Left (WireUnknownEnum "CacheType" 65535)
    it "rejects unknown NetInterfaceType tags" $ fromCapnpNetInterfaceType (E.NetInterfaceType'unknown' 65535) `shouldBe` Left (WireUnknownEnum "NetInterfaceType" 65535)
    it "rejects unknown SharedDirCache tags" $ fromCapnpSharedDirCache (E.SharedDirCache'unknown' 65535) `shouldBe` Left (WireUnknownEnum "SharedDirCache" 65535)
    it "rejects unknown TemplateCloneStrategy tags" $ fromCapnpTemplateCloneStrategy (E.TemplateCloneStrategy'unknown' 65535) `shouldBe` Left (WireUnknownEnum "TemplateCloneStrategy" 65535)
    it "rejects unknown TaskSubsystem tags" $ fromCapnpTaskSubsystem (E.TaskSubsystem'unknown' 65535) `shouldBe` Left (WireUnknownEnum "TaskSubsystem" 65535)
    it "rejects unknown TaskResult tags" $ fromCapnpTaskResult (E.TaskResult'unknown' 65535) `shouldBe` Left (WireUnknownEnum "TaskResult" 65535)
    it "rejects unknown AudioDeviceModel tags" $ fromCapnpAudioDeviceModel (E.AudioDeviceModel'unknown' 65535) `shouldBe` Left (WireUnknownEnum "AudioDeviceModel" 65535)
    it "rejects unknown NetworkDeviceModel tags" $ fromCapnpNetworkDeviceModel (E.NetworkDeviceModel'unknown' 65535) `shouldBe` Left (WireUnknownEnum "NetworkDeviceModel" 65535)
    it "rejects unknown GraphicsAdapter tags" $ fromCapnpGraphicsAdapter (E.GraphicsAdapter'unknown' 65535) `shouldBe` Left (WireUnknownEnum "GraphicsAdapter" 65535)
    it "rejects unknown AudioBackend tags" $ fromCapnpAudioBackend (E.AudioBackend'unknown' 65535) `shouldBe` Left (WireUnknownEnum "AudioBackend" 65535)
    it "rejects unknown NodeAdminState tags" $ fromCapnpNodeAdminState (E.NodeAdminState'unknown' 65535) `shouldBe` Left (WireUnknownEnum "NodeAdminState" 65535)
    it "rejects unknown UploadIfExists tags" $ fromCapnpUploadIfExists (E.UploadIfExists'unknown' 65535) `shouldBe` Left (WireUnknownEnum "UploadIfExists" 65535)

  describe "Wire.Common.EntityRef" $ do
    it "round-trips numeric ids" $
      fromCapnpEntityRef (toCapnpEntityRef (RefById 42)) `shouldBe` Right (RefById 42)
    it "round-trips symbolic names" $
      fromCapnpEntityRef (toCapnpEntityRef (RefByName "web-1")) `shouldBe` Right (RefByName "web-1")
    it "round-trips empty names" $
      fromCapnpEntityRef (toCapnpEntityRef (RefByName "")) `shouldBe` Right (RefByName "")

  describe "Wire.Common.entityRefFromText (CLI heuristic)" $ do
    it "parses bare digits as id" $
      entityRefFromText "42" `shouldBe` RefById 42
    it "parses zero as id" $
      entityRefFromText "0" `shouldBe` RefById 0
    it "treats negative numbers as names (CLI doesn't address by negative id)" $
      entityRefFromText "-1" `shouldBe` RefByName "-1"
    it "treats mixed-content tokens as names" $
      entityRefFromText "web-1" `shouldBe` RefByName "web-1"
    it "treats plain alphabetic tokens as names" $
      entityRefFromText "webserver" `shouldBe` RefByName "webserver"
    it "treats names beginning with digits as names" $
      entityRefFromText "42web" `shouldBe` RefByName "42web"

  describe "Wire.Time" $ do
    -- The wire layer truncates to nanoseconds, which is lossless
    -- for any UTCTime whose picoseconds-since-epoch divide evenly
    -- by 1000. We pick samples on second / millisecond boundaries
    -- to dodge the precision floor.
    it "utcTimeToNanos . nanosToUtcTime ≡ id for sample points" $ do
      let samples = [0, 1000000000, 1700000000000000000]
      mapM_ (\n -> utcTimeToNanos (nanosToUtcTime n) `shouldBe` n) samples

    it "utcTimeToNanosMaybe Nothing maps to the 0 sentinel" $
      utcTimeToNanosMaybe Nothing `shouldBe` 0

    it "nanosToUtcTimeMaybe 0 maps back to Nothing" $
      nanosToUtcTimeMaybe 0 `shouldBe` Nothing

    it "nanosToUtcTimeMaybe of a non-zero value round-trips" $ do
      let t = sampleUtc
      utcTimeToNanosMaybe (nanosToUtcTimeMaybe (utcTimeToNanos t)) `shouldBe` utcTimeToNanos t

  describe "Wire.Node.NodeInfo" $ do
    it "round-trips a fully-populated NodeInfo" $
      fromCapnpNodeInfo (toCapnpNodeInfo sampleNodeInfo) `shouldBe` Right sampleNodeInfo

    it "round-trips a NodeInfo with all stats Nothing" $
      let ni =
            sampleNodeInfo
              { PN.noiCpuCount = Nothing
              , PN.noiRamTotal = Nothing
              , PN.noiRamFree = Nothing
              , PN.noiStorageBytesTotal = Nothing
              , PN.noiStorageBytesFree = Nothing
              , PN.noiLoadAvg1 = Nothing
              , PN.noiLastNodeAgentPushAt = Nothing
              , PN.noiLastNetAgentPushAt = Nothing
              }
       in fromCapnpNodeInfo (toCapnpNodeInfo ni) `shouldBe` Right ni

  describe "Wire.Node.NodeDetails" $ do
    it "round-trips a fully-populated NodeDetails" $
      fromCapnpNodeDetails (toCapnpNodeDetails sampleNodeDetails)
        `shouldBe` Right sampleNodeDetails

    it "drops an empty description to Nothing (sentinel mapping)" $ do
      -- The wire stores Maybe Text as "" for Nothing; a fresh
      -- '' on the way back must come out as Nothing — exactly
      -- the property the schema documents.
      let nd = sampleNodeDetails {PN.nodDescription = Nothing}
      fromCapnpNodeDetails (toCapnpNodeDetails nd) `shouldBe` Right nd

  describe "Wire.Task.TaskInfo" $ do
    it "round-trips a fully-populated TaskInfo" $
      fromCapnpTaskInfo (toCapnpTaskInfo sampleTaskInfo) `shouldBe` Right sampleTaskInfo

    it "round-trips a TaskInfo without a parent or finished-at" $
      let ti =
            sampleTaskInfo
              { PT.tiParentId = Nothing
              , PT.tiFinishedAt = Nothing
              , PT.tiEntity = Nothing
              , PT.tiMessage = Nothing
              }
       in fromCapnpTaskInfo (toCapnpTaskInfo ti) `shouldBe` Right ti

  describe "Wire.Vm.VmStats" $ do
    it "round-trips a fully-populated VmStats sample" $
      fromCapnpVmStats (toCapnpVmStats sampleVmStats)
        `shouldBe` sampleVmStats

    it "round-trips a stopped-VM (zero) sample" $
      fromCapnpVmStats (toCapnpVmStats PV.zeroVmStats)
        `shouldBe` PV.zeroVmStats

-- | A fixed UTCTime that survives the nanos→ps→nanos round-trip
-- exactly: posix-second granularity, no sub-second tail.
sampleUtc :: UTCTime
sampleUtc = posixSecondsToUTCTime 1700000000

-- | A finished-at one second after sampleUtc, for tasks.
sampleUtc1 :: UTCTime
sampleUtc1 = addUTCTime (secondsToNominalDiffTime 1) sampleUtc

sampleNodeInfo :: PN.NodeInfo
sampleNodeInfo =
  PN.NodeInfo
    { PN.noiId = 7
    , PN.noiName = "alpha"
    , PN.noiHost = "10.0.0.1"
    , PN.noiNodeAgentPort = 9878
    , PN.noiNetAgentPort = 9877
    , PN.noiAdminState = M.NodeOnline
    , PN.noiCreatedAt = sampleUtc
    , PN.noiCpuCount = Just 8
    , PN.noiRamTotal = Just 34359738368
    , PN.noiRamFree = Just 17179869184
    , PN.noiStorageBytesTotal = Just 1099511627776
    , PN.noiStorageBytesFree = Just 549755813888
    , PN.noiLoadAvg1 = Just 0.42
    , PN.noiLastNodeAgentPushAt = Just sampleUtc1
    , PN.noiLastNetAgentPushAt = Just sampleUtc1
    , PN.noiNetdDisabled = False
    , PN.noiNetdConnected = True
    }

sampleNodeDetails :: PN.NodeDetails
sampleNodeDetails =
  PN.NodeDetails
    { PN.nodId = 7
    , PN.nodName = "alpha"
    , PN.nodHost = "10.0.0.1"
    , PN.nodNodeAgentPort = 9878
    , PN.nodNetAgentPort = 9877
    , PN.nodBasePath = "/var/lib/corvus"
    , PN.nodDescription = Just "head node"
    , PN.nodAdminState = M.NodeOnline
    , PN.nodCreatedAt = sampleUtc
    , PN.nodCpuCount = Just 8
    , PN.nodRamTotal = Just 34359738368
    , PN.nodRamFree = Just 17179869184
    , PN.nodStorageBytesTotal = Just 1099511627776
    , PN.nodStorageBytesFree = Just 549755813888
    , PN.nodLoadAvg1 = Just 0.42
    , PN.nodLoadAvg5 = Just 0.5
    , PN.nodLoadAvg15 = Just 0.6
    , PN.nodKernelRelease = Just "6.6.0"
    , PN.nodAgentVersion = Just "0.10.0.0"
    , PN.nodLastNodeAgentPushAt = Just sampleUtc1
    , PN.nodLastNetAgentPushAt = Just sampleUtc1
    , PN.nodNetdDisabled = False
    , PN.nodNetdConnected = True
    }

sampleTaskInfo :: PT.TaskInfo
sampleTaskInfo =
  PT.TaskInfo
    { PT.tiId = 100
    , PT.tiParentId = Just 99
    , PT.tiStartedAt = sampleUtc
    , PT.tiFinishedAt = Just sampleUtc1
    , PT.tiSubsystem = M.SubVm
    , PT.tiEntity = Just P.NamedRef {P.nrId = 42, P.nrName = "web-1"}
    , PT.tiCommand = "create"
    , PT.tiResult = M.TaskSuccess
    , PT.tiMessage = Just "ok"
    , PT.tiClientName = "alice"
    }

sampleVmStats :: PV.VmStats
sampleVmStats =
  PV.VmStats
    { PV.vstSampledAtNanos = 1700000000000000000
    , PV.vstIntervalMillis = 10000
    , PV.vstCpuJiffiesTotal = 142000
    , PV.vstClkTck = 100
    , PV.vstHostRssBytes = 4031733760
    , PV.vstBalloonActualBytes = 3221225472
    , PV.vstBalloonMaxBytes = 4294967296
    , PV.vstDrives =
        [ PV.DriveIo
            { PV.dioName = "drive0"
            , PV.dioReadBytesTotal = 51768954880
            , PV.dioWriteBytesTotal = 12998545408
            , PV.dioReadOpsTotal = 1240315
            , PV.dioWriteOpsTotal = 332108
            }
        ]
    , PV.vstNets =
        [ PV.NetIo
            { PV.nioTapName = "vmtap0"
            , PV.nioRxBytesTotal = 34025467904
            , PV.nioTxBytesTotal = 4509265920
            }
        ]
    }
