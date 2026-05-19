{-# LANGUAGE RecordWildCards #-}

-- | Cap'n Proto conversion for nodes (the host-orchestration unit
-- introduced by the multi-node refactor).
module Corvus.Wire.Node
  ( toCapnpNodeInfo
  , fromCapnpNodeInfo
  , toCapnpNodeDetails
  , fromCapnpNodeDetails
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Node as CGNode
import qualified Corvus.Protocol.Node as P
import Corvus.Wire.Enums (fromCapnpNodeAdminState, toCapnpNodeAdminState)
import Corvus.Wire.Errors (WireError)
import Corvus.Wire.Time (nanosToUtcTime, nanosToUtcTimeMaybe, utcTimeToNanos, utcTimeToNanosMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

-- ---------------------------------------------------------------------
-- NodeInfo
-- ---------------------------------------------------------------------

toCapnpNodeInfo :: P.NodeInfo -> C.Parsed CGNode.NodeInfo
toCapnpNodeInfo P.NodeInfo {..} =
  CGNode.NodeInfo
    { CGNode.id = noiId
    , CGNode.name = noiName
    , CGNode.host = noiHost
    , CGNode.nodeAgentPort = fromIntegral noiNodeAgentPort
    , CGNode.netAgentPort = fromIntegral noiNetAgentPort
    , CGNode.adminState = toCapnpNodeAdminState noiAdminState
    , CGNode.createdAt = utcTimeToNanos noiCreatedAt
    , CGNode.cpuCount = maybe 0 fromIntegral noiCpuCount
    , CGNode.ramMbTotal = maybe 0 fromIntegral noiRamMbTotal
    , CGNode.ramMbFree = maybe 0 fromIntegral noiRamMbFree
    , CGNode.storageBytesTotal = maybe 0 fromIntegral noiStorageBytesTotal
    , CGNode.storageBytesFree = maybe 0 fromIntegral noiStorageBytesFree
    , CGNode.loadAvg1 = fromMaybe 0 noiLoadAvg1
    , CGNode.lastNodeAgentPushAt = utcTimeToNanosMaybe noiLastNodeAgentPushAt
    , CGNode.lastNetAgentPushAt = utcTimeToNanosMaybe noiLastNetAgentPushAt
    }

fromCapnpNodeInfo :: C.Parsed CGNode.NodeInfo -> Either WireError P.NodeInfo
fromCapnpNodeInfo CGNode.NodeInfo {..} = do
  st <- fromCapnpNodeAdminState adminState
  pure
    P.NodeInfo
      { P.noiId = id
      , P.noiName = name
      , P.noiHost = host
      , P.noiNodeAgentPort = fromIntegral nodeAgentPort
      , P.noiNetAgentPort = fromIntegral netAgentPort
      , P.noiAdminState = st
      , P.noiCreatedAt = nanosToUtcTime createdAt
      , P.noiCpuCount = nonZero cpuCount
      , P.noiRamMbTotal = nonZero ramMbTotal
      , P.noiRamMbFree = nonZero ramMbFree
      , P.noiStorageBytesTotal = nonZeroInt64 storageBytesTotal
      , P.noiStorageBytesFree = nonZeroInt64 storageBytesFree
      , P.noiLoadAvg1 = if loadAvg1 == 0 then Nothing else Just loadAvg1
      , P.noiLastNodeAgentPushAt = nanosToUtcTimeMaybe lastNodeAgentPushAt
      , P.noiLastNetAgentPushAt = nanosToUtcTimeMaybe lastNetAgentPushAt
      }

-- ---------------------------------------------------------------------
-- NodeDetails
-- ---------------------------------------------------------------------

toCapnpNodeDetails :: P.NodeDetails -> C.Parsed CGNode.NodeDetails
toCapnpNodeDetails P.NodeDetails {..} =
  CGNode.NodeDetails
    { CGNode.id = nodId
    , CGNode.name = nodName
    , CGNode.host = nodHost
    , CGNode.nodeAgentPort = fromIntegral nodNodeAgentPort
    , CGNode.netAgentPort = fromIntegral nodNetAgentPort
    , CGNode.basePath = nodBasePath
    , CGNode.description = fromMaybe mempty nodDescription
    , CGNode.adminState = toCapnpNodeAdminState nodAdminState
    , CGNode.createdAt = utcTimeToNanos nodCreatedAt
    , CGNode.cpuCount = maybe 0 fromIntegral nodCpuCount
    , CGNode.ramMbTotal = maybe 0 fromIntegral nodRamMbTotal
    , CGNode.ramMbFree = maybe 0 fromIntegral nodRamMbFree
    , CGNode.storageBytesTotal = maybe 0 fromIntegral nodStorageBytesTotal
    , CGNode.storageBytesFree = maybe 0 fromIntegral nodStorageBytesFree
    , CGNode.loadAvg1 = fromMaybe 0 nodLoadAvg1
    , CGNode.loadAvg5 = fromMaybe 0 nodLoadAvg5
    , CGNode.loadAvg15 = fromMaybe 0 nodLoadAvg15
    , CGNode.kernelRelease = fromMaybe mempty nodKernelRelease
    , CGNode.agentVersion = fromMaybe mempty nodAgentVersion
    , CGNode.lastNodeAgentPushAt = utcTimeToNanosMaybe nodLastNodeAgentPushAt
    , CGNode.lastNetAgentPushAt = utcTimeToNanosMaybe nodLastNetAgentPushAt
    }

fromCapnpNodeDetails :: C.Parsed CGNode.NodeDetails -> Either WireError P.NodeDetails
fromCapnpNodeDetails CGNode.NodeDetails {..} = do
  st <- fromCapnpNodeAdminState adminState
  pure
    P.NodeDetails
      { P.nodId = id
      , P.nodName = name
      , P.nodHost = host
      , P.nodNodeAgentPort = fromIntegral nodeAgentPort
      , P.nodNetAgentPort = fromIntegral netAgentPort
      , P.nodBasePath = basePath
      , P.nodDescription = emptyToNothing description
      , P.nodAdminState = st
      , P.nodCreatedAt = nanosToUtcTime createdAt
      , P.nodCpuCount = nonZero cpuCount
      , P.nodRamMbTotal = nonZero ramMbTotal
      , P.nodRamMbFree = nonZero ramMbFree
      , P.nodStorageBytesTotal = nonZeroInt64 storageBytesTotal
      , P.nodStorageBytesFree = nonZeroInt64 storageBytesFree
      , P.nodLoadAvg1 = if loadAvg1 == 0 then Nothing else Just loadAvg1
      , P.nodLoadAvg5 = if loadAvg5 == 0 then Nothing else Just loadAvg5
      , P.nodLoadAvg15 = if loadAvg15 == 0 then Nothing else Just loadAvg15
      , P.nodKernelRelease = emptyToNothing kernelRelease
      , P.nodAgentVersion = emptyToNothing agentVersion
      , P.nodLastNodeAgentPushAt = nanosToUtcTimeMaybe lastNodeAgentPushAt
      , P.nodLastNetAgentPushAt = nanosToUtcTimeMaybe lastNetAgentPushAt
      }

-- ---------------------------------------------------------------------
-- Sentinels
-- ---------------------------------------------------------------------

-- | Treat 0 as "not yet reported" for an unsigned counter the agent
-- pushes from @/proc/meminfo@ / @statvfs@. Doesn't roundtrip a
-- genuine zero (e.g. a node that legitimately has 0 free RAM) — but
-- such a node wouldn't be schedulable anyway, so the lossy mapping
-- is acceptable in v1.
nonZero :: (Integral a) => a -> Maybe Int
nonZero 0 = Nothing
nonZero n = Just (fromIntegral n)

nonZeroInt64 :: (Integral a) => a -> Maybe Int
nonZeroInt64 = nonZero

emptyToNothing :: Text -> Maybe Text
emptyToNothing t
  | t == mempty = Nothing
  | otherwise = Just t
