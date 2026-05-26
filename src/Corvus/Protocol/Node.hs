{-# LANGUAGE DeriveGeneric #-}

-- | Node response data. Mirrors the on-the-wire 'NodeInfo' /
-- 'NodeDetails' structs in @schema/node.capnp@.
module Corvus.Protocol.Node
  ( NodeInfo (..)
  , NodeDetails (..)
  )
where

import Corvus.Model (NodeAdminState)
import Corvus.Protocol.JsonOptions (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Short node summary surfaced by @crv node list@.
--
-- Capacity fields are @Maybe@ because a freshly-added node hasn't
-- pushed its first stats snapshot yet — clients render those as
-- @--@ in the table.
data NodeInfo = NodeInfo
  { noiId :: !Int64
  , noiName :: !Text
  , noiHost :: !Text
  , noiNodeAgentPort :: !Int
  , noiNetAgentPort :: !Int
  , noiAdminState :: !NodeAdminState
  , noiCreatedAt :: !UTCTime
  , noiCpuCount :: !(Maybe Int)
  , noiRamMbTotal :: !(Maybe Int)
  , noiRamMbFree :: !(Maybe Int)
  , noiStorageBytesTotal :: !(Maybe Int)
  , noiStorageBytesFree :: !(Maybe Int)
  , noiLoadAvg1 :: !(Maybe Double)
  , noiLastNodeAgentPushAt :: !(Maybe UTCTime)
  , noiLastNetAgentPushAt :: !(Maybe UTCTime)
  , noiNetdDisabled :: !Bool
  -- ^ Persistent flag: this node operates without a netd agent.
  , noiNetdConnected :: !Bool
  -- ^ Derived live state: daemon currently holds a netd cap for this
  -- node. Always 'False' when 'noiNetdDisabled' is 'True'.
  }
  deriving (Eq, Show, Generic)

instance ToJSON NodeInfo where
  toJSON = genericToJSON innerOptions

-- | Full per-node detail surfaced by @crv node show@.
data NodeDetails = NodeDetails
  { nodId :: !Int64
  , nodName :: !Text
  , nodHost :: !Text
  , nodNodeAgentPort :: !Int
  , nodNetAgentPort :: !Int
  , nodBasePath :: !Text
  , nodDescription :: !(Maybe Text)
  , nodAdminState :: !NodeAdminState
  , nodCreatedAt :: !UTCTime
  , nodCpuCount :: !(Maybe Int)
  , nodRamMbTotal :: !(Maybe Int)
  , nodRamMbFree :: !(Maybe Int)
  , nodStorageBytesTotal :: !(Maybe Int)
  , nodStorageBytesFree :: !(Maybe Int)
  , nodLoadAvg1 :: !(Maybe Double)
  , nodLoadAvg5 :: !(Maybe Double)
  , nodLoadAvg15 :: !(Maybe Double)
  , nodKernelRelease :: !(Maybe Text)
  , nodAgentVersion :: !(Maybe Text)
  , nodLastNodeAgentPushAt :: !(Maybe UTCTime)
  , nodLastNetAgentPushAt :: !(Maybe UTCTime)
  , nodNetdDisabled :: !Bool
  , nodNetdConnected :: !Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON NodeDetails where
  toJSON = genericToJSON innerOptions
