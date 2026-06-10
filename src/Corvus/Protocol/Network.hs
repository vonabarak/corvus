{-# LANGUAGE DeriveGeneric #-}

-- | Virtual network response data.
module Corvus.Protocol.Network
  ( NetworkInfo (..)
  )
where

import Corvus.Protocol.JsonOptions (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Virtual network info
data NetworkInfo = NetworkInfo
  { nwiId :: !Int64
  , nwiName :: !Text
  , nwiSubnet :: !Text
  , nwiDhcp :: !Bool
  , nwiNat :: !Bool
  , nwiRunning :: !Bool
  , nwiDnsmasqPid :: !(Maybe Int)
  , nwiCreatedAt :: !UTCTime
  , nwiAutostart :: !Bool
  , nwiVni :: !(Maybe Int)
  , nwiPeerNodeIds :: ![Int64]
  , nwiDnsServers :: ![Text]
  }
  deriving (Eq, Show, Generic)

instance ToJSON NetworkInfo where
  toJSON = genericToJSON innerOptions
