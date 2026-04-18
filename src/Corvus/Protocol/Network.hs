{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Virtual network response data.
module Corvus.Protocol.Network
  ( NetworkInfo (..)
  )
where

import Corvus.Model ()

-- for orphan Binary UTCTime instance
import Corvus.Protocol.Aeson (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Binary (Binary)
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
  }
  deriving (Eq, Show, Generic, Binary)

instance ToJSON NetworkInfo where
  toJSON = genericToJSON innerOptions
