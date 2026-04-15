{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Virtual network response data.
module Corvus.Protocol.Network
  ( NetworkInfo (..)
  )
where

import Corvus.Model ()
-- for orphan Binary UTCTime instance
import Data.Aeson (ToJSON (..), object, (.=))
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
  toJSON n =
    object
      [ "id" .= nwiId n
      , "name" .= nwiName n
      , "subnet" .= nwiSubnet n
      , "dhcp" .= nwiDhcp n
      , "nat" .= nwiNat n
      , "running" .= nwiRunning n
      , "dnsmasqPid" .= nwiDnsmasqPid n
      , "createdAt" .= nwiCreatedAt n
      , "autostart" .= nwiAutostart n
      ]
