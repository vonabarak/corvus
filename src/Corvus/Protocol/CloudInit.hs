{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Cloud-init response data returned by the daemon.
--
-- Used by both the VM subsystem (as a field in 'VmDetails') and the
-- template subsystem (as a field in 'TemplateDetails'), so it lives on
-- its own to avoid either side importing the other.
module Corvus.Protocol.CloudInit
  ( CloudInitInfo (..)
  )
where

import Corvus.Protocol.Aeson (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Cloud-init configuration info
data CloudInitInfo = CloudInitInfo
  { ciiUserData :: !(Maybe Text)
  , ciiNetworkConfig :: !(Maybe Text)
  , ciiInjectSshKeys :: !Bool
  }
  deriving (Eq, Show, Generic, Binary)

instance ToJSON CloudInitInfo where
  toJSON = genericToJSON innerOptions
