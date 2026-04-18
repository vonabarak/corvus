{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | SSH key response data.
module Corvus.Protocol.SshKey
  ( SshKeyInfo (..)
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

-- | SSH key info
data SshKeyInfo = SshKeyInfo
  { skiId :: !Int64
  , skiName :: !Text
  , skiPublicKey :: !Text
  , skiCreatedAt :: !UTCTime
  , skiAttachedVms :: ![(Int64, Text)]
  -- ^ VM (ID, name) pairs this key is attached to
  }
  deriving (Eq, Show, Generic, Binary)

instance ToJSON SshKeyInfo where
  toJSON = genericToJSON innerOptions
