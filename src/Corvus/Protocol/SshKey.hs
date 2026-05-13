{-# LANGUAGE DeriveGeneric #-}

-- | SSH key response data.
module Corvus.Protocol.SshKey
  ( SshKeyInfo (..)
  )
where

import Corvus.Protocol.JsonOptions (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
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
  deriving (Eq, Show, Generic)

instance ToJSON SshKeyInfo where
  toJSON = genericToJSON innerOptions
