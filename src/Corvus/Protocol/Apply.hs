{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | @crv apply@ response data.
module Corvus.Protocol.Apply
  ( ApplyCreated (..)
  , ApplyResult (..)
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A resource created during an apply operation
data ApplyCreated = ApplyCreated
  { acName :: !Text
  , acId :: !Int64
  }
  deriving (Eq, Show, Generic, Binary)

-- | Summary of resources created by an apply operation
data ApplyResult = ApplyResult
  { arSshKeys :: ![ApplyCreated]
  , arDisks :: ![ApplyCreated]
  , arNetworks :: ![ApplyCreated]
  , arVms :: ![ApplyCreated]
  , arTemplates :: ![ApplyCreated]
  }
  deriving (Eq, Show, Generic, Binary)

instance ToJSON ApplyCreated where
  toJSON a =
    object
      [ "name" .= acName a
      , "id" .= acId a
      ]

instance ToJSON ApplyResult where
  toJSON r =
    object
      [ "sshKeys" .= arSshKeys r
      , "disks" .= arDisks r
      , "networks" .= arNetworks r
      , "vms" .= arVms r
      , "templates" .= arTemplates r
      ]
