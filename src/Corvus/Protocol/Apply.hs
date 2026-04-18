{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | @crv apply@ response data.
module Corvus.Protocol.Apply
  ( ApplyCreated (..)
  , ApplyResult (..)
  )
where

import Corvus.Protocol.Aeson (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
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
  toJSON = genericToJSON innerOptions

instance ToJSON ApplyResult where
  toJSON = genericToJSON innerOptions
