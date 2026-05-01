{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | @crv build@ response data.
module Corvus.Protocol.Build
  ( BuildResult (..)
  , BuildOne (..)
  )
where

import Corvus.Protocol.Aeson (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Per-build result for one entry in a @builds:@ list.
data BuildOne = BuildOne
  { boName :: !Text
  , boArtifactDiskId :: !(Maybe Int64)
  -- ^ Registered disk ID on success
  , boError :: !(Maybe Text)
  -- ^ Error message on failure
  }
  deriving (Eq, Show, Generic, Binary)

-- | Aggregate result returned by a @ReqBuild@ call.
newtype BuildResult = BuildResult
  { brBuilds :: [BuildOne]
  }
  deriving (Eq, Show, Generic, Binary)

instance ToJSON BuildOne where
  toJSON = genericToJSON innerOptions

instance ToJSON BuildResult where
  toJSON = genericToJSON innerOptions
