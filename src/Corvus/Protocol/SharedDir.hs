{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Shared directory (virtiofs) response data.
module Corvus.Protocol.SharedDir
  ( SharedDirInfo (..)
  )
where

import Corvus.Model (SharedDirCache)
import Corvus.Protocol.Aeson (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Shared directory info
data SharedDirInfo = SharedDirInfo
  { sdiId :: !Int64
  , sdiPath :: !Text
  , sdiTag :: !Text
  , sdiCache :: !SharedDirCache
  , sdiReadOnly :: !Bool
  , sdiPid :: !(Maybe Int)
  -- ^ virtiofsd PID if running
  }
  deriving (Eq, Show, Generic, Binary)

instance ToJSON SharedDirInfo where
  toJSON = genericToJSON innerOptions
