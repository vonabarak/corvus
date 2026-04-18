{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Task history response data.
module Corvus.Protocol.Task
  ( TaskInfo (..)
  )
where

import Corvus.Model (TaskResult, TaskSubsystem)
import Corvus.Protocol.Aeson (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Information about a task history entry
data TaskInfo = TaskInfo
  { tiId :: !Int64
  , tiParentId :: !(Maybe Int64)
  , tiStartedAt :: !UTCTime
  , tiFinishedAt :: !(Maybe UTCTime)
  , tiSubsystem :: !TaskSubsystem
  , tiEntityId :: !(Maybe Int)
  , tiEntityName :: !(Maybe Text)
  , tiCommand :: !Text
  , tiResult :: !TaskResult
  , tiMessage :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, Binary)

instance ToJSON TaskInfo where
  toJSON = genericToJSON innerOptions
