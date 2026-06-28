{-# LANGUAGE DeriveGeneric #-}

-- | Task history response data.
module Corvus.Protocol.Task
  ( TaskInfo (..)
  )
where

import Corvus.Model (TaskResult, TaskSubsystem)
import Corvus.Protocol.JsonOptions (innerOptions)
import Corvus.Protocol.NamedRef (NamedRef)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Information about a task history entry
data TaskInfo = TaskInfo
  { tiId :: !Int64
  , tiParentId :: !(Maybe Int64)
  -- ^ Parent task id — flat, NOT a 'NamedRef'. Tasks don't carry a
  -- human-readable name field, so there's nothing to nest. See
  -- @AGENTS.md ## Project Rules / Cross-entity references@.
  , tiStartedAt :: !UTCTime
  , tiFinishedAt :: !(Maybe UTCTime)
  , tiSubsystem :: !TaskSubsystem
  , tiEntity :: !(Maybe NamedRef)
  -- ^ Subject of the task (VM / disk / etc. being acted on).
  -- 'Nothing' for daemon-level tasks that don't act on a specific
  -- entity (e.g. startup / shutdown).
  , tiCommand :: !Text
  , tiResult :: !TaskResult
  , tiMessage :: !(Maybe Text)
  , tiClientName :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON TaskInfo where
  toJSON = genericToJSON innerOptions
