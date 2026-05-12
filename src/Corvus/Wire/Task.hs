{-# LANGUAGE RecordWildCards #-}

-- | Cap'n Proto conversion for task history info.
module Corvus.Wire.Task
  ( toCapnpTaskInfo
  , fromCapnpTaskInfo
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Task as CGTask
import qualified Corvus.Protocol.Task as P
import Corvus.Wire.Enums (fromCapnpTaskResult, fromCapnpTaskSubsystem, toCapnpTaskResult, toCapnpTaskSubsystem)
import Corvus.Wire.Errors (WireError)
import Corvus.Wire.Time (nanosToUtcTime, nanosToUtcTimeMaybe, utcTimeToNanos, utcTimeToNanosMaybe)
import Data.Maybe (fromMaybe, isJust)

toCapnpTaskInfo :: P.TaskInfo -> C.Parsed CGTask.TaskInfo
toCapnpTaskInfo P.TaskInfo {..} =
  CGTask.TaskInfo
    { CGTask.id = tiId
    , CGTask.parentId = fromMaybe 0 tiParentId
    , CGTask.startedAt = utcTimeToNanos tiStartedAt
    , CGTask.finishedAt = utcTimeToNanosMaybe tiFinishedAt
    , CGTask.subsystem = toCapnpTaskSubsystem tiSubsystem
    , CGTask.entityId = maybe 0 fromIntegral tiEntityId
    , CGTask.entityName = fromMaybe mempty tiEntityName
    , CGTask.command = tiCommand
    , CGTask.result = toCapnpTaskResult tiResult
    , CGTask.message = fromMaybe mempty tiMessage
    }

fromCapnpTaskInfo :: C.Parsed CGTask.TaskInfo -> Either WireError P.TaskInfo
fromCapnpTaskInfo CGTask.TaskInfo {..} = do
  sub <- fromCapnpTaskSubsystem subsystem
  res <- fromCapnpTaskResult result
  pure
    P.TaskInfo
      { P.tiId = id
      , P.tiParentId = if parentId == 0 then Nothing else Just parentId
      , P.tiStartedAt = nanosToUtcTime startedAt
      , P.tiFinishedAt = nanosToUtcTimeMaybe finishedAt
      , P.tiSubsystem = sub
      , P.tiEntityId = if entityId == 0 then Nothing else Just (fromIntegral entityId)
      , P.tiEntityName = if entityName == mempty then Nothing else Just entityName
      , P.tiCommand = command
      , P.tiResult = res
      , P.tiMessage = if message == mempty then Nothing else Just message
      }
