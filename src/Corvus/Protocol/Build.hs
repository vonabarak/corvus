{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | @crv build@ response data.
module Corvus.Protocol.Build
  ( BuildResult (..)
  , BuildOne (..)
  , BuildEvent (..)
  )
where

import Corvus.Model (TaskResult)
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

-- | Streaming events emitted on the upgraded socket after the daemon
-- replies with @RespBuildStreamStarted@. Frames use the same
-- length-prefixed binary framing as ordinary protocol messages — see
-- 'Corvus.Protocol.encodeMessage'.
--
-- The stream terminates with exactly one @PipelineEnd@; the daemon
-- closes the socket immediately afterwards.
data BuildEvent
  = -- | Daemon-level progress note (e.g. \"instantiating template\").
    BuildLogLine !Text
  | -- | Provisioner started: 1-based step index, kind tag
    -- (@\"shell\"@\/@\"file\"@\/@\"wait-for\"@\/@\"reboot\"@), and a short human
    -- description (e.g. the file destination, or \"step 3 of 8\").
    StepStart !Int !Text !Text
  | -- | One line of merged stdout+stderr from a step. No trailing
    -- newline. Lines longer than 'streamLineSplitBytes' are split into
    -- multiple events at byte boundaries (with U+FFFD substituted for
    -- invalid UTF-8 bytes).
    StepOutput !Int !Text
  | -- | Provisioner finished. The 'TaskResult' mirrors the subtask row
    -- written to the database; the optional @Text@ carries the failure
    -- message (if any) for inline rendering before the next step.
    StepEnd !Int !TaskResult !(Maybe Text)
  | -- | One build entry finished — error message or artifact disk id.
    BuildEnd !(Either Text Int64)
  | -- | Aggregate pipeline result, identical in shape to the response
    -- the non-streaming path returns. Always the last event.
    PipelineEnd !BuildResult
  deriving (Eq, Show, Generic, Binary)

instance ToJSON BuildEvent where
  toJSON = genericToJSON innerOptions
