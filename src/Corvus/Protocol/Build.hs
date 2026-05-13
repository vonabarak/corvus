{-# LANGUAGE DeriveGeneric #-}

-- | @crv build@ response data.
module Corvus.Protocol.Build
  ( BuildResult (..)
  , BuildOne (..)
  , BuildEvent (..)
  )
where

import Corvus.Model (TaskResult)
import Corvus.Protocol.JsonOptions (innerOptions)
import Data.Aeson (ToJSON (..), genericToJSON)
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
  deriving (Eq, Show, Generic)

-- | Aggregate result returned by a @ReqBuild@ call.
newtype BuildResult = BuildResult
  { brBuilds :: [BuildOne]
  }
  deriving (Eq, Show, Generic)

instance ToJSON BuildOne where
  toJSON = genericToJSON innerOptions

instance ToJSON BuildResult where
  toJSON = genericToJSON innerOptions

-- | Streaming events emitted by the build pipeline. Originally a
-- length-prefixed binary stream; Phase 6 will deliver these via a
-- @BuildEventSink@ Cap'n Proto cap. The Haskell ADT is still useful
-- for the in-process pipeline → sink shim.
--
-- The stream terminates with exactly one @PipelineEnd@.
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
  deriving (Eq, Show, Generic)

instance ToJSON BuildEvent where
  toJSON = genericToJSON innerOptions
