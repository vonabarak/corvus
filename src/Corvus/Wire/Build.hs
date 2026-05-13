{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Conversion between the in-process 'BuildEvent' ADT and the
-- Cap'n Proto @BuildEvent@ struct used on the wire by
-- @daemon.build@'s 'BuildEventSink'.
module Corvus.Wire.Build
  ( toCapnpBuildEvent
  , toCapnpBuildOneResult
  , fromCapnpBuildEvent
  , fromCapnpBuildOneResult
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Streams as CGS
import Corvus.Protocol.Build (BuildEvent (..), BuildOne (..), BuildResult (..))
import Corvus.Wire.Enums (fromCapnpTaskResult, toCapnpTaskResult)
import Corvus.Wire.Errors (WireError (..))
import qualified Data.Maybe
import qualified Data.Text as T

-- | Encode a Haskell 'BuildEvent' as its Cap'n Proto wire form.
toCapnpBuildEvent :: BuildEvent -> C.Parsed CGS.BuildEvent
toCapnpBuildEvent ev =
  CGS.BuildEvent {CGS.union' = encodeUnion ev}
  where
    encodeUnion = \case
      BuildLogLine t ->
        CGS.BuildEvent'logLine t
      StepStart idx kind msg ->
        CGS.BuildEvent'stepStart
          CGS.BuildEvent'stepStart'
            { CGS.stepIndex = fromIntegral idx
            , CGS.name = kind
            , CGS.command = msg
            }
      StepOutput idx line ->
        CGS.BuildEvent'stepOutput
          CGS.BuildEvent'stepOutput'
            { CGS.stepIndex = fromIntegral idx
            , CGS.line = line
            }
      StepEnd idx result mMsg ->
        CGS.BuildEvent'stepEnd
          CGS.BuildEvent'stepEnd'
            { CGS.stepIndex = fromIntegral idx
            , CGS.result = toCapnpTaskResult result
            , CGS.message = Data.Maybe.fromMaybe "" mMsg
            }
      BuildEnd (Left err) ->
        CGS.BuildEvent'buildEnd
          CGS.BuildEvent'buildEnd'
            { CGS.success = False
            , CGS.errorMessage = err
            , CGS.artifactDiskId = 0
            }
      BuildEnd (Right diskId) ->
        CGS.BuildEvent'buildEnd
          CGS.BuildEvent'buildEnd'
            { CGS.success = True
            , CGS.errorMessage = T.empty
            , CGS.artifactDiskId = diskId
            }
      PipelineEnd (BuildResult builds) ->
        CGS.BuildEvent'pipelineEnd
          CGS.BuildEvent'pipelineEnd'
            { CGS.builds = map toCapnpBuildOneResult builds
            }

-- | Encode a single per-build result.
toCapnpBuildOneResult :: BuildOne -> C.Parsed CGS.BuildOneResult
toCapnpBuildOneResult bo =
  CGS.BuildOneResult
    { CGS.name = boName bo
    , CGS.artifactDiskId = Data.Maybe.fromMaybe 0 (boArtifactDiskId bo)
    , CGS.errorMessage = Data.Maybe.fromMaybe T.empty (boError bo)
    }

-- | Decode a wire 'BuildEvent' back into the Haskell ADT used by
-- the in-process build pipeline.
fromCapnpBuildEvent :: C.Parsed CGS.BuildEvent -> Either WireError BuildEvent
fromCapnpBuildEvent CGS.BuildEvent {CGS.union' = u} = case u of
  CGS.BuildEvent'logLine t ->
    Right (BuildLogLine t)
  CGS.BuildEvent'stepStart CGS.BuildEvent'stepStart' {CGS.stepIndex, CGS.name, CGS.command} ->
    Right (StepStart (fromIntegral stepIndex) name command)
  CGS.BuildEvent'stepOutput CGS.BuildEvent'stepOutput' {CGS.stepIndex, CGS.line} ->
    Right (StepOutput (fromIntegral stepIndex) line)
  CGS.BuildEvent'stepEnd CGS.BuildEvent'stepEnd' {CGS.stepIndex, CGS.result, CGS.message} -> do
    tr <- fromCapnpTaskResult result
    let msg = if T.null message then Nothing else Just message
    Right (StepEnd (fromIntegral stepIndex) tr msg)
  CGS.BuildEvent'buildEnd CGS.BuildEvent'buildEnd' {CGS.success, CGS.errorMessage, CGS.artifactDiskId} ->
    if success
      then Right (BuildEnd (Right artifactDiskId))
      else Right (BuildEnd (Left errorMessage))
  CGS.BuildEvent'pipelineEnd CGS.BuildEvent'pipelineEnd' {CGS.builds} -> do
    bs <- traverse fromCapnpBuildOneResult builds
    Right (PipelineEnd (BuildResult bs))
  CGS.BuildEvent'unknown' _ ->
    Left (WireMissingUnionVariant "BuildEvent")

-- | Decode a wire 'BuildOneResult'.
fromCapnpBuildOneResult :: C.Parsed CGS.BuildOneResult -> Either WireError BuildOne
fromCapnpBuildOneResult CGS.BuildOneResult {CGS.name, CGS.artifactDiskId, CGS.errorMessage} =
  Right
    BuildOne
      { boName = name
      , boArtifactDiskId = if artifactDiskId == 0 then Nothing else Just artifactDiskId
      , boError = if T.null errorMessage then Nothing else Just errorMessage
      }
