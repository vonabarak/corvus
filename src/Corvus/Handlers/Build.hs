{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Image-bake orchestration for @crv build@.
--
-- A build pipeline is procedural: instantiate a template, run a sequence of
-- in-VM provisioners against it, capture one of its disks as a registered
-- Corvus image, and tear down everything else. The single entry point is
-- 'BuildAction'; per-Build helpers run inline so the entire pipeline is one
-- task with many subtasks (template instantiate, vm start, vm stop, …).
--
-- Cleanup of ephemeral resources is delegated to "Corvus.Handlers.Build.Cleanup".
-- Each created resource pushes a destructor onto the stack as soon as it
-- exists; on success or failure the stack is drained according to the
-- @cleanup:@ mode in the YAML.
module Corvus.Handlers.Build
  ( -- * Action
    BuildAction (..)

    -- * Handlers
  , runBuildPipeline

    -- * Streaming sink
  , BuildSink

    -- * Shell command assembly (exported for tests)
  , buildShellCommand
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import Corvus.Action
import qualified Corvus.Build.Identity as H
import Corvus.Handlers.Apply.Execute (ApplyAction (..))
import Corvus.Handlers.Build.Artifact (checkBuildUpdate, checkIfExistsPreBake)
import Corvus.Handlers.Build.Cleanup (CleanupStack, newCleanupStack, withCleanup)
import Corvus.Handlers.Build.Provisioner (buildShellCommand)
import Corvus.Handlers.Build.Run (runBakeAndPublish)
import Corvus.Handlers.Build.Template (instantiateBakeVm, resolveTemplateAndValidate, sanitizeNameFragment, setupTargetDisk)
import Corvus.Handlers.Resolve (validateName)
import Corvus.Handlers.Template (getTemplateDetails)
import Corvus.Model
import Corvus.Protocol
import Corvus.Schema.Build
import Corvus.Types
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml (decodeEither')
import Database.Persist
import Database.Persist.Sql (runSqlPool)

--------------------------------------------------------------------------------
-- Streaming sink
--------------------------------------------------------------------------------

-- | Discard all events. Used when the operator did not request a live
-- stream — the full build still records subtasks and per-step messages
-- in the database, just nothing is pushed over the wire.
noOpBuildSink :: BuildSink
noOpBuildSink _ = pure ()

-- | Input for @daemon.build@.
newtype BuildAction = BuildAction
  { baYaml :: Text
  }

instance Action BuildAction where
  actionSubsystem _ = SubBuild
  actionCommand _ = "build"
  actionExecute ctx a = handleBuildExecute (acState ctx) (acTaskId ctx) (baYaml a)

--------------------------------------------------------------------------------
-- Pipeline entry points
--------------------------------------------------------------------------------

-- | Action-driven entry point. Runs the build with no streaming sink,
-- producing a 'RespBuildResult' (or 'RespError') the same way it
-- always has. Used for @--wait=false@ (forked from
-- 'runActionAsyncWithId') and for any caller that doesn't want events.
handleBuildExecute :: ServerState -> TaskId -> Text -> IO Response
handleBuildExecute state parentTaskId =
  runBuildPipeline state parentTaskId noOpBuildSink

-- | Run a build pipeline, sending events to the supplied sink. Returns
-- the same response shape as before. Per-step 'BuildEnd' events are
-- emitted by 'runPipelineStep'; the caller is responsible for
-- emitting the terminating 'PipelineEnd' once the response is in hand.
runBuildPipeline :: ServerState -> TaskId -> BuildSink -> Text -> IO Response
runBuildPipeline state parentTaskId sink yamlContent = runServerLogging state $ do
  case decodeEither' (TE.encodeUtf8 yamlContent) of
    Left err -> do
      let msg = T.pack (show err)
      logWarnN $ "Failed to parse pipeline YAML: " <> msg
      pure $ RespError msg
    Right (config :: PipelineConfig) ->
      case validateConfig config of
        Left err -> do
          logWarnN $ "Pipeline config validation failed: " <> err
          pure $ RespError err
        Right () -> do
          results <- runPipelineSteps state parentTaskId sink (pcSteps config)
          pure $ RespBuildResult (BuildResult results)

-- | Iterate over pipeline steps in order. A step that fails aborts the
-- pipeline (no rollback of prior successful steps); the per-step
-- result is appended either way so the caller sees how far we got.
runPipelineSteps
  :: ServerState
  -> TaskId
  -> BuildSink
  -> [PipelineStep]
  -> LoggingT IO [BuildOne]
runPipelineSteps _ _ _ [] = pure []
runPipelineSteps state parentTaskId sink (s : rest) = do
  -- Cooperative cancellation checkpoint: a `crv task cancel` on the
  -- build stops the pipeline here rather than launching the next step.
  liftIO $ throwIfCancelled (mkActionContext state parentTaskId "system")
  one <- runPipelineStep state parentTaskId sink s
  case boError one of
    Just _ -> pure [one]
    Nothing -> (one :) <$> runPipelineSteps state parentTaskId sink rest

-- | Dispatch a single 'PipelineStep' to the build orchestrator or the
-- apply handler, then collapse the result into 'BuildOne' shape so a
-- pipeline with mixed steps still produces a uniform 'BuildResult'.
runPipelineStep :: ServerState -> TaskId -> BuildSink -> PipelineStep -> LoggingT IO BuildOne
runPipelineStep state parentTaskId sink step = case step of
  PipelineBuild b -> runOneBuildLogged state parentTaskId sink b
  PipelineApply cfg -> do
    logInfoN "Applying environment configuration (pipeline step)"
    liftIO $ sink (BuildLogLine "applying environment configuration")
    let applySink :: ApplySink
        applySink ev = case renderApplyEventForBuild ev of
          Nothing -> pure ()
          Just txt -> sink (BuildLogLine txt)
        ctx = (mkActionContext state parentTaskId "system") {acApplySink = applySink}
    resp <- liftIO $ runActionAsSubtask ctx (ApplyAction cfg False)
    let one = case resp of
          RespApplyResult _ ->
            BuildOne {boName = "apply", boArtifactDiskId = Nothing, boError = Nothing}
          RespError err ->
            BuildOne {boName = "apply", boArtifactDiskId = Nothing, boError = Just err}
          other ->
            BuildOne
              { boName = "apply"
              , boArtifactDiskId = Nothing
              , boError = Just $ "apply: unexpected response: " <> T.pack (show other)
              }
    case boError one of
      Just err -> do
        logWarnN $ "apply step failed: " <> err
        liftIO $ sink (BuildEnd (Left err))
      Nothing -> do
        logInfoN "apply step completed"
        liftIO $ sink (BuildEnd (Right 0))
    pure one
  PipelineUpload _ ->
    pure $
      BuildOne
        { boName = "upload"
        , boArtifactDiskId = Nothing
        , boError = Just "upload steps must be processed by crv build before submission"
        }

-- | Project an 'ApplyEvent' to a human-readable build log line.
-- 'Nothing' suppresses the event (used for noisy intermediate
-- variants we don't want to log inside a build). 'ApplyEnd' is
-- suppressed because the build step's outer 'BuildEnd' already
-- conveys success/failure.
renderApplyEventForBuild :: ApplyEvent -> Maybe Text
renderApplyEventForBuild = \case
  ApplyLogLine t -> Just t
  PhaseStart phase total ->
    Just $ "[apply/" <> phase <> "] creating " <> T.pack (show total)
  EntityStart phase name kind ->
    Just $ "[apply/" <> phase <> "] " <> kind <> " " <> name <> ": starting"
  EntityEnd phase name TaskSuccess _ eid
    | eid > 0 ->
        Just $ "[apply/" <> phase <> "] " <> name <> ": ok (id " <> T.pack (show eid) <> ")"
    | otherwise -> Just $ "[apply/" <> phase <> "] " <> name <> ": ok"
  EntityEnd phase name result msg _ ->
    Just $
      "[apply/"
        <> phase
        <> "] "
        <> name
        <> ": "
        <> enumToText result
        <> (if T.null msg then "" else " - " <> msg)
  DownloadStart name url ->
    Just $ "[apply/disks] downloading " <> name <> " from " <> url
  -- Per-progress events are rate-limited at the source (250ms in
  -- the node agent); pass them through verbatim so the build log
  -- shows a moving counter without spamming.
  DownloadProgress {} -> Nothing
  DownloadEnd name True _ ->
    Just $ "[apply/disks] download complete: " <> name
  DownloadEnd name False errMsg ->
    Just $ "[apply/disks] download failed: " <> name <> " — " <> errMsg
  ApplyEnd {} -> Nothing

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validateConfig :: PipelineConfig -> Either Text ()
validateConfig cfg = do
  let names = [buildName b | PipelineBuild b <- pcSteps cfg]
  case findDuplicate names of
    Just d -> Left $ "Duplicate build name: " <> d
    Nothing -> pure ()
  mapM_ validateStep (pcSteps cfg)
  where
    findDuplicate :: [Text] -> Maybe Text
    findDuplicate [] = Nothing
    findDuplicate (x : xs)
      | x `elem` xs = Just x
      | otherwise = findDuplicate xs

    validateStep (PipelineBuild b) = validateBuild b
    -- Apply documents are validated inside the apply handler when the
    -- step actually runs; pre-running validation would duplicate that
    -- logic and risk drift.
    validateStep (PipelineApply _) = Right ()
    validateStep (PipelineUpload _) = Left "upload steps must be processed by crv build before submission"

validateBuild :: Build -> Either Text ()
validateBuild b = do
  validateName "Build" (buildName b)
  mapM_ (validateProvisioner (buildName b)) (buildProvisioners b)

validateProvisioner :: Text -> Provisioner -> Either Text ()
validateProvisioner buildLbl p = case p of
  ProvShell sh ->
    case (shellInline sh, shellScript sh) of
      (Just _, Nothing) -> Right ()
      (Nothing, Just _) ->
        Left $
          "Build '"
            <> buildLbl
            <> "': shell.script must be inlined by the client before sending. Run "
            <> "`crv build` with the client-side preprocessor."
      (Just _, Just _) ->
        Left $ "Build '" <> buildLbl <> "': shell may not have both inline and script"
      (Nothing, Nothing) ->
        Left $ "Build '" <> buildLbl <> "': shell needs either inline or script"
  ProvFile fp ->
    case (fileFrom fp, fileContentBase64 fp) of
      (Nothing, Just _) -> Right ()
      (Just _, Nothing) ->
        Left $
          "Build '"
            <> buildLbl
            <> "': file.from must be inlined by the client as file.content before sending"
      (Just _, Just _) ->
        Left $ "Build '" <> buildLbl <> "': file may not have both from and content"
      (Nothing, Nothing) ->
        Left $ "Build '" <> buildLbl <> "': file needs either from or content"
  _ -> Right ()

--------------------------------------------------------------------------------
-- Single-build orchestration
--------------------------------------------------------------------------------

runOneBuildLogged :: ServerState -> TaskId -> BuildSink -> Build -> LoggingT IO BuildOne
runOneBuildLogged state parentTaskId sink b = do
  logInfoN $ "Starting build: " <> buildName b
  liftIO $ sink (BuildLogLine ("starting build: " <> buildName b))
  result <- runOneBuild state parentTaskId sink b
  case result of
    Right diskId -> do
      logInfoN $ "Build '" <> buildName b <> "' completed; artifact disk #" <> T.pack (show diskId)
      liftIO $ sink (BuildEnd (Right diskId))
      pure
        BuildOne
          { boName = buildName b
          , boArtifactDiskId = Just diskId
          , boError = Nothing
          }
    Left err -> do
      logWarnN $ "Build '" <> buildName b <> "' failed: " <> err
      liftIO $ sink (BuildEnd (Left err))
      pure
        BuildOne
          { boName = buildName b
          , boArtifactDiskId = Nothing
          , boError = Just err
          }

-- | Run a single build, returning the published artifact disk id or an error.
-- The build's @cleanup:@ mode controls whether ephemeral resources are torn
-- down on failure. Published artifacts are preserved independently.
runOneBuild :: ServerState -> TaskId -> BuildSink -> Build -> LoggingT IO (Either Text Int64)
runOneBuild state parentTaskId sink b = do
  stack <- liftIO newCleanupStack
  outcome <- withCleanup (buildCleanup b) stack (runOneBuildBody state parentTaskId sink stack b)
  case outcome of
    Right inner -> pure inner
    Left ex -> pure $ Left $ "exception: " <> T.pack (show ex)

runOneBuildBody
  :: ServerState
  -> TaskId
  -> BuildSink
  -> CleanupStack
  -> Build
  -> LoggingT IO (Either Text Int64)
runOneBuildBody state parentTaskId sink stack b = do
  let target = buildTarget b

  -- Error and skip retain their fast path. Update requires the resolved
  -- template and its source versions before deciding whether to bake.
  preBake <- checkIfExistsPreBake state (buildName b) target
  case preBake of
    Left err -> pure $ Left err
    Right (Just existingId) -> pure $ Right existingId
    Right Nothing -> do
      snapshot <-
        liftIO $
          runSqlPool
            ( do
                template <- getBy (UniqueTemplateVmName (buildTemplate b))
                case template of
                  Nothing -> pure Nothing
                  Just (Entity key _) -> do
                    details <- getTemplateDetails key
                    case details of
                      Nothing -> pure Nothing
                      Just resolved -> do
                        keys <- mapM (get . toSqlKey . tvskiId) (tvdSshKeys resolved)
                        pure (Just (resolved, map (fmap sshKeyPublicKey) keys))
            )
            (ssDbPool state)
      case snapshot of
        Nothing -> pure (Left "build template not found")
        Just (details, keys)
          | any (\d -> tvdiCloneStrategy d /= StrategyCreate && isNothing (tvdiDiskImage d)) (tvdDrives details) -> pure (Left "build template source image not found")
          | any isNothing keys -> pure (Left "build template SSH key not found")
          | otherwise -> do
              let resolved = b {buildResolvedTemplate = Just details}
                  identity = H.buildInputIdentity resolved (map (fromMaybe "") keys)
                  captured = resolved {buildIdentity = Just identity}
              validation <- resolveTemplateAndValidate captured
              case validation of
                Left err -> pure (Left err)
                Right _ -> do
                  existing <-
                    if btIfExists target == BuildIfExistsUpdate
                      then checkBuildUpdate state sink (buildName b) identity
                      else pure Nothing
                  case existing of
                    Just key -> pure (Right key)
                    Nothing -> runFreshBake state parentTaskId sink stack captured

runFreshBake
  :: ServerState
  -> TaskId
  -> BuildSink
  -> CleanupStack
  -> Build
  -> LoggingT IO (Either Text Int64)
runFreshBake state parentTaskId sink stack b = do
  let prefix = "__build_" <> T.pack (show (fromSqlKey parentTaskId)) <> "_"
      bakeVmName = prefix <> sanitizeNameFragment (buildName b) <> "-vm"
      targetTmpName = prefix <> sanitizeNameFragment (buildName b) <> "-target"
      target = buildTarget b
      strategy = buildStrategy b
  tplR <- resolveTemplateAndValidate b
  case tplR of
    Left err -> pure $ Left err
    Right templateId -> do
      vmR <- instantiateBakeVm state parentTaskId stack (buildResolvedTemplate b) templateId bakeVmName (buildNode b)
      case vmR of
        Left err -> pure $ Left err
        Right vmIdLong -> do
          tgtR <- setupTargetDisk state parentTaskId stack vmIdLong strategy target targetTmpName (buildNode b)
          case tgtR of
            Left err -> pure $ Left err
            Right (artifactDiskId, needFlatten) ->
              runBakeAndPublish
                state
                parentTaskId
                sink
                vmIdLong
                strategy
                artifactDiskId
                target
                needFlatten
                b
