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
  , BuildOptions (..)
  , defaultBuildOptions

    -- * Handlers
  , runBuildPipeline

    -- * Streaming sink
  , BuildSink

    -- * Shell command assembly (exported for tests)
  , buildShellCommand
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Streams as CGS
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import Corvus.Action
import qualified Corvus.Build.Cache.Hash as H
import qualified Corvus.Build.Cache.Store as CStore
import Corvus.Handlers.Apply (ApplyAction (..))
import Corvus.Handlers.Build.Artifact
  ( checkIfExistsPreBake
  , compactDisk
  , deleteOverwriteTargetIfNeeded
  , publishArtifact
  , publishArtifactByClone
  )
import qualified Corvus.Handlers.Build.Cache as Cache
import Corvus.Handlers.Build.CacheResume
  ( resolveCachedArtifactDiskId
  , resumeMemoryCacheBakeVm
  , runFromCachedBakeVm
  , strategyCacheRoles
  )
import Corvus.Handlers.Build.Cleanup (CleanupStack, newCleanupStack, push, withCleanup)
import Corvus.Handlers.Build.CleanupBakeVm (cleanupBakeVm)
import Corvus.Handlers.Build.Installer
  ( runBootKeys
  , runInstallerPhase
  , waitForBakeVmShutdown
  )
import Corvus.Handlers.Build.Provisioner
  ( buildShellCommand
  , provDesc
  , provKind
  , runProvisioner
  , runProvisioners
  , strategyName
  )
import Corvus.Handlers.Build.Qga
  ( agentGuestExec
  , agentGuestExecWithStdin
  , agentGuestExecWithTail
  , agentGuestPing
  )
import Corvus.Handlers.Build.Run
  ( runBakeAndPublish
  , runProvisionersStopAndPublish
  )
import Corvus.Handlers.Build.Template
  ( instantiateBakeVm
  , resolveTemplateAndValidate
  , resolveTemplateIdOrErr
  , sanitizeNameFragment
  , setupTargetDisk
  )
import Corvus.Handlers.Disk.Agent
  ( cloneImageViaAgent
  , getImageSizeMbViaAgent
  , guestSetTimeViaAgent
  , loadSnapshotViaAgentWithVmstate
  , rebaseImageViaAgent
  )
import Corvus.Handlers.Disk.Attach (DiskAttach (..), DiskDetachByDisk (..))
import Corvus.Handlers.Disk.Create (DiskCreate (..))
import Corvus.Handlers.Disk.Db (listDiskImageNodes, recordDiskImageNode)
import Corvus.Handlers.Disk.Maintenance (DiskDelete (..))
import Corvus.Handlers.Disk.Path (makeRelativeToBase, resolveDiskFilePathPure, resolveDiskPath)
import Corvus.Handlers.Disk.Rebase (DiskRebase (..))
import Corvus.Handlers.Resolve (validateName)
import Corvus.Handlers.Scheduler (pickNodeForExistingDisk)
import Corvus.Handlers.Template (TemplateInstantiate (..))
import Corvus.Handlers.Vm.Db (hasNetdMediatedNetIf, setVmError, setVmStatus)
import Corvus.Handlers.Vm.Delete (VmDelete (..))
import Corvus.Handlers.Vm.Lifecycle (VmStop (..))
import Corvus.Handlers.Vm.Query (getVmDetails)
import Corvus.Handlers.Vm.Start (VmStart (..))
import Corvus.Model
import qualified Corvus.Model as M
import Corvus.Node.GuestAgent (GuestExecResult (..))
import Corvus.Node.Image (ImageResult (..))
import Corvus.Node.Qmp (QmpResult (..), qmpSendKey)
import qualified Corvus.Node.VmSpec as VS
import qualified Corvus.NodeAgentClient as NOA
import qualified Corvus.NodeAgentClient.Spec as NSpec
import Corvus.NodeRouting (withVmNodeAgent)
import Corvus.Protocol
import Corvus.Protocol.Build (BuildSink)
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Rpc.Streams (newLineBufferSink)
import Corvus.Schema.Build
import Corvus.Types
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.List
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Version as Version
import Data.Word (Word32)
import Data.Yaml (decodeEither')
import Database.Persist
import Database.Persist.Sql (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import Paths_corvus (version)
import System.Directory (copyFile, createDirectoryIfMissing, removeDirectory, removeFile, renameFile)
import System.FilePath (takeDirectory, (</>))

--------------------------------------------------------------------------------
-- Streaming sink
--------------------------------------------------------------------------------

-- | Discard all events. Used when the operator did not request a live
-- stream — the full build still records subtasks and per-step messages
-- in the database, just nothing is pushed over the wire.
noOpBuildSink :: BuildSink
noOpBuildSink _ = pure ()

-- | Runtime options for @daemon.build@.
data BuildAction = BuildAction
  { baYaml :: !Text
  , baOptions :: !BuildOptions
  }

instance Action BuildAction where
  actionSubsystem _ = SubBuild
  actionCommand _ = "build"
  actionExecute ctx a = handleBuildExecute (acState ctx) (acTaskId ctx) (baYaml a) (baOptions a)

--------------------------------------------------------------------------------
-- Pipeline entry points
--------------------------------------------------------------------------------

-- | Action-driven entry point. Runs the build with no streaming sink,
-- producing a 'RespBuildResult' (or 'RespError') the same way it
-- always has. Used for @--wait=false@ (forked from
-- 'runActionAsyncWithId') and for any caller that doesn't want events.
handleBuildExecute :: ServerState -> TaskId -> Text -> BuildOptions -> IO Response
handleBuildExecute state parentTaskId =
  runBuildPipeline state parentTaskId noOpBuildSink

-- | Run a build pipeline, sending events to the supplied sink. Returns
-- the same response shape as before. Per-step 'BuildEnd' events are
-- emitted by 'runPipelineStep'; the caller is responsible for
-- emitting the terminating 'PipelineEnd' once the response is in hand.
runBuildPipeline :: ServerState -> TaskId -> BuildSink -> Text -> BuildOptions -> IO Response
runBuildPipeline state parentTaskId sink yamlContent opts = runServerLogging state $ do
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
          results <- runPipelineSteps state parentTaskId sink opts (pcSteps config)
          pure $ RespBuildResult (BuildResult results)

-- | Iterate over pipeline steps in order. A step that fails aborts the
-- pipeline (no rollback of prior successful steps); the per-step
-- result is appended either way so the caller sees how far we got.
runPipelineSteps
  :: ServerState
  -> TaskId
  -> BuildSink
  -> BuildOptions
  -> [PipelineStep]
  -> LoggingT IO [BuildOne]
runPipelineSteps _ _ _ _ [] = pure []
runPipelineSteps state parentTaskId sink opts (s : rest) = do
  -- Cooperative cancellation checkpoint: a `crv task cancel` on the
  -- build stops the pipeline here rather than launching the next step.
  liftIO $ throwIfCancelled (mkActionContext state parentTaskId "system")
  one <- runPipelineStep state parentTaskId sink opts s
  case boError one of
    Just _ -> pure [one]
    Nothing -> (one :) <$> runPipelineSteps state parentTaskId sink opts rest

-- | Dispatch a single 'PipelineStep' to the build orchestrator or the
-- apply handler, then collapse the result into 'BuildOne' shape so a
-- pipeline with mixed steps still produces a uniform 'BuildResult'.
runPipelineStep :: ServerState -> TaskId -> BuildSink -> BuildOptions -> PipelineStep -> LoggingT IO BuildOne
runPipelineStep state parentTaskId sink opts step = case step of
  PipelineBuild b -> runOneBuildLogged state parentTaskId sink opts b
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

runOneBuildLogged :: ServerState -> TaskId -> BuildSink -> BuildOptions -> Build -> LoggingT IO BuildOne
runOneBuildLogged state parentTaskId sink opts b = do
  logInfoN $ "Starting build: " <> buildName b
  liftIO $ sink (BuildLogLine ("starting build: " <> buildName b))
  result <- runOneBuild state parentTaskId sink opts b
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
-- down on failure. The artifact disk's destructor is detached on success.
runOneBuild :: ServerState -> TaskId -> BuildSink -> BuildOptions -> Build -> LoggingT IO (Either Text Int64)
runOneBuild state parentTaskId sink opts b = do
  startTime <- liftIO getCurrentTime
  stack <- liftIO newCleanupStack
  let effectiveOpts = mergeBuildOptions opts b
  outcome <- withCleanup (buildCleanup b) stack (runOneBuildBody state parentTaskId sink stack startTime effectiveOpts b)
  case outcome of
    Right inner -> pure inner
    Left ex -> pure $ Left $ "exception: " <> T.pack (show ex)

-- | OR the request-time cache flags against the build YAML's own
-- flags. The CLI-side knobs are an opt-in; the YAML can already say
-- "use cache" and the CLI can layer on "build cache too" without
-- editing the file. 'boRebuildFrom' is purely runtime; the YAML
-- doesn't carry it.
mergeBuildOptions :: BuildOptions -> Build -> BuildOptions
mergeBuildOptions opts b =
  BuildOptions
    { boUseCache = boUseCache opts || buildUseCache b
    , boBuildCache = boBuildCache opts || buildBuildCache b
    , boRebuildFrom = boRebuildFrom opts
    }

runOneBuildBody
  :: ServerState
  -> TaskId
  -> BuildSink
  -> CleanupStack
  -> UTCTime
  -> BuildOptions
  -> Build
  -> LoggingT IO (Either Text Int64)
runOneBuildBody state parentTaskId sink stack startTime opts b = do
  let target = buildTarget b

  -- 0. Pre-bake target.ifExists check. Decide all three policies before
  --    spinning up the bake VM so we never bake just to discover at
  --    publish time that the target name was a problem.
  preBake <- checkIfExistsPreBake state (buildName b) target
  case preBake of
    Left err -> pure $ Left err
    Right (Just existingId) -> pure $ Right existingId
    Right Nothing -> runOneBuildBodyAfterPreBake state parentTaskId sink stack startTime opts b

-- | The original 'runOneBuildBody'. Renamed so the pre-bake
-- ifExists check can short-circuit cleanly without nesting the
-- whole bake pipeline inside another @case@.
runOneBuildBodyAfterPreBake
  :: ServerState
  -> TaskId
  -> BuildSink
  -> CleanupStack
  -> UTCTime
  -> BuildOptions
  -> Build
  -> LoggingT IO (Either Text Int64)
runOneBuildBodyAfterPreBake state parentTaskId sink stack startTime opts b = do
  let target = buildTarget b
      strategy = buildStrategy b
      pipelineKey = H.envelopeHash b <> ":" <> buildName b
      chains = map snd (H.chainHashes b)

  -- Cache lookup. Installer strategy never caches (no provisioners),
  -- so the lookup is short-circuited there.
  cacheRes <-
    if boUseCache opts && strategy /= BuildStrategyInstaller
      then liftIO $ CStore.lookupCachePrefix state pipelineKey chains (strategyCacheRoles strategy)
      else pure (CStore.CacheLookup 0 Nothing Nothing)

  let cappedK = case boRebuildFrom opts of
        0 -> CStore.clPrefix cacheRes
        n -> min (CStore.clPrefix cacheRes) (max 0 (n - 1))

  case (cappedK > 0, CStore.clVmId cacheRes, CStore.clChainHashOfPrefix cacheRes) of
    (True, Just cachedVmKey, Just prefixHash) ->
      runFromCachedBakeVm
        state
        parentTaskId
        sink
        stack
        startTime
        opts
        b
        cappedK
        chains
        (fromSqlKey cachedVmKey)
        prefixHash
    _ -> runFreshBake state parentTaskId sink stack startTime opts b

-- | The fresh-bake path: instantiate a new bake VM, set up the
-- target disk, then hand off to 'runBakeAndPublish'.
-- Lifted out of 'runOneBuildBodyAfterPreBake' so the cache-resumed
-- path doesn't have to share a single deeply-nested case block.
runFreshBake
  :: ServerState
  -> TaskId
  -> BuildSink
  -> CleanupStack
  -> UTCTime
  -> BuildOptions
  -> Build
  -> LoggingT IO (Either Text Int64)
runFreshBake state parentTaskId sink stack startTime opts b = do
  -- No prefix matched (or --use-cache was off). Any rows still on
  -- file under this pipeline key are stale orphans from a previous
  -- run that no longer shares a chain — drop them before we start
  -- writing new ones. Gated on the *effective* build-cache flag
  -- ('mergeBuildOptions' has already OR'd the YAML field with the
  -- CLI's @--build-cache@); using the YAML field directly skips
  -- the prune for the common @--build-cache@-on-the-CLI case where
  -- the YAML still says nothing about it, and the stale tail
  -- accumulates one snapshot per rebuild.
  when (boBuildCache opts) $
    Cache.pruneCacheTail
      state
      (H.envelopeHash b <> ":" <> buildName b)
      0
  let prefix = "__build_" <> T.pack (show (fromSqlKey parentTaskId)) <> "_"
      bakeVmName = prefix <> sanitizeNameFragment (buildName b) <> "-vm"
      targetTmpName = prefix <> sanitizeNameFragment (buildName b) <> "-target"
      target = buildTarget b
      strategy = buildStrategy b
  tplR <- resolveTemplateAndValidate state strategy (buildTemplate b)
  case tplR of
    Left err -> pure $ Left err
    Right templateId -> do
      vmR <- instantiateBakeVm state parentTaskId stack templateId bakeVmName (buildNode b)
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
                startTime
                opts
                1
                b

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
