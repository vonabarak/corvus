{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Corvus.Handlers.Build.Cache as Cache
import Corvus.Handlers.Build.Cleanup (CleanupStack, newCleanupStack, push, withCleanup)
import Corvus.Handlers.Build.Floppy (buildFloppyImage)
import Corvus.Handlers.Disk (DiskCreate (..), DiskDelete (..), DiskRebase (..))
import Corvus.Handlers.Disk.Agent
  ( cloneImageViaAgent
  , getImageSizeMbViaAgent
  , guestSetTimeViaAgent
  , loadSnapshotViaAgentWithVmstate
  , rebaseImageViaAgent
  )
import Corvus.Handlers.Disk.Attach (DiskAttach (..), DiskDetachByDisk (..))
import Corvus.Handlers.Disk.Db (listDiskImageNodes, recordDiskImageNode)
import Corvus.Handlers.Disk.Path (makeRelativeToBase, resolveDiskFilePathPure, resolveDiskPath)
import Corvus.Handlers.Resolve (validateName)
import Corvus.Handlers.Scheduler (pickNodeForExistingDisk)
import Corvus.Handlers.Template (TemplateInstantiate (..))
import Corvus.Handlers.Vm
  ( VmDelete (..)
  , VmStart (..)
  , VmStop (..)
  , getVmDetails
  , hasNetdMediatedNetIf
  , setVmError
  , setVmStatus
  )
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
import Corvus.Qemu.Config (getEffectiveBasePath)
import Corvus.Rpc.Streams (newLineBufferSink)
import Corvus.Schema.Build
import Corvus.Types
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
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
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, toSqlKey)
import Paths_corvus (version)
import System.Directory (copyFile, createDirectoryIfMissing, removeDirectory, removeFile, renameFile)
import System.FilePath (takeDirectory, (</>))

--------------------------------------------------------------------------------
-- Agent-routed QGA helpers
--
-- All guest-exec / guest-ping calls during a bake-VM build go
-- through @nodeagent.vmGuestExec@ (the agent owns the QGA
-- socket). These helpers wrap the underlying RPC and preserve
-- the daemon-side 'GuestExecResult' shape so the bake-pipeline
-- code reads the same.
--
-- Trade-off (Phase 4): @agentGuestExecWithTail@ no longer
-- streams stdout line-by-line as QGA chunks arrive — the agent's
-- vmGuestExec aggregates and returns the full output on exit.
-- Build provisioner output therefore appears at end-of-step
-- instead of live. A streaming RPC variant
-- (@vmGuestExecStream(req, sink)@) is the natural follow-up.
--------------------------------------------------------------------------------

-- | One-shot guest exec via @nodeagent.vmGuestExec@.
agentGuestExec :: ServerState -> Int64 -> Text -> Word32 -> IO GuestExecResult
agentGuestExec state vmId cmd =
  agentGuestExecCore state vmId cmd BS.empty

-- | Guest exec with a stdin payload.
agentGuestExecWithStdin
  :: ServerState
  -> Int64
  -> Text
  -> BS.ByteString
  -> Word32
  -> IO GuestExecResult
agentGuestExecWithStdin = agentGuestExecCore

-- | Trivial liveness probe — run @/bin/true@; success iff exit 0.
-- Drop-in replacement for the old QGA @guest-ping@.
agentGuestPing :: ServerState -> Int64 -> IO Bool
agentGuestPing state vmId = do
  r <- agentGuestExecCore state vmId "true" BS.empty 5
  pure $ case r of
    GuestExecSuccess 0 _ _ -> True
    _ -> False

-- | Guest exec whose stdout / stderr stream line-by-line through
-- @onLine@ as the guest emits them. Drives the agent's
-- @vmGuestExecStream@ RPC: we export two 'LineBufferSink' caps,
-- the agent pushes raw QGA chunks into them as @guest-exec-status@
-- returns, and our line buffer fires @onLine@ for each complete
-- line on the way in. The agent calls @sink.end()@ on completion
-- (success or failure) so any trailing partial line gets flushed
-- before this function returns.
--
-- Stdout and stderr each get their own line buffer (lines from
-- the two streams never split mid-line), but the SAME @onLine@
-- callback — so the build sees a single merged event stream,
-- matching the user-visible UX of 0.10's @exec 2>&1@ log file.
agentGuestExecWithTail
  :: ServerState
  -> Int64
  -> Text
  -> Word32
  -> (Text -> IO ())
  -> IO GuestExecResult
agentGuestExecWithTail state vmId cmd timeoutSec onLine = do
  let req =
        VS.VmGuestExecReq
          { VS.vgeVmId = vmId
          , VS.vgePath = cmd
          , VS.vgeArgs = []
          , VS.vgeCaptureOutput = True
          , VS.vgeInputData = BS.empty
          , VS.vgeTimeoutSec = timeoutSec
          }
  outer <- withVmNodeAgent state vmId $ \nac -> do
    let sup = NOA.nacSupervisor nac
    stdoutSink <- newLineBufferSink onLine
    stderrSink <- newLineBufferSink onLine
    stdoutCap <- C.export @CGS.ByteSink sup stdoutSink
    stderrCap <- C.export @CGS.ByteSink sup stderrSink
    NOA.vmGuestExecStream nac req stdoutCap stderrCap
  case outer of
    Left err -> pure (GuestExecConnectionFailed err)
    Right r -> case r of
      Left e ->
        pure (GuestExecError ("vmGuestExecStream: " <> T.pack (show e)))
      Right info
        | VS.vgiHasExit info ->
            -- Bytes already flowed through 'onLine'. Stdout /
            -- stderr fields are empty by design (the agent's
            -- streaming path doesn't echo them back), so don't
            -- forward them to the caller — pass 'T.empty'.
            pure $
              GuestExecSuccess
                (fromIntegral (VS.vgiExitCode info))
                T.empty
                T.empty
        | otherwise ->
            let stderrText =
                  TE.decodeUtf8With lenientDecode (VS.vgiStderr info)
                msg
                  | T.null stderrText =
                      "guest-exec did not return an exit code"
                  | otherwise = stderrText
             in pure (GuestExecError msg)

-- | Common body for the four wrappers above.
agentGuestExecCore
  :: ServerState
  -> Int64
  -> Text
  -> BS.ByteString
  -> Word32
  -> IO GuestExecResult
agentGuestExecCore state vmId cmd stdinPayload timeoutSec = do
  -- Send the bare command. The agent's 'guestExec' detects
  -- the guest OS and wraps with @/bin/sh -c@ (Linux/BSD) or
  -- @cmd.exe /c@ (Windows). Pre-wrapping here would double-
  -- wrap on Windows.
  let req =
        VS.VmGuestExecReq
          { VS.vgeVmId = vmId
          , VS.vgePath = cmd
          , VS.vgeArgs = []
          , VS.vgeCaptureOutput = True
          , VS.vgeInputData = stdinPayload
          , VS.vgeTimeoutSec = timeoutSec
          }
  outer <- withVmNodeAgent state vmId $ \nac -> NOA.vmGuestExec nac req
  case outer of
    Left err -> pure (GuestExecConnectionFailed err)
    Right r -> case r of
      Left e ->
        pure (GuestExecError ("vmGuestExec: " <> T.pack (show e)))
      Right info
        | VS.vgiHasExit info ->
            pure $
              GuestExecSuccess
                (fromIntegral (VS.vgiExitCode info))
                (TE.decodeUtf8With lenientDecode (VS.vgiStdout info))
                (TE.decodeUtf8With lenientDecode (VS.vgiStderr info))
        | otherwise ->
            -- hasExit=False: forward the agent's stderr (QGA
            -- timeout, connection error, …) so the failure is
            -- actually diagnosable in build output.
            let stderrText =
                  TE.decodeUtf8With lenientDecode (VS.vgiStderr info)
                msg
                  | T.null stderrText =
                      "guest-exec did not return an exit code"
                  | otherwise = stderrText
             in pure (GuestExecError msg)

--------------------------------------------------------------------------------
-- Top-level Action
--------------------------------------------------------------------------------

-- | Runtime options for @daemon.build@. None of these belong to the
-- per-build YAML schema directly: 'boUseCache' and 'boBuildCache' OR
-- with the build's YAML-level flags so an operator can enable caching
-- on a per-invocation basis without editing the YAML, and
-- 'boRebuildFrom' is purely a CLI knob (1-based step index that caps
-- the matched prefix at @K = min K (rebuildFrom - 1)@; 0 means unset).
data BuildOptions = BuildOptions
  { boUseCache :: !Bool
  , boBuildCache :: !Bool
  , boRebuildFrom :: !Int
  }
  deriving (Eq, Show)

defaultBuildOptions :: BuildOptions
defaultBuildOptions = BuildOptions False False 0

data BuildAction = BuildAction
  { baYaml :: !Text
  , baOptions :: !BuildOptions
  }

instance Action BuildAction where
  actionSubsystem _ = SubBuild
  actionCommand _ = "build"
  actionExecute ctx a = handleBuildExecute (acState ctx) (acTaskId ctx) (baYaml a) (baOptions a)

--------------------------------------------------------------------------------
-- Streaming sink
--------------------------------------------------------------------------------

-- | Receives 'BuildEvent's as the build runs. The non-streaming code
-- path uses 'noOpBuildSink'; @--wait@ wires this up to encode events
-- onto the upgraded socket (see "Corvus.Server").
type BuildSink = BuildEvent -> IO ()

-- | Discard all events. Used when the operator did not request a live
-- stream — the full build still records subtasks and per-step messages
-- in the database, just nothing is pushed over the wire.
noOpBuildSink :: BuildSink
noOpBuildSink _ = pure ()

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

validateBuild :: Build -> Either Text ()
validateBuild b = do
  validateName "Build" (buildName b)
  mapM_ (validateProvisioner (buildName b)) (buildProvisioners b)
  validateFloppy (buildName b) (buildFloppy b)

validateFloppy :: Text -> Maybe Floppy -> Either Text ()
validateFloppy _ Nothing = Right ()
validateFloppy buildLbl (Just f) = case (floppyFrom f, floppyContentBase64 f) of
  (Nothing, Just _) -> Right ()
  (Just _, Nothing) ->
    Left $
      "Build '"
        <> buildLbl
        <> "': floppy.from must be inlined by the client as floppy.contentBase64 before sending"
  (Just _, Just _) ->
    Left $ "Build '" <> buildLbl <> "': floppy may not have both from and contentBase64"
  (Nothing, Nothing) ->
    Left $ "Build '" <> buildLbl <> "': floppy needs either from or contentBase64"

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
-- target disk, attach the floppy, then hand off to 'runBakeAndPublish'.
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
            Right (artifactDiskId, needFlatten) -> do
              floppyR <- attachBuildFloppy state parentTaskId stack vmIdLong prefix b
              case floppyR of
                Left err -> pure $ Left err
                Right () ->
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

-- | The cache-resumed path: take the bake VM the prior @--build-cache@
-- run left behind, stop it (offline rollback needs an unlocked qcow2),
-- roll its writable disks back to step @K@'s snapshot, push a
-- 'cleanupBakeVm' destructor on the stack (which will skip the
-- @VmDelete@ thanks to the surviving cache rows), then hand off to
-- 'runBakeAndPublish' with @startStep = K + 1@. The cached VM
-- already has its target disk attached and its floppy (if any) in
-- place from the priming run; we don't re-do that setup.
runFromCachedBakeVm
  :: ServerState
  -> TaskId
  -> BuildSink
  -> CleanupStack
  -> UTCTime
  -> BuildOptions
  -> Build
  -> Int
  -- ^ matched prefix length @K@
  -> [Text]
  -- ^ chain hashes (left-to-right)
  -> Int64
  -- ^ cached bake VM id
  -> Text
  -- ^ chain hash at step K (the snapshot to roll back to)
  -> LoggingT IO (Either Text Int64)
runFromCachedBakeVm state parentTaskId sink stack startTime opts b k chains cachedVmId prefixHash = do
  setupRes <- prepareCacheReuse
  case setupRes of
    Right (artifactDiskId, alreadyRunning) -> do
      liftIO $ sink (StepCacheRestore k prefixHash)
      mapM_
        (\(i, h) -> liftIO $ sink (StepCacheHit i h))
        (zip [1 .. k] (take k chains))
      if alreadyRunning
        then
          -- Memory-mode resume: the bake VM is already up and
          -- running with vmstate restored. Skip the VmStart in
          -- 'runBakeAndPublish' and continue directly with the
          -- provisioner tail.
          case buildStrategy b of
            BuildStrategyInstaller ->
              -- Installer strategy doesn't cache (strategyCacheRoles
              -- returns []), so we should never reach here. Defensive:
              -- fall back to the standard path which will detect the
              -- inconsistency.
              runBakeAndPublish
                state
                parentTaskId
                sink
                cachedVmId
                (buildStrategy b)
                artifactDiskId
                (buildTarget b)
                False
                startTime
                opts
                (k + 1)
                b
            _ ->
              runProvisionersStopAndPublish
                state
                parentTaskId
                sink
                cachedVmId
                artifactDiskId
                (buildTarget b)
                False
                startTime
                opts
                (k + 1)
                b
        else
          runBakeAndPublish
            state
            parentTaskId
            sink
            cachedVmId
            (buildStrategy b)
            artifactDiskId
            (buildTarget b)
            False
            startTime
            opts
            (k + 1)
            b
    Left reason -> do
      -- Cache reuse setup failed (cached VM gone, snapshot gone,
      -- rollback error, no writable disks left, …). Purge the stale
      -- cache rows that pointed at this bake VM so the next build
      -- doesn't repeat the same mistake, then fall through to a
      -- fresh bake. The user-visible event stream notes the fallback
      -- so the operator understands why their cached prefix wasn't
      -- used. Only SETUP failures fall back; once we hand off to
      -- runBakeAndPublish, any downstream failure is reported as-is.
      logWarnN $ "cache: reuse failed — falling back to a fresh bake: " <> reason
      liftIO $ sink (BuildLogLine ("cache: reuse failed (" <> reason <> "); rebuilding from scratch"))
      liftIO $ CStore.purgeCacheRowsForVm state cachedVmId
      runFreshBake state parentTaskId sink stack startTime opts b
  where
    -- Returns @Right (artifactDiskId, alreadyRunning)@ where
    -- @alreadyRunning = True@ for the memory-mode path (the
    -- snapshot-load lifecycle already brought the bake VM up and
    -- the caller must skip its own VmStart). @Left reason@ means
    -- cache setup failed.
    prepareCacheReuse = do
      -- Verify the cached VM still exists. The Vm row could have
      -- been deleted by an operator's `crv vm delete` between the
      -- previous bake and now; the FK cascade in 'deleteVm' should
      -- have dropped the cache rows too, but we double-check.
      mVm <-
        liftIO $
          runSqlPool
            (get (toSqlKey cachedVmId :: VmId))
            (ssDbPool state)
      case mVm of
        Nothing -> pure (Left "cached bake VM has been deleted")
        Just vm -> stopAndPrepare vm

    stopAndPrepare vm = do
      -- Both modes need the bake VM stopped at this point: disk
      -- mode does an offline qcow2 rollback that takes the file
      -- lock; memory mode launches a fresh QEMU process with
      -- `-S`. VmStop on an already-stopped VM returns
      -- RespInvalidTransition, which classifyStopResp surfaces as
      -- Left, so skip the call when state is already VmStopped.
      stopR <-
        if vmStatus vm == VmStopped
          then pure (Right ())
          else do
            resp <-
              liftIO $
                runActionAsSubtask
                  (mkActionContext state parentTaskId "system")
                  (VmStop cachedVmId 300)
            pure (classifyStopResp resp)
      case stopR of
        Left err -> pure (Left ("stop cached bake VM: " <> err))
        Right () -> dispatchOnMode

    dispatchOnMode = do
      artifactRes <- liftIO $ resolveCachedArtifactDiskId state cachedVmId prefixHash
      case artifactRes of
        Left err -> pure (Left err)
        Right artifactDiskId -> do
          dr <- Cache.writableCacheDisks state cachedVmId artifactDiskId
          case dr of
            Left err -> pure (Left ("enumerate disks: " <> err))
            Right disks
              | null disks ->
                  pure (Left "cached bake VM has no writable disks left")
              | otherwise -> do
                  -- Push the destructor BEFORE the mode-specific
                  -- work so a partial failure still gets the bake VM
                  -- reaped by the standard cleanup pass.
                  liftIO $
                    push
                      stack
                      "bake-vm-cached"
                      (cleanupBakeVm state parentTaskId cachedVmId)
                  case buildCacheMode b of
                    CacheModeDisk -> do
                      rbR <-
                        Cache.rollbackToCachedStep
                          state
                          disks
                          prefixHash
                      case rbR of
                        Left err -> pure (Left err)
                        Right () -> pure (Right (artifactDiskId, False))
                    CacheModeMemory -> do
                      lifecycleR <-
                        resumeMemoryCacheBakeVm
                          state
                          cachedVmId
                          disks
                          prefixHash
                      case lifecycleR of
                        Left err -> pure (Left err)
                        Right () -> pure (Right (artifactDiskId, True))

-- | Memory-mode cache resume lifecycle: launch the cached bake VM
-- paused (QEMU @-S@), restore vmstate via QMP @snapshot-load@,
-- @cont@ to unfreeze, resync the wall clock via QGA
-- @guest-set-time@, then mark the DB row 'VmRunning'.
--
-- Bypasses the @VmStart@ Action — that action commits @VmStarting@
-- and then waits for the first QGA ping, which can't land before
-- @snapshot-load@ runs. We talk to the agent's vmStart RPC
-- directly with @vsStartPaused = True@ so the agent skips its
-- own GA-wait fork.
--
-- The pre-bake artifact disk is the vmstate carrier; the
-- @CacheDisk@ enumeration already filters to writable qcow2
-- drives, and the disk tagged @"artifact"@ holds the RAM dump.
resumeMemoryCacheBakeVm
  :: ServerState
  -> Int64
  -- ^ cached bake VM id (must be stopped)
  -> [Cache.CacheDisk]
  -- ^ writable cache-worthy disks; must include role="artifact"
  -> Text
  -- ^ chainHash of step K (the snapshot tag to load)
  -> LoggingT IO (Either Text ())
resumeMemoryCacheBakeVm state cachedVmId disks chain = do
  let pool = ssDbPool state
      cfg = ssQemuConfig state
      snapName = H.cacheSnapshotName chain
  case Data.List.find (\d -> Cache.cdRole d == "artifact") disks of
    Nothing ->
      pure $
        Left "memory-mode resume: no artifact-role disk in the cache set"
    Just carrier -> do
      mVm <- liftIO $ runSqlPool (get (toSqlKey cachedVmId :: VmId)) pool
      case mVm of
        Nothing -> pure $ Left "cached bake VM disappeared mid-resume"
        Just vm -> do
          needsNetd <- liftIO $ runSqlPool (hasNetdMediatedNetIf cachedVmId) pool
          mNetAgent <- liftIO $ lookupNetAgentMaybe state (M.vmNodeId vm)
          when (needsNetd && isNothing mNetAgent) $
            logWarnN $
              "memory-mode resume: VM "
                <> T.pack (show cachedVmId)
                <> " has a managed NIC but netd is unavailable; the lifecycle will probably fail"
          let netAgentForSpec = if needsNetd then mNetAgent else Nothing
              -- Cache resume re-uses the bake VM. The QGA wait
              -- happens AFTER our snapshot-load + cont, so we
              -- inherit the 90-second steady-state budget the
              -- non-cloud-init path uses. The bake VM has QGA on
              -- by construction (overlay + from-scratch strategies
              -- both require it).
              waitMs = if vmGuestAgent vm then 90000 else 0
          mSpec <-
            liftIO $
              NSpec.assembleVmSpec pool cfg netAgentForSpec cachedVmId waitMs
          case mSpec of
            Left err -> pure $ Left $ "memory-mode resume: assembleVmSpec: " <> err
            Right baseSpec -> do
              let spec = baseSpec {VS.vsStartPaused = True}
                  nodeId = M.vmNodeId vm
                  paths = map Cache.cdFilePath disks
                  carrierPath = Cache.cdFilePath carrier
              -- Pre-commit VmStarting so external observers see
              -- the row leave VmStopped while we drive the
              -- multi-step lifecycle. We flip to VmRunning at the
              -- end.
              liftIO $ runSqlPool (setVmStatus cachedVmId VmStarting) pool
              logInfoN $
                "memory-mode resume: starting bake VM "
                  <> T.pack (show cachedVmId)
                  <> " paused for snapshot-load tag="
                  <> snapName
              startR <-
                liftIO $ withVmNodeAgent state cachedVmId $ \nac -> NOA.vmStart nac spec
              case startR of
                Left err -> do
                  liftIO $ runSqlPool (setVmError cachedVmId err) pool
                  pure $ Left ("memory-mode resume: vmStart paused: " <> err)
                Right (Left e) -> do
                  let msg = "vmStart paused: " <> T.pack (show e)
                  liftIO $ runSqlPool (setVmError cachedVmId msg) pool
                  pure $ Left ("memory-mode resume: " <> msg)
                Right (Right _runtime) -> do
                  -- QEMU is up and paused. Drive snapshot-load.
                  logInfoN $
                    "memory-mode resume: loading vmstate tag="
                      <> snapName
                      <> " into VM "
                      <> T.pack (show cachedVmId)
                  loadRes <-
                    liftIO $
                      loadSnapshotViaAgentWithVmstate
                        state
                        nodeId
                        carrierPath
                        paths
                        snapName
                        cachedVmId
                  case loadRes of
                    ImageSuccess -> do
                      logInfoN "memory-mode resume: snapshot-load OK, issuing cont"
                      resumeR <-
                        liftIO $ withVmNodeAgent state cachedVmId $ \nac ->
                          NOA.vmResume nac cachedVmId
                      case resumeR of
                        Left e -> do
                          let msg = "memory-mode resume: cont (outer): " <> e
                          liftIO $ runSqlPool (setVmError cachedVmId msg) pool
                          pure $ Left msg
                        Right (Left e) -> do
                          let msg =
                                "memory-mode resume: cont: " <> T.pack (show e)
                          liftIO $ runSqlPool (setVmError cachedVmId msg) pool
                          pure $ Left msg
                        Right (Right ()) -> do
                          -- Best-effort clock resync.
                          _ <-
                            liftIO $ guestSetTimeViaAgent state nodeId cachedVmId
                          liftIO $
                            runSqlPool (setVmStatus cachedVmId VmRunning) pool
                          logInfoN $
                            "memory-mode resume: VM "
                              <> T.pack (show cachedVmId)
                              <> " resumed at snapshot state"
                          pure (Right ())
                    ImageFormatNotSupported msg ->
                      finishWithError pool ("snapshot-load: " <> msg)
                    ImageError err ->
                      finishWithError pool ("snapshot-load: " <> err)
                    ImageNotFound ->
                      finishWithError pool "snapshot-load: snapshot not found"
  where
    finishWithError pool msg = do
      liftIO $ runSqlPool (setVmError cachedVmId msg) pool
      pure $ Left ("memory-mode resume: " <> msg)

-- | Disk roles a strategy expects to find in a complete cache step.
strategyCacheRoles :: BuildStrategy -> [Text]
strategyCacheRoles BuildStrategyOverlay = ["artifact"]
strategyCacheRoles BuildStrategyFromScratch = ["artifact", "system"]
strategyCacheRoles BuildStrategyInstaller = []

-- | Walk the cache rows for @(bakeVmId, chainHash)@ and return the
-- 'DiskImage' id behind the row tagged \"artifact\".
resolveCachedArtifactDiskId :: ServerState -> Int64 -> Text -> IO (Either Text Int64)
resolveCachedArtifactDiskId state vmId chain = do
  rows <-
    runSqlPool
      ( selectList
          [ M.BuildCacheEntryVmId ==. toSqlKey vmId
          , M.BuildCacheEntryChainHash ==. chain
          , M.BuildCacheEntryDiskRole ==. ("artifact" :: Text)
          ]
          [LimitTo 1]
      )
      (ssDbPool state)
  case rows of
    (Entity _ row : _) -> do
      mSnap <- runSqlPool (get (M.buildCacheEntrySnapshotId row)) (ssDbPool state)
      case mSnap of
        Just snap -> pure (Right (fromSqlKey (M.snapshotDiskImageId snap)))
        Nothing -> pure (Left "cache row refers to a missing snapshot")
    [] -> pure (Left "no 'artifact' cache row found for the matched prefix")

-- | Phase 1: resolve the build's template by name and enforce the
-- guest-agent precondition. The installer strategy doesn't need QGA
-- (vendor autounattend drives everything); every other strategy does.
resolveTemplateAndValidate
  :: ServerState
  -> BuildStrategy
  -> Text
  -- ^ template name
  -> LoggingT IO (Either Text Int64)
resolveTemplateAndValidate state strategy tplName = do
  r <- liftIO $ resolveTemplateIdOrErr state tplName
  pure $ case r of
    Left err -> Left err
    Right (templateId, hasGuestAgent)
      | strategy /= BuildStrategyInstaller && not hasGuestAgent ->
          Left ("template '" <> tplName <> "' must have guestAgent: true")
      | otherwise -> Right templateId

-- | Phase 2: instantiate the template into a bake VM and register
-- its cleanup destructor immediately so a later failure tears it down.
instantiateBakeVm
  :: ServerState
  -> TaskId
  -> CleanupStack
  -> Int64
  -- ^ template id
  -> Text
  -- ^ bake VM name
  -> Text
  -- ^ node reference
  -> LoggingT IO (Either Text Int64)
instantiateBakeVm state parentTaskId stack templateId bakeVmName nodeRef = do
  resp <-
    liftIO $
      runActionAsSubtask
        (mkActionContext state parentTaskId "system")
        ( TemplateInstantiate
            { tiTemplateId = templateId
            , tiName = bakeVmName
            , tiNodeRef = nodeRef
            }
        )
  case resp of
    RespTemplateInstantiated vmIdLong -> do
      liftIO $
        push stack "bake-vm" $
          cleanupBakeVm state parentTaskId vmIdLong
      pure $ Right vmIdLong
    RespError err -> pure $ Left $ "instantiate template: " <> err
    other ->
      pure $ Left $ "instantiate template: unexpected response: " <> T.pack (show other)

-- | Phase 3: prepare the target disk for the artifact, per strategy.
-- Returns @(artifactDiskId, needFlatten)@:
--
--   * overlay   — the bake VM's first drive is the artifact; the
--                 backing chain must be flattened at publish time.
--   * installer — same, but the drive was freshly created so there
--                 is no chain to flatten.
--   * fromScratch — create a new empty target disk sized from the
--                 build's @target.sizeGb@ and attach it to the bake VM.
setupTargetDisk
  :: ServerState
  -> TaskId
  -> CleanupStack
  -> Int64
  -- ^ bake VM id
  -> BuildStrategy
  -> BuildTarget
  -> Text
  -- ^ ephemeral target name
  -> Text
  -- ^ build's target node (passes through to DiskCreate so the
  -- target lands on the same kernel as the bake VM)
  -> LoggingT IO (Either Text (Int64, Bool))
setupTargetDisk state parentTaskId stack vmIdLong strategy target targetTmpName buildNodeRef = case strategy of
  BuildStrategyOverlay -> firstDriveAsArtifact True
  BuildStrategyInstaller -> firstDriveAsArtifact False
  BuildStrategyFromScratch -> createAndAttachTarget
  where
    firstDriveAsArtifact needFlatten = do
      mDrive <-
        liftIO $
          runSqlPool
            ( selectFirst
                [DriveVmId ==. toSqlKey vmIdLong]
                [Asc DriveId]
            )
            (ssDbPool state)
      case mDrive of
        Nothing -> pure $ Left "instantiated bake VM has no drives"
        Just (Entity _ drv) ->
          pure $ Right (fromSqlKey (driveDiskImageId drv), needFlatten)

    createAndAttachTarget = do
      let sizeMb = fromIntegral (btSizeGb target) * 1024
      -- ephemeral=True for the bake-VM-attached disk; publish
      -- produces a CLONE (non-ephemeral) so the bake disk stays
      -- ephemeral whether the build succeeds or fails. If
      -- @--build-cache@ leaves cache rows behind, 'cleanupBakeVm'
      -- skips the @VmDelete@ and the bake VM + this ephemeral
      -- disk both survive for future cache hits.
      diskResp <-
        liftIO $
          runActionAsSubtask
            (mkActionContext state parentTaskId "system")
            (DiskCreate targetTmpName (btFormat target) sizeMb Nothing True buildNodeRef)
      case diskResp of
        RespDiskCreated diskIdLong -> attachTarget diskIdLong
        RespError err -> pure $ Left $ "create target disk: " <> err
        _ -> pure $ Left "create target disk: unexpected response"

    attachTarget diskIdLong = do
      attachResp <-
        liftIO $
          runActionAsSubtask
            (mkActionContext state parentTaskId "system")
            ( DiskAttach
                vmIdLong
                diskIdLong
                InterfaceVirtio
                Nothing
                False
                False
                CacheWriteback
            )
      case attachResp of
        RespDiskAttached _ -> pure $ Right (diskIdLong, False)
        RespError err -> do
          -- The disk was created but couldn't attach: register a
          -- cleanup so a failed attach doesn't strand it.
          liftIO $
            push stack "orphan-target-disk" $ do
              _ <-
                runActionAsSubtask
                  (mkActionContext state parentTaskId "system")
                  (DiskDelete diskIdLong)
              pure ()
          pure $ Left $ "attach target disk: " <> err
        _ -> pure $ Left "attach target disk: unexpected response"

-- | Phase 4: start the bake VM, run the strategy-specific work
-- (installer wait OR QGA-driven provisioners + clean stop), then
-- publish the artifact.
runBakeAndPublish
  :: ServerState
  -> TaskId
  -> BuildSink
  -> Int64
  -- ^ bake VM id
  -> BuildStrategy
  -> Int64
  -- ^ artifact disk id
  -> BuildTarget
  -> Bool
  -- ^ needs flatten?
  -> UTCTime
  -> BuildOptions
  -> Int
  -- ^ start step (1-based; > 1 when resuming from a cache prefix)
  -> Build
  -> LoggingT IO (Either Text Int64)
runBakeAndPublish state parentTaskId sink vmIdLong strategy artifactDiskId target needFlatten startTime opts startStep b = do
  -- For QGA-driven strategies VmStart blocks until the guest agent
  -- first-pings; for the installer strategy the template has
  -- guestAgent:false so VmStart returns as soon as QEMU is up.
  startResp <-
    liftIO $
      runActionAsSubtask (mkActionContext state parentTaskId "system") (VmStart vmIdLong)
  case classifyStartResp startResp of
    Left err -> pure $ Left $ "start bake VM: " <> err
    Right () -> case strategy of
      BuildStrategyInstaller ->
        runInstallerPhase
          state
          parentTaskId
          sink
          vmIdLong
          artifactDiskId
          target
          needFlatten
          b
      _ ->
        runProvisionersStopAndPublish
          state
          parentTaskId
          sink
          vmIdLong
          artifactDiskId
          target
          needFlatten
          startTime
          opts
          startStep
          b

-- | The non-installer tail of phase 4: run user provisioners over
-- QGA, gracefully stop the bake VM, then publish via clone. When
-- the build's effective 'buildBuildCache' (CLI flag OR YAML field)
-- is set, the per-step hook snapshots the bake VM's writable disks
-- after each successful step and writes a 'BuildCacheEntry' row.
runProvisionersStopAndPublish
  :: ServerState
  -> TaskId
  -> BuildSink
  -> Int64
  -> Int64
  -> BuildTarget
  -> Bool
  -> UTCTime
  -> BuildOptions
  -> Int
  -- ^ starting step index (1 = no cache resume)
  -> Build
  -> LoggingT IO (Either Text Int64)
runProvisionersStopAndPublish state parentTaskId sink vmIdLong artifactDiskId target needFlatten startTime opts startStep b = do
  hook <- buildCacheStepHook state sink vmIdLong artifactDiskId opts b
  provResult <- runProvisioners state parentTaskId sink vmIdLong b startTime startStep hook
  case provResult of
    Left err -> pure $ Left err
    Right () -> do
      stopResp <-
        liftIO $
          runActionAsSubtask (mkActionContext state parentTaskId "system") (VmStop vmIdLong 300)
      case classifyStopResp stopResp of
        Left err -> pure $ Left $ "stop bake VM: " <> err
        Right () -> do
          -- Clone the bake VM's artifact disk into a fresh
          -- non-ephemeral 'DiskImage' (qemu-img convert; flat by
          -- construction). The bake VM's original artifact disk
          -- stays attached and ephemeral so a follow-up
          -- @--use-cache@ build can roll it back to a cached step
          -- (the cleanup stack will reap it if no cache rows
          -- reference the bake VM).
          publishResult <-
            publishArtifact
              state
              parentTaskId
              vmIdLong
              artifactDiskId
              (buildName b)
              target
              needFlatten
          case publishResult of
            Left err -> pure $ Left err
            Right publishedId -> pure $ Right publishedId

-- | Build the per-step success hook for 'runProvisioners'. When
-- caching is off (the YAML's @buildCache:@ field is False and the
-- CLI's @--build-cache@ wasn't passed) the hook is a no-op. When
-- caching is on, the hook enumerates the bake VM's writable disks,
-- takes one atomic multi-disk snapshot via QMP @transaction@ with
-- @fsfreeze@ (quiesce=Require), and writes the matching 'Snapshot'
-- + 'BuildCacheEntry' rows. A snapshot or DB-write failure aborts
-- the build — the alternative (keep going with a partial cache) is
-- a foot-gun, and the operator can always retry without
-- @--build-cache@.
buildCacheStepHook
  :: ServerState
  -> BuildSink
  -> Int64
  -- ^ bake VM id (the chain's owner)
  -> Int64
  -- ^ artifact disk id
  -> BuildOptions
  -> Build
  -> LoggingT IO (Int -> LoggingT IO (Either Text ()))
buildCacheStepHook state sink vmId artifactDiskId opts b
  | not (boBuildCache opts) =
      pure (\_ -> pure (Right ()))
  | buildStrategy b == BuildStrategyInstaller =
      -- The installer strategy doesn't run provisioner steps, so a
      -- per-step hook can never fire. Defensive no-op.
      pure (\_ -> pure (Right ()))
  | otherwise = do
      let pipelineKey = H.envelopeHash b <> ":" <> buildName b
          chains = map snd (H.chainHashes b)
      pure $ \stepIdx -> do
        case drop (stepIdx - 1) chains of
          [] -> pure (Right ()) -- no chain hash for this index (shouldn't happen)
          (chain : _) -> do
            dr <- Cache.writableCacheDisks state vmId artifactDiskId
            case dr of
              Left err -> pure (Left ("cache: enumerate disks: " <> err))
              Right disks -> do
                snapResult <-
                  Cache.snapshotCachedStep
                    state
                    (buildCacheMode b)
                    vmId
                    pipelineKey
                    stepIdx
                    chain
                    disks
                case snapResult of
                  Left err -> pure (Left err)
                  Right () -> do
                    liftIO $ sink (StepCacheStore stepIdx chain)
                    pure (Right ())

--------------------------------------------------------------------------------
-- Build floppy attachment
--------------------------------------------------------------------------------

-- | If the build supplies a 'Floppy', materialise the FAT12 image,
-- register a 'DiskImage' for it (marked @ephemeral=True@ so the bake-
-- VM teardown auto-reaps it), and attach it to the bake VM via
-- 'DiskAttach' as @if=floppy,readonly=on@. The @__build_<taskId>_*@
-- name prefix is kept for diagnostic clarity only — cleanup is driven
-- by the ephemeral flag, not the name.
--
-- A 'Nothing' floppy is a no-op (returns @Right ()@), so this can be
-- called unconditionally.
attachBuildFloppy
  :: ServerState
  -> TaskId
  -> CleanupStack
  -> Int64
  -- ^ bake VM id
  -> Text
  -- ^ ephemeral name prefix (matches 'runOneBuildBody')
  -> Build
  -> LoggingT IO (Either Text ())
attachBuildFloppy state parentTaskId stack vmIdLong prefix b = case buildFloppy b of
  Nothing -> pure $ Right ()
  Just f -> case floppyContentBase64 f of
    Nothing ->
      pure $
        Left $
          "build '" <> buildName b <> "': floppy missing contentBase64 (client preprocessing bug)"
    Just b64 -> case B64.decode (TE.encodeUtf8 b64) of
      Left e ->
        pure $
          Left $
            "build '" <> buildName b <> "': floppy contentBase64 not valid base64: " <> T.pack e
      Right bytes -> do
        let filename = fromMaybe "autounattend.xml" (floppyFilename f)
            diskName = prefix <> sanitizeNameFragment (buildName b) <> "-floppy"
        basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
        let imgPath = basePath </> T.unpack diskName <> ".img"
            storedPath = makeRelativeToBase basePath imgPath
        logInfoN $
          "build floppy: writing FAT12 image at "
            <> T.pack imgPath
            <> " (file="
            <> filename
            <> ", "
            <> T.pack (show (BS.length bytes))
            <> " bytes)"
        imgRes <- liftIO $ buildFloppyImage imgPath filename bytes
        case imgRes of
          Left err -> pure $ Left $ "build floppy: " <> err
          Right () -> do
            now <- liftIO getCurrentTime
            -- Insert the DiskImage row AND pin it to the bake VM's
            -- node in one transaction. 'buildFloppyImage' just wrote
            -- the .img to the daemon's base path; in the single-node
            -- setup that is the only place builds run today the
            -- daemon host is also the bake VM's node, so the
            -- 'storedPath' we record here resolves to a real file
            -- when qemu opens it. Without the DiskImageNode row the
            -- same-node check in 'handleDiskAttach' fails and the
            -- floppy can't be attached.
            mResult <-
              liftIO $
                runSqlPool
                  ( do
                      mvm <- get (toSqlKey vmIdLong :: VmId)
                      case mvm of
                        Nothing -> pure Nothing
                        Just bakeVm -> do
                          dkey <-
                            insert
                              DiskImage
                                { diskImageName = diskName
                                , diskImageFormat = FormatRaw
                                , diskImageSizeMb = Just 2
                                , diskImageCreatedAt = now
                                , diskImageBackingImageId = Nothing
                                , -- Floppy ISO is generated per-build for the
                                  -- bake VM and only carries that build's
                                  -- credentials/script; reaped together with
                                  -- the bake VM on success or failure.
                                  diskImageEphemeral = True
                                }
                          recordDiskImageNode dkey (vmNodeId bakeVm) storedPath
                          pure $ Just dkey
                  )
                  (ssDbPool state)
            case mResult of
              Nothing ->
                pure $
                  Left "build floppy: bake VM not found when registering disk"
              Just diskKey -> do
                let diskIdLong = fromSqlKey diskKey
                -- Attach as floppy. If the bake VM rejects the attach we
                -- still want the DiskImage row dropped, hence the cleanup
                -- destructor here rather than only on success.
                liftIO $
                  push stack "build-floppy" $ do
                    _ <- runActionAsSubtask (mkActionContext state parentTaskId "system") (DiskDelete diskIdLong)
                    pure ()
                attachResp <-
                  liftIO $
                    runActionAsSubtask
                      (mkActionContext state parentTaskId "system")
                      ( DiskAttach
                          vmIdLong
                          diskIdLong
                          InterfaceFloppy
                          Nothing
                          True -- readOnly
                          False
                          CacheNone
                      )
                case attachResp of
                  RespDiskAttached _ -> pure $ Right ()
                  RespError err -> pure $ Left $ "attach build floppy: " <> err
                  _ -> pure $ Left "attach build floppy: unexpected response"

--------------------------------------------------------------------------------
-- Installer-strategy phase
--------------------------------------------------------------------------------

-- | The installer strategy's middle: dispatch boot keys, wait for the
-- guest to power itself off, then publish the artifact. No QGA, no
-- provisioners — the autounattend (or equivalent vendor mechanism) on
-- a floppy/CD inside the bake VM drives everything.
runInstallerPhase
  :: ServerState
  -> TaskId
  -> BuildSink
  -> Int64
  -> Int64
  -> BuildTarget
  -> Bool
  -> Build
  -> LoggingT IO (Either Text Int64)
runInstallerPhase state parentTaskId sink vmId artifactDiskId target needFlatten b = do
  logInfoN $ "installer: bake VM " <> T.pack (show vmId) <> " started"
  liftIO $ sink (BuildLogLine "installer: bake VM started")
  -- Fire boot-time keystrokes (e.g. dismiss UEFI's "Press any key").
  runBootKeys state vmId (buildBootKeys b) sink
  let waitMsg =
        "installer: waiting up to "
          <> T.pack (show (buildWaitForShutdownSec b))
          <> "s for guest-initiated shutdown"
  logInfoN waitMsg
  liftIO $ sink (BuildLogLine waitMsg)
  shutdownResult <- liftIO $ waitForBakeVmShutdown state vmId (buildWaitForShutdownSec b)
  case shutdownResult of
    Left err -> pure $ Left $ "installer: " <> err
    Right () -> do
      logInfoN "installer: guest shut down; publishing artifact"
      liftIO $ sink (BuildLogLine "installer: guest shut down; publishing artifact")
      publishResult <-
        publishArtifact
          state
          parentTaskId
          vmId
          artifactDiskId
          (buildName b)
          target
          needFlatten
      case publishResult of
        Left err -> pure $ Left err
        Right publishedId -> pure $ Right publishedId

-- | Sleep, then send each boot-key chord via QMP. Errors are logged
-- (the bake VM may not have its QMP socket up yet on the very first
-- key, or the user's @delaySec@ may be too small) but never fatal —
-- a missed keystroke usually just means the firmware default-booted
-- correctly anyway.
runBootKeys :: ServerState -> Int64 -> [BootKey] -> BuildSink -> LoggingT IO ()
runBootKeys state vmId keys sink = mapM_ fire keys
  where
    fire bk = do
      liftIO $ threadDelay (bkDelaySec bk * 1000000)
      let presses = max 1 (bkRepeat bk)
          interval = max 0 (bkIntervalSec bk) * 1000000
          msg = "installer: sending key '" <> bkKeys bk <> "' x" <> T.pack (show presses)
      logInfoN msg
      liftIO $ sink (BuildLogLine msg)
      mapM_
        ( \i -> do
            res <- liftIO $ qmpSendKey (ssQemuConfig state) vmId (bkKeys bk)
            case res of
              QmpSuccess -> pure ()
              other -> do
                let warnMsg = "installer: send-key failed: " <> T.pack (show other)
                logWarnN warnMsg
                liftIO $ sink (BuildLogLine warnMsg)
            liftIO $ when (i < presses) (threadDelay interval)
        )
        [1 .. presses]

-- | Poll until the bake VM's status moves to 'VmStopped' or 'VmError',
-- or the timeout elapses. Polls once per second; the existing process
-- monitor thread (forked at VmStart) writes the status update.
waitForBakeVmShutdown :: ServerState -> Int64 -> Int -> IO (Either Text ())
waitForBakeVmShutdown state vmId timeoutSec = go (max 1 timeoutSec)
  where
    go 0 = pure $ Left "timed out waiting for guest shutdown"
    go remaining = do
      mStatus <- runSqlPool (statusOf vmId) (ssDbPool state)
      case mStatus of
        Just VmStopped -> pure $ Right ()
        Just VmError -> pure $ Left "bake VM entered error state"
        _ -> do
          threadDelay 1000000
          go (remaining - 1)
    statusOf vid = do
      mVm <- get (toSqlKey vid :: VmId)
      pure (vmStatus <$> mVm)

--------------------------------------------------------------------------------
-- Provisioner execution
--------------------------------------------------------------------------------

-- | Per-step output cap persisted to @task.message@. Streamed lines are
-- forwarded to the client unbounded; only the snapshot we save to the DB
-- is bounded.
provisionerOutputCap :: Int
provisionerOutputCap = 64 * 1024

-- | Iterate provisioners @startIdx..N@ in order. After each
-- successful step the 'postStep' hook fires with the 1-based step
-- index — used by the build cache to snapshot the bake VM's
-- writable disks and record a 'BuildCacheEntry'. A 'Left' from
-- either the provisioner or the hook aborts the loop.
runProvisioners
  :: ServerState
  -> TaskId
  -> BuildSink
  -> Int64
  -> Build
  -> UTCTime
  -> Int
  -- ^ starting step index (1-based; passing N+1 means \"all cached, skip\")
  -> (Int -> LoggingT IO (Either Text ()))
  -- ^ per-step success hook
  -> LoggingT IO (Either Text ())
runProvisioners state parentTaskId sink vmId b _startTime startIdx postStep = do
  cEnv <- liftIO $ buildCorvusEnv state b vmId parentTaskId
  let sd = buildShellDefaults b
      provs = drop (startIdx - 1) (buildProvisioners b)
      go _ [] = pure $ Right ()
      go idx (p : ps) = do
        r <- runProvisioner state parentTaskId sink vmId sd cEnv idx p
        case r of
          Left err -> pure $ Left err
          Right () -> do
            hookR <- postStep idx
            case hookR of
              Left err -> pure $ Left err
              Right () -> go (idx + 1) ps
  go startIdx provs

-- | Predefined environment variables exposed to every shell provisioner.
-- Names are stable; values are read from the current 'Build', the bake
-- VM record, and the running daemon. Steps that need different values
-- override by re-declaring the same key in 'shellDefaults.env' or the
-- step's own @env:@ — later @export@ wins in the assembled command.
buildCorvusEnv :: ServerState -> Build -> Int64 -> TaskId -> IO [(Text, Text)]
buildCorvusEnv state b vmId parentTaskId = do
  mDetails <- runSqlPool (getVmDetails (ssQemuConfig state) vmId) (ssDbPool state)
  let bakeJson = case mDetails of
        Just d -> TE.decodeUtf8 (LBS.toStrict (Aeson.encode d))
        Nothing -> "null"
      bakeName = maybe "" vdName mDetails
      vsockEntry = case mDetails >>= vdVsockCid of
        Just cid -> [("CORVUS_BAKEVM_VSOCK_CID", T.pack (show cid))]
        Nothing -> []
  pure $
    [ ("CORVUS_VERSION", T.pack (Version.showVersion version))
    , ("CORVUS_BUILD_NAME", buildName b)
    , ("CORVUS_BUILD_TARGET", buildName b)
    , ("CORVUS_BUILD_TEMPLATE", buildTemplate b)
    , ("CORVUS_BUILD_STRATEGY", strategyName (buildStrategy b))
    , ("CORVUS_BUILD_TASK_ID", T.pack (show (fromSqlKey parentTaskId)))
    , ("CORVUS_BAKEVM_ID", T.pack (show vmId))
    , ("CORVUS_BAKEVM_NAME", bakeName)
    , ("CORVUS_BAKEVM", bakeJson)
    ]
      ++ vsockEntry

strategyName :: BuildStrategy -> Text
strategyName BuildStrategyOverlay = "overlay"
strategyName BuildStrategyFromScratch = "from-scratch"
strategyName BuildStrategyInstaller = "installer"

-- | Run a single provisioner: insert a subtask row, invoke the body,
-- finalize the row with the result + (cropped) output, emit the
-- bracketing 'StepStart'/'StepEnd' events.
runProvisioner
  :: ServerState
  -> TaskId
  -> BuildSink
  -> Int64
  -> ShellDefaults
  -> [(Text, Text)]
  -> Int
  -> Provisioner
  -> LoggingT IO (Either Text ())
runProvisioner state parentTaskId sink vmId sd cEnv stepIdx p = do
  let kind = provKind p
      desc = provDesc p
  logInfoN $ "step " <> T.pack (show stepIdx) <> " (" <> kind <> "): " <> desc
  liftIO $ sink (StepStart stepIdx kind desc)
  startedAt <- liftIO getCurrentTime
  taskKey <-
    liftIO $
      runSqlPool
        ( insert
            Task
              { taskParent = Just parentTaskId
              , taskStartedAt = startedAt
              , taskFinishedAt = Nothing
              , taskSubsystem = SubBuild
              , taskEntityId = Nothing
              , taskEntityName = if T.null desc then Nothing else Just desc
              , taskCommand = kind
              , taskResult = TaskRunning
              , taskMessage = Nothing
              , taskClientName = "system"
              }
        )
        (ssDbPool state)
  bodyResult <- liftIO $ try $ runProvisionerBody state vmId sd cEnv stepIdx sink p
  finishedAt <- liftIO getCurrentTime
  let (orchResult, taskMsg, taskResult) = case bodyResult of
        Right (Right msg) -> (Right (), msg, TaskSuccess)
        Right (Left (shortErr, longMsg)) -> (Left shortErr, longMsg, TaskError)
        Left (e :: SomeException) ->
          let txt = T.pack (show e)
           in (Left txt, Just txt, TaskError)
  liftIO $
    runSqlPool
      ( update
          taskKey
          [ TaskFinishedAt =. Just finishedAt
          , TaskResult =. taskResult
          , TaskMessage =. taskMsg
          ]
      )
      (ssDbPool state)
  case orchResult of
    Left _ ->
      liftIO $ cancelRemainingSubtasks (ssDbPool state) parentTaskId
    Right _ -> pure ()
  let endEvent = case orchResult of
        Right _ -> StepEnd stepIdx TaskSuccess Nothing
        Left e -> StepEnd stepIdx TaskError (Just e)
  liftIO $ sink endEvent
  pure orchResult

-- | The body half of 'runProvisioner', isolated so subtask bookkeeping
-- stays out of the per-kind logic. Returns:
--
--   @Right msg@ — success; @msg@ is persisted as the subtask's
--   @task.message@ (typically the cropped output tail).
--
--   @Left (shortErr, longMsg)@ — failure; @shortErr@ propagates up to
--   the build-level error summary, @longMsg@ is what the subtask's
--   @task.message@ stores (cropped output tail prefixed with the exit
--   code, etc.).
runProvisionerBody
  :: ServerState
  -> Int64
  -> ShellDefaults
  -> [(Text, Text)]
  -> Int
  -> BuildSink
  -> Provisioner
  -> IO (Either (Text, Maybe Text) (Maybe Text))
runProvisionerBody state vmId sd cEnv stepIdx sink p = case p of
  ProvShell sh -> case shellInline sh of
    Nothing ->
      pure $
        Left
          ( "shell: missing inline body (client-side bug)"
          , Just "shell: missing inline body (client-side bug)"
          )
    Just body -> do
      let maxPolls = case shellTimeoutSec sh of
            Just s -> max 60 (s * 10)
            Nothing -> 6000 -- 10 minutes default
          fullCmd = buildShellCommand sd cEnv sh body
      bufRef <- newIORef BS.empty
      totalRef <- newIORef (0 :: Int)
      let onLine line = do
            sink (StepOutput stepIdx line)
            accumulateLine bufRef totalRef line
      result <-
        agentGuestExecWithTail
          state
          vmId
          fullCmd
          (fromIntegral maxPolls)
          onLine
      tail' <- finalizeStepBuf bufRef totalRef
      pure $ case result of
        GuestExecSuccess 0 _ _ -> Right tail'
        GuestExecSuccess code _ _ ->
          let header = "exit code " <> T.pack (show code)
              long = case tail' of
                Just t -> Just (header <> "\n" <> t)
                Nothing -> Just header
           in Left ("shell: " <> header, long)
        GuestExecError msg ->
          Left ("shell agent error: " <> msg, Just msg)
        GuestExecConnectionFailed msg ->
          Left ("shell agent connection failed: " <> msg, Just msg)
  ProvFile fp -> case fileContentBase64 fp of
    Nothing ->
      pure $
        Left
          ( "file: missing content (client-side bug)"
          , Just "file: missing content (client-side bug)"
          )
    Just contentB64 -> do
      let mode = fromMaybe "0644" (fileMode fp)
          dest = shellQuote (fileTo fp)
          payload = TE.encodeUtf8 contentB64
          cmd =
            "umask 022; install -d $(dirname "
              <> dest
              <> ") && base64 -d > "
              <> dest
              <> " && chmod "
              <> mode
              <> " "
              <> dest
      sink (StepOutput stepIdx ("writing " <> fileTo fp))
      result <-
        agentGuestExecWithStdin
          state
          vmId
          cmd
          payload
          600
      pure $ classifyAtomic "file" (fileTo fp) result
  ProvWaitFor wf -> waitForIO state stepIdx sink vmId wf
  ProvReboot rb -> rebootGuestIO state stepIdx sink vmId (rebootTimeoutSec rb)

-- | Append a streamed line to the per-step ring buffer, updating the
-- byte-counter that lets 'finalizeStepBuf' know whether truncation
-- happened. Inputs are encoded UTF-8 with a trailing newline so the
-- saved tail looks like the operator's terminal.
accumulateLine :: IORef BS.ByteString -> IORef Int -> Text -> IO ()
accumulateLine bufRef totalRef line = do
  let bytes = TE.encodeUtf8 line <> "\n"
  modifyIORef' totalRef (+ BS.length bytes)
  buf <- readIORef bufRef
  let combined = buf <> bytes
      cropped =
        if BS.length combined > provisionerOutputCap
          then BS.drop (BS.length combined - provisionerOutputCap) combined
          else combined
  writeIORef bufRef cropped

-- | Materialize the bounded buffer as Text. If we dropped data, prepend
-- a marker so post-mortem readers know they're seeing only the tail.
finalizeStepBuf :: IORef BS.ByteString -> IORef Int -> IO (Maybe Text)
finalizeStepBuf bufRef totalRef = do
  buf <- readIORef bufRef
  if BS.null buf
    then pure Nothing
    else do
      total <- readIORef totalRef
      let droppedKb = (total - BS.length buf) `div` 1024
          prefix
            | droppedKb > 0 =
                "... [truncated, "
                  <> T.pack (show droppedKb)
                  <> " KiB earlier]\n"
            | otherwise = ""
      pure $ Just (prefix <> TE.decodeUtf8With lenientDecode buf)

-- | One-shot classifier for non-streaming provisioners (file upload,
-- provenance) where the output isn't tailed line-by-line.
classifyAtomic
  :: Text
  -> Text
  -> GuestExecResult
  -> Either (Text, Maybe Text) (Maybe Text)
classifyAtomic lbl entityName r = case r of
  GuestExecSuccess 0 _ _ ->
    Right (Just (lbl <> ": " <> entityName))
  GuestExecSuccess code stdout stderr ->
    let combined = joinOutputs stdout stderr
        long =
          "exit code "
            <> T.pack (show code)
            <> ( if T.null combined
                  then ""
                  else "\n" <> combined
               )
     in Left (lbl <> " exited " <> T.pack (show code), Just long)
  GuestExecError msg -> Left (lbl <> " agent error: " <> msg, Just msg)
  GuestExecConnectionFailed msg ->
    Left (lbl <> " agent connection failed: " <> msg, Just msg)

joinOutputs :: Text -> Text -> Text
joinOutputs out err
  | T.null err = out
  | T.null out = err
  | otherwise = out <> "\n" <> err

-- | Tag a provisioner with its short kind name (used as the subtask's
-- @command@ field and the streaming @StepStart@ event tag).
provKind :: Provisioner -> Text
provKind ProvShell {} = "shell"
provKind ProvFile {} = "file"
provKind ProvWaitFor {} = "wait-for"
provKind ProvReboot {} = "reboot"

-- | A short human description: file path for file provisioners,
-- inline body's first line for shell, etc. Surfaced as the @StepStart@
-- description and the subtask's @entity_name@.
provDesc :: Provisioner -> Text
provDesc (ProvShell sh) =
  let firstLine = case shellInline sh of
        Just body ->
          let trimmed = T.strip body
              ln = T.takeWhile (/= '\n') trimmed
           in if T.null ln then "<empty>" else T.take 60 ln
        Nothing -> "<no body>"
   in firstLine
provDesc (ProvFile fp) = fileTo fp
provDesc (ProvWaitFor wf) = case wf of
  WaitForPing _ -> "guest-agent ping"
  WaitForFile p _ -> "file " <> p
  WaitForPort port _ -> "port " <> T.pack (show port)
provDesc (ProvReboot _) = "reboot"

-- | Lightweight shell quoting: wrap in single quotes and escape any single
-- quotes inside. Sufficient for paths and env values that don't contain
-- newlines.
shellQuote :: Text -> Text
shellQuote t = "'" <> T.replace "'" "'\\''" t <> "'"

-- | Assemble the shell command sent to the guest agent for a shell
-- provisioner. Pure; lifted out of 'runProvisionerBody' so the
-- concatenation order is unit-testable.
--
-- The pieces are joined with newlines (not @;@) so a multi-line
-- preamble (e.g. @set -eux@ followed by a function definition) and
-- a body that uses @#@ comments or backslash line-continuations
-- both parse correctly.
--
-- Order:
--
--   1. 'sdPreamble' — runs first so @set -e@ propagates into env exports.
--   2. corvusEnv    — predefined @CORVUS_*@ vars (build name, version,
--      bake VM identity, …) exported BEFORE user-defined env so steps
--      and 'shellDefaults.env' can override by re-declaring the same key.
--   3. 'sdEnv'      — build-level defaults env, exported.
--   4. 'shellEnv'   — per-step env, exported AFTER defaults so the
--      step can override defaults by re-declaring the same key.
--   5. 'shellWorkdir' — @cd@, if set.
--   6. body         — the operator's @inline@ text.
--
-- The auto-injected env (Corvus + 'sdEnv') is wrapped in a @set +x@
-- save/restore so that a user preamble like @set -eux@ doesn't flood
-- the build log with two lines per Corvus variable. The wrapper is
-- POSIX (no bash-isms) and a no-op when @-x@ wasn't on. Per-step
-- @env:@ is left outside the wrapper — operators wrote those vars
-- themselves, so seeing them traced is appropriate.
buildShellCommand :: ShellDefaults -> [(Text, Text)] -> Shell -> Text -> Text
buildShellCommand sd corvusEnv sh body =
  preambleBlock
    <> autoEnvBlock
    <> stepEnvBlock
    <> workdirBlock
    <> body
  where
    preambleBlock = case sdPreamble sd of
      Just p -> p <> "\n"
      Nothing -> ""
    autoEnv = renderEnv corvusEnv <> renderEnv (sdEnv sd)
    autoEnvBlock
      | T.null autoEnv = ""
      | otherwise =
          "{ __corvus_xs=$-; set +x; } 2>/dev/null\n"
            <> autoEnv
            <> "{ case $__corvus_xs in *x*) set -x;; esac; unset __corvus_xs; } 2>/dev/null\n"
    stepEnvBlock = renderEnv (shellEnv sh)
    workdirBlock = case shellWorkdir sh of
      Just d -> "cd " <> shellQuote d <> "\n"
      Nothing -> ""
    renderEnv =
      T.concat
        . map (\(k, v) -> "export " <> k <> "=" <> shellQuote v <> "\n")

waitForIO
  :: ServerState
  -> Int
  -> BuildSink
  -> Int64
  -> WaitFor
  -> IO (Either (Text, Maybe Text) (Maybe Text))
waitForIO state stepIdx sink vmId w = case w of
  WaitForPing timeoutSec ->
    loopUntil timeoutSec "guest-agent ping" $
      agentGuestPing state vmId
  WaitForFile path timeoutSec ->
    loopUntil timeoutSec ("file " <> path) $ do
      r <- agentGuestExec state vmId ("test -e " <> shellQuote path) 5
      pure $ case r of
        GuestExecSuccess 0 _ _ -> True
        _ -> False
  WaitForPort port timeoutSec ->
    loopUntil timeoutSec ("port " <> T.pack (show port)) $ do
      let probe =
            "ss -ltn 2>/dev/null | awk '{print $4}' | grep -q ':"
              <> T.pack (show port)
              <> "$' || netstat -ltn 2>/dev/null | awk '{print $4}' | grep -q ':"
              <> T.pack (show port)
              <> "$'"
      r <- agentGuestExec state vmId probe 5
      pure $ case r of
        GuestExecSuccess 0 _ _ -> True
        _ -> False
  where
    loopUntil totalSec lbl probe = go 0
      where
        go elapsed
          | elapsed >= totalSec =
              let err = "wait-for " <> lbl <> ": timed out after " <> T.pack (show totalSec) <> "s"
               in pure $ Left (err, Just err)
          | otherwise = do
              ok <- probe
              if ok
                then pure $ Right (Just (lbl <> " ok after " <> T.pack (show elapsed) <> "s"))
                else do
                  when (elapsed `mod` 10 == 0) $
                    sink (StepOutput stepIdx ("waiting for " <> lbl <> " (" <> T.pack (show elapsed) <> "s)"))
                  threadDelay 2000000
                  go (elapsed + 2)

rebootGuestIO
  :: ServerState
  -> Int
  -> BuildSink
  -> Int64
  -> Int
  -> IO (Either (Text, Maybe Text) (Maybe Text))
rebootGuestIO state stepIdx sink vmId timeoutSec = do
  sink (StepOutput stepIdx "rebooting via guest-exec")
  result <-
    agentGuestExec
      state
      vmId
      "(sleep 1; /sbin/reboot || /usr/sbin/reboot || reboot) >/dev/null 2>&1 &"
      30
  case result of
    GuestExecConnectionFailed msg ->
      pure $ Left ("reboot dispatch: " <> msg, Just msg)
    _ -> do
      threadDelay 5000000 -- 5s grace
      waitForIO state stepIdx sink vmId (WaitForPing timeoutSec)

--------------------------------------------------------------------------------
-- Artifact publication
--------------------------------------------------------------------------------

-- | Publish the bake VM's artifact by CLONING it (via
-- @qemu-img convert@) into a fresh non-ephemeral 'DiskImage'. The
-- bake VM's original artifact disk stays attached and ephemeral so
-- a follow-up @--use-cache@ build can roll it back to a cached
-- step. @qemu-img convert@ copies only the source qcow2's active
-- state and does NOT preserve internal snapshots, so the published
-- disk is hard-guaranteed flat (verifiable via
-- @qemu-img snapshot -l@ returning empty output).
--
-- The bake VM MUST already be stopped before this runs — the
-- @qemu-img convert@ source path takes the file's exclusive lock.
-- The caller (runProvisionersStopAndPublish / runInstallerPhase)
-- handles the @VmStop@ before calling here.
--
-- Returns the new 'DiskImage' row's id on success.
publishArtifact
  :: ServerState
  -> TaskId
  -> Int64
  -- ^ bake VM ID (kept for diagnostics; the drive is NOT detached)
  -> Int64
  -- ^ artifact disk ID (the bake VM's ephemeral source)
  -> Text
  -- ^ published artifact name (= buildName)
  -> BuildTarget
  -- ^ target spec (format, compact, path, …)
  -> Bool
  -- ^ (legacy) flatten flag — ignored; clone is always flat
  -> LoggingT IO (Either Text Int64)
publishArtifact state parentTaskId _bakeVmId artifactDiskId name target _needFlatten = do
  overwriteResult <- deleteOverwriteTargetIfNeeded state parentTaskId name target
  case overwriteResult of
    Left err -> pure $ Left err
    Right () -> publishArtifactByClone state artifactDiskId name target

-- | Clone the bake VM's artifact disk into a fresh 'DiskImage' row,
-- compacting if asked. Returns the new disk's id.
publishArtifactByClone
  :: ServerState
  -> Int64
  -- ^ source (bake VM artifact) disk id
  -> Text
  -- ^ published artifact name
  -> BuildTarget
  -> LoggingT IO (Either Text Int64)
publishArtifactByClone state srcDiskId name target = do
  let pool = ssDbPool state
      srcKey = toSqlKey srcDiskId :: DiskImageId
  placements <- liftIO $ runSqlPool (listDiskImageNodes srcKey) pool
  case placements of
    [] -> pure $ Left "publish: bake artifact has no recorded placement"
    (Entity _ row : _) -> do
      let nid = diskImageNodeNodeId row
      srcPath <- liftIO $ resolveDiskPath pool (ssQemuConfig state) srcKey nid
      basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
      let ext = T.unpack (enumToText (btFormat target))
          fileName = T.unpack name <> "." <> ext
          destPath = resolveDiskFilePathPure basePath (btPath target) fileName
      liftIO $ createDirectoryIfMissing True (takeDirectory destPath)
      logInfoN $
        "publish: cloning "
          <> T.pack srcPath
          <> " -> "
          <> T.pack destPath
      cloneRes <- liftIO $ cloneImageViaAgent state nid srcPath destPath (btFormat target)
      case cloneRes of
        ImageError err -> pure $ Left $ "publish clone: " <> err
        ImageNotFound -> pure $ Left "publish clone: source not found"
        ImageFormatNotSupported msg -> pure $ Left $ "publish clone: " <> msg
        ImageSuccess -> do
          now <- liftIO getCurrentTime
          mSize <- liftIO $ getImageSizeMbViaAgent state nid destPath
          let storedPath =
                if (basePath ++ "/") `Data.List.isPrefixOf` destPath
                  then T.pack (drop (length basePath + 1) destPath)
                  else T.pack destPath
          newKey <-
            liftIO $
              runSqlPool
                ( insert
                    DiskImage
                      { diskImageName = name
                      , diskImageFormat = btFormat target
                      , diskImageSizeMb = mSize
                      , diskImageCreatedAt = now
                      , diskImageBackingImageId = Nothing
                      , diskImageEphemeral = False
                      }
                )
                pool
          liftIO $
            runSqlPool
              (recordDiskImageNode newKey nid storedPath)
              pool
          let newId = fromSqlKey newKey
          when (btCompact target) $ compactDisk state newId
          pure $ Right newId

-- | Decide what to do at the very top of a build, before the bake
-- VM is created, based on the target's 'btIfExists' policy and
-- whether a disk with the target name already exists.
--
--   * @Right Nothing@ — proceed to bake.
--   * @Right (Just diskId)@ — skip the bake; this disk is the
--     existing artifact and is returned as the build's success
--     result. Only happens with @ifExists: skip@.
--   * @Left err@ — fail-fast. Either @ifExists: error@ and the name
--     is taken, or @ifExists: overwrite@ and the existing disk is
--     attached to one or more VMs (delete-then-rebake would yank
--     the disk out from under those VMs, which we refuse).
--
-- For @ifExists: overwrite@ this only validates that the deletion
-- can later proceed safely; the actual deletion is deferred to
-- 'deleteOverwriteTargetIfNeeded' at publish time, so a mid-bake
-- failure preserves the existing artifact.
checkIfExistsPreBake
  :: ServerState
  -> Text
  -> BuildTarget
  -> LoggingT IO (Either Text (Maybe Int64))
checkIfExistsPreBake state name target = do
  let pool = ssDbPool state
  mExisting <- liftIO $ runSqlPool (getBy (UniqueDiskImageName name)) pool
  case (btIfExists target, mExisting) of
    (_, Nothing) -> pure $ Right Nothing
    (IfExistsError, Just _) ->
      pure $
        Left $
          "target '"
            <> name
            <> "' already exists; use ifExists: skip or overwrite to allow"
    (IfExistsSkip, Just (Entity existingId _)) -> do
      logInfoN $ "target '" <> name <> "' exists; skipping bake (ifExists: skip)"
      pure $ Right (Just (fromSqlKey existingId))
    (IfExistsOverwrite, Just (Entity existingId _)) -> do
      attachedVms <- liftIO $ runSqlPool (vmsAttachedToDisk existingId) pool
      if null attachedVms
        then pure $ Right Nothing
        else
          pure $
            Left $
              "target.ifExists: overwrite refused — disk '"
                <> name
                <> "' is attached to VM(s): "
                <> T.intercalate ", " attachedVms
                <> "; detach or delete those VMs first."

-- | At publish time, if the target's policy is 'IfExistsOverwrite'
-- and the existing disk is still there, delete it so the freshly
-- baked artifact can take the name. The pre-bake check has already
-- verified the disk is not attached; if a new VM has attached
-- during the bake, this still fails loudly.
--
-- For 'IfExistsError' and 'IfExistsSkip' this is a no-op — those
-- cases are decided pre-bake and never reach publish.
deleteOverwriteTargetIfNeeded
  :: ServerState
  -> TaskId
  -> Text
  -> BuildTarget
  -> LoggingT IO (Either Text ())
deleteOverwriteTargetIfNeeded state parentTaskId name target
  | btIfExists target /= IfExistsOverwrite = pure $ Right ()
  | otherwise = do
      let pool = ssDbPool state
      mExisting <- liftIO $ runSqlPool (getBy (UniqueDiskImageName name)) pool
      case mExisting of
        Nothing -> pure $ Right ()
        Just (Entity existingId _) -> do
          attachedVms <- liftIO $ runSqlPool (vmsAttachedToDisk existingId) pool
          if null attachedVms
            then do
              logInfoN $ "target.ifExists: deleting existing disk '" <> name <> "' (overwrite)"
              resp <-
                liftIO $
                  runActionAsSubtask (mkActionContext state parentTaskId "system") (DiskDelete (fromSqlKey existingId))
              case resp of
                RespDiskOk -> pure $ Right ()
                RespError err ->
                  pure $ Left $ "target.ifExists: delete existing disk: " <> err
                _ -> pure $ Left "target.ifExists: delete existing disk: unexpected response"
            else
              pure $
                Left $
                  "target.ifExists: disk '"
                    <> name
                    <> "' is attached to VM(s): "
                    <> T.intercalate ", " attachedVms
                    <> "; detach or delete those VMs first."

-- | Names of every VM that has the given disk attached. Used by the
-- overwrite check to refuse silently yanking a disk out of a VM.
vmsAttachedToDisk :: DiskImageId -> SqlPersistT IO [Text]
vmsAttachedToDisk diskId = do
  drives <- selectList [DriveDiskImageId ==. diskId] []
  let vmIds = map (driveVmId . entityVal) drives
  vms <- mapM get vmIds
  pure [vmName v | Just v <- vms]

-- | Compact a qcow2 by running @qemu-img convert -O qcow2@ in place (atomic
-- via temp file + rename). On any failure this logs at @warn@ and returns
-- without raising — compaction is a size optimisation, not a correctness
-- requirement.
compactDisk :: ServerState -> Int64 -> LoggingT IO ()
compactDisk state diskId = do
  mNid <- liftIO $ pickNodeForExistingDisk state (toSqlKey diskId :: DiskImageId)
  case mNid of
    Left err -> logWarnN $ "compact: cannot resolve node: " <> err
    Right nid -> do
      mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
      case mDisk of
        Nothing -> logWarnN "compact: disk vanished"
        Just _disk -> do
          logInfoN "compact: rebasing onto self with -c (no-op flatten)"
          path <-
            liftIO $
              resolveDiskPath
                (ssDbPool state)
                (ssQemuConfig state)
                (toSqlKey diskId :: DiskImageId)
                nid
          -- A no-op rebase (Nothing → Nothing) effectively rewrites the image
          -- through qemu-img, dropping unused clusters. The rebaseImage helper
          -- already handles the in-place pass.
          result <- liftIO $ rebaseImageViaAgent state nid path Nothing False
          case result of
            ImageSuccess -> do
              mSize <- liftIO $ getImageSizeMbViaAgent state nid path
              case mSize of
                Just newSize ->
                  liftIO $
                    runSqlPool
                      ( update
                          (toSqlKey diskId :: DiskImageId)
                          [DiskImageSizeMb =. Just newSize]
                      )
                      (ssDbPool state)
                Nothing -> pure ()
            _ -> logWarnN "compact: qemu-img rebase failed (artifact still usable)"

--------------------------------------------------------------------------------
-- Bake VM teardown
--------------------------------------------------------------------------------

-- | Tear down the bake VM safely.
--
-- Stops the VM and runs @VmDelete keepDisks=False@. The delete reaps
-- every disk still attached with @DiskImage.ephemeral=True@ — which
-- covers every disk the build pipeline creates: the template-
-- instantiated overlay/clone/create-strategy disks, the bake
-- artifact disk (the published artifact is a clone, see
-- 'publishArtifact'), the build floppy and any cloud-init ISO.
-- Shared infrastructure (direct-strategy template disks, registered
-- base images) is created with @ephemeral=False@ and is preserved.
--
-- When the bake VM has any 'BuildCacheEntry' rows referencing it
-- (@--build-cache@ on the build YAML or the @--build-cache@ CLI
-- flag was set on a prior run), the @VmDelete@ is skipped: the VM
-- + its ephemeral disks survive so future @--use-cache@ builds can
-- roll back to a cached step. Only an explicit @crv vm delete@
-- reaps a cache-retained bake VM (which cascades the cache rows
-- via 'deleteVm').
cleanupBakeVm :: ServerState -> TaskId -> Int64 -> IO ()
cleanupBakeVm state parentTaskId vmIdLong = do
  -- Best-effort stop first; VmDelete refuses while running.
  _ <- runActionAsSubtask (mkActionContext state parentTaskId "system") (VmStop vmIdLong 300)
  -- If any 'BuildCacheEntry' row still references this bake VM, keep
  -- it alive: future builds with @--use-cache@ need its disks intact
  -- to roll back to a cached step. Drop the cache rows (via VM
  -- delete) only when the build's own cleanup or a subsequent
  -- @crv vm delete@ removes the VM explicitly.
  hasCache <- CStore.bakeVmHasCache state vmIdLong
  if hasCache
    then pure ()
    else do
      _ <- runActionAsSubtask (mkActionContext state parentTaskId "system") (VmDelete vmIdLong False)
      pure ()

--------------------------------------------------------------------------------
-- Resolving and validating the source template
--------------------------------------------------------------------------------

-- | Look up a template by name, returning its id and whether guest-agent
-- is enabled. Both of those have to be true for a build to proceed.
resolveTemplateIdOrErr :: ServerState -> Text -> IO (Either Text (Int64, Bool))
resolveTemplateIdOrErr state name = do
  mEntity <- runSqlPool (getBy (UniqueTemplateVmName name)) (ssDbPool state)
  pure $ case mEntity of
    Nothing -> Left $ "template '" <> name <> "' not found"
    Just (Entity key tpl) -> Right (fromSqlKey key, templateVmGuestAgent tpl)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Sanitise a build name for use in a generated VM/disk name. Keeps
-- alphanumerics and dashes; everything else becomes a single dash.
sanitizeNameFragment :: Text -> Text
sanitizeNameFragment t =
  let cleaned = T.map (\c -> if isSafe c then c else '-') t
      collapsed = T.intercalate "-" $ filter (not . T.null) (T.split (== '-') cleaned)
   in T.take 32 (if T.null collapsed then "build" else collapsed)
  where
    isSafe c = c `elem` ('-' : ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z'])

classifyStartResp :: Response -> Either Text ()
classifyStartResp r = case r of
  RespVmStateChanged _ -> Right ()
  RespError err -> Left err
  RespVmRunning -> Right ()
  RespInvalidTransition _ msg -> Left msg
  _ -> Left $ "unexpected response: " <> T.pack (show r)

classifyStopResp :: Response -> Either Text ()
classifyStopResp r = case r of
  RespVmStateChanged _ -> Right ()
  RespError err -> Left err
  RespInvalidTransition _ msg -> Left msg
  _ -> Left $ "unexpected response: " <> T.pack (show r)
