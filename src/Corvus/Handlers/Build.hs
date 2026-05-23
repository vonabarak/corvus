{-# LANGUAGE FlexibleContexts #-}
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
import Corvus.Handlers.Apply (ApplyAction (..))
import Corvus.Handlers.Build.Cleanup (CleanupStack, newCleanupStack, push, withCleanup)
import Corvus.Handlers.Build.Floppy (buildFloppyImage)
import Corvus.Handlers.Disk (DiskCreate (..), DiskDelete (..), DiskRebase (..))
import Corvus.Handlers.Disk.Agent (getImageSizeMbViaAgent, rebaseImageViaAgent)
import Corvus.Handlers.Disk.Attach (DiskAttach (..), DiskDetachByDisk (..))
import Corvus.Handlers.Disk.Db (listDiskImageNodes, recordDiskImageNode)
import Corvus.Handlers.Disk.Path (makeRelativeToBase, resolveDiskFilePathPure, resolveDiskPath)
import Corvus.Handlers.Resolve (validateName)
import Corvus.Handlers.Scheduler (pickNodeForDisk)
import Corvus.Handlers.Template (TemplateInstantiate (..))
import Corvus.Handlers.Vm (VmDelete (..), VmStart (..), VmStop (..), getVmDetails)
import Corvus.Model
import Corvus.Node.GuestAgent (GuestExecResult (..))
import Corvus.Node.Image (ImageResult (..))
import Corvus.Node.Qmp (QmpResult (..), qmpSendKey)
import qualified Corvus.Node.VmSpec as VS
import qualified Corvus.NodeAgentClient as NOA
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
import Data.Maybe (fromMaybe)
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

newtype BuildAction = BuildAction
  { baYaml :: Text
  }

instance Action BuildAction where
  actionSubsystem _ = SubBuild
  actionCommand _ = "build"
  actionExecute ctx a = handleBuildExecute (acState ctx) (acTaskId ctx) (baYaml a)

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
    resp <- liftIO $ runActionAsSubtask state (ApplyAction cfg False) parentTaskId
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
  validateName "Target disk" (btName (buildTarget b))
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
-- down on failure. The artifact disk's destructor is detached on success.
runOneBuild :: ServerState -> TaskId -> BuildSink -> Build -> LoggingT IO (Either Text Int64)
runOneBuild state parentTaskId sink b = do
  startTime <- liftIO getCurrentTime
  stack <- liftIO newCleanupStack
  outcome <- withCleanup (buildCleanup b) stack (runOneBuildBody state parentTaskId sink stack startTime b)
  case outcome of
    Right inner -> pure inner
    Left ex -> pure $ Left $ "exception: " <> T.pack (show ex)

runOneBuildBody
  :: ServerState
  -> TaskId
  -> BuildSink
  -> CleanupStack
  -> UTCTime
  -> Build
  -> LoggingT IO (Either Text Int64)
runOneBuildBody state parentTaskId sink stack startTime b = do
  let prefix = "__build_" <> T.pack (show (fromSqlKey parentTaskId)) <> "_"
      bakeVmName = prefix <> sanitizeNameFragment (buildName b) <> "-vm"
      targetTmpName = prefix <> sanitizeNameFragment (buildName b) <> "-target"
      target = buildTarget b
      strategy = buildStrategy b

  -- 0. Pre-bake target.ifExists check. Decide all three policies before
  --    spinning up the bake VM so we never bake just to discover at
  --    publish time that the target name was a problem.
  preBake <- checkIfExistsPreBake state target
  case preBake of
    Left err -> pure $ Left err
    Right (Just existingId) -> pure $ Right existingId
    Right Nothing -> runOneBuildBodyAfterPreBake state parentTaskId sink stack startTime b

-- | The original 'runOneBuildBody'. Renamed so the pre-bake
-- ifExists check can short-circuit cleanly without nesting the
-- whole bake pipeline inside another @case@.
runOneBuildBodyAfterPreBake
  :: ServerState
  -> TaskId
  -> BuildSink
  -> CleanupStack
  -> UTCTime
  -> Build
  -> LoggingT IO (Either Text Int64)
runOneBuildBodyAfterPreBake state parentTaskId sink stack startTime b = do
  let prefix = "__build_" <> T.pack (show (fromSqlKey parentTaskId)) <> "_"
      bakeVmName = prefix <> sanitizeNameFragment (buildName b) <> "-vm"
      targetTmpName = prefix <> sanitizeNameFragment (buildName b) <> "-target"
      target = buildTarget b
      strategy = buildStrategy b

  -- 1. Resolve template
  templateIdResult <- liftIO $ resolveTemplateIdOrErr state (buildTemplate b)
  case templateIdResult of
    Left err -> pure $ Left err
    Right (templateId, hasGuestAgent) ->
      -- The installer strategy boots a vendor installer ISO and waits for
      -- the guest to shut itself down; QGA isn't available until the
      -- installer reaches first-logon. The other strategies drive
      -- provisioners over QGA and cannot proceed without it.
      if strategy /= BuildStrategyInstaller && not hasGuestAgent
        then
          pure $
            Left $
              "template '" <> buildTemplate b <> "' must have guestAgent: true"
        else do
          -- 2. Instantiate template → bake VM
          mVmId <-
            liftIO $
              runActionAsSubtask
                state
                ( TemplateInstantiate
                    { tiTemplateId = templateId
                    , tiName = bakeVmName
                    , tiNodeRef = buildNode b
                    }
                )
                parentTaskId
          case mVmId of
            RespTemplateInstantiated vmIdLong -> do
              liftIO $
                push stack "bake-vm" $
                  cleanupBakeVm state parentTaskId vmIdLong
              -- 3. From-scratch strategy: create empty target disk + attach
              targetSetup <- case strategy of
                BuildStrategyOverlay -> do
                  -- Identify the artifact drive: first drive of the bake VM.
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
                      pure $
                        Right
                          ( fromSqlKey (driveDiskImageId drv)
                          , True -- needs flatten on overlay strategy
                          )
                BuildStrategyInstaller -> do
                  -- Installer's artifact is the bake VM's first drive
                  -- (a fresh blank disk created by the template's first
                  -- drive having strategy: create). No flatten needed —
                  -- there is no backing chain to collapse.
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
                      pure $ Right (fromSqlKey (driveDiskImageId drv), False)
                BuildStrategyFromScratch -> do
                  let sizeMb = fromIntegral (btSizeGb target) * 1024
                  diskResp <-
                    liftIO $
                      runActionAsSubtask
                        state
                        (DiskCreate targetTmpName (btFormat target) sizeMb Nothing False)
                        parentTaskId
                  case diskResp of
                    RespDiskCreated diskIdLong -> do
                      attachResp <-
                        liftIO $
                          runActionAsSubtask
                            state
                            ( DiskAttach
                                vmIdLong
                                diskIdLong
                                InterfaceVirtio
                                Nothing
                                False
                                False
                                CacheWriteback
                            )
                            parentTaskId
                      case attachResp of
                        RespDiskAttached _ -> pure $ Right (diskIdLong, False)
                        RespError err -> do
                          -- The disk was created but couldn't attach: register
                          -- a cleanup so a failed attach doesn't strand it.
                          liftIO $
                            push stack "orphan-target-disk" $ do
                              _ <-
                                runActionAsSubtask
                                  state
                                  (DiskDelete diskIdLong)
                                  parentTaskId
                              pure ()
                          pure $ Left $ "attach target disk: " <> err
                        _ -> pure $ Left "attach target disk: unexpected response"
                    RespError err -> pure $ Left $ "create target disk: " <> err
                    _ -> pure $ Left "create target disk: unexpected response"

              case targetSetup of
                Left err -> pure $ Left err
                Right (artifactDiskId, needFlatten) -> do
                  -- 3.5. Build + attach the autounattend / kickstart
                  --      floppy if the build supplied one. Done after
                  --      template instantiation so its drive entry is
                  --      part of the bake VM's hardware before VmStart
                  --      generates the QEMU command line.
                  floppyResult <-
                    attachBuildFloppy state parentTaskId stack vmIdLong prefix b
                  case floppyResult of
                    Left err -> pure $ Left err
                    Right () -> do
                      -- 4. Start the bake VM. For QGA-driven strategies this
                      --    blocks until the guest agent first-pings; for the
                      --    installer strategy the template has guestAgent:false
                      --    so VmStart returns as soon as QEMU is up.
                      startResp <-
                        liftIO $
                          runActionAsSubtask state (VmStart vmIdLong) parentTaskId
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
                          _ -> do
                            -- 5. Run provisioners + provenance
                            provResult <- runProvisioners state parentTaskId sink vmIdLong b startTime
                            case provResult of
                              Left err -> pure $ Left err
                              Right () -> do
                                -- 6. Stop the VM gracefully
                                stopResp <-
                                  liftIO $
                                    runActionAsSubtask state (VmStop vmIdLong) parentTaskId
                                case classifyStopResp stopResp of
                                  Left err -> pure $ Left $ "stop bake VM: " <> err
                                  Right () -> do
                                    -- 7. Detach artifact, rename, optional flatten + compact, relocate
                                    publishResult <-
                                      publishArtifact
                                        state
                                        parentTaskId
                                        vmIdLong
                                        artifactDiskId
                                        target
                                        needFlatten
                                    case publishResult of
                                      Left err -> pure $ Left err
                                      Right () ->
                                        -- The artifact has been detached from the
                                        -- bake VM and renamed. The remaining
                                        -- cleanup pass will VmDelete the bake VM
                                        -- with deleteDisks=true; the artifact is
                                        -- no longer attached so it survives.
                                        pure $ Right artifactDiskId
            RespError err -> pure $ Left $ "instantiate template: " <> err
            other ->
              pure $ Left $ "instantiate template: unexpected response: " <> T.pack (show other)

--------------------------------------------------------------------------------
-- Build floppy attachment
--------------------------------------------------------------------------------

-- | If the build supplies a 'Floppy', materialise the FAT12 image,
-- register a 'DiskImage' for it, and attach it to the bake VM via
-- 'DiskAttach' as @if=floppy,readonly=on@. The disk's name uses the
-- @__build_<taskId>_*@ prefix so the standard ephemeral cleanup path
-- ('collectEphemeralDiskIds') drops it on success or failure.
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
            -- TODO(multi-node Phase 3): record 'storedPath' in
            -- DiskImageNode on the bake VM's node.
            let _ = storedPath
            diskKey <-
              liftIO $
                runSqlPool
                  ( insert
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
                  )
                  (ssDbPool state)
            let diskIdLong = fromSqlKey diskKey
            -- Attach as floppy. If the bake VM rejects the attach we
            -- still want the DiskImage row dropped, hence the cleanup
            -- destructor here rather than only on success.
            liftIO $
              push stack "build-floppy" $ do
                _ <- runActionAsSubtask state (DiskDelete diskIdLong) parentTaskId
                pure ()
            attachResp <-
              liftIO $
                runActionAsSubtask
                  state
                  ( DiskAttach
                      vmIdLong
                      diskIdLong
                      InterfaceFloppy
                      Nothing
                      True -- readOnly
                      False
                      CacheNone
                  )
                  parentTaskId
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
          target
          needFlatten
      case publishResult of
        Left err -> pure $ Left err
        Right () -> pure $ Right artifactDiskId

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

runProvisioners
  :: ServerState
  -> TaskId
  -> BuildSink
  -> Int64
  -> Build
  -> UTCTime
  -> LoggingT IO (Either Text ())
runProvisioners state parentTaskId sink vmId b _startTime = do
  cEnv <- liftIO $ buildCorvusEnv state b vmId parentTaskId
  let sd = buildShellDefaults b
      go _ [] = pure $ Right ()
      go idx (p : ps) = do
        r <- runProvisioner state parentTaskId sink vmId sd cEnv idx p
        case r of
          Left err -> pure $ Left err
          Right () -> go (idx + 1) ps
  go 1 (buildProvisioners b)

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
    , ("CORVUS_BUILD_TARGET", btName (buildTarget b))
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

publishArtifact
  :: ServerState
  -> TaskId
  -> Int64
  -- ^ bake VM ID (so we can detach the artifact drive)
  -> Int64
  -- ^ artifact disk ID
  -> BuildTarget
  -- ^ target spec (name, format, compact, path, …)
  -> Bool
  -- ^ flatten? (overlay strategy)
  -> LoggingT IO (Either Text ())
publishArtifact state parentTaskId vmId diskId target needFlatten = do
  -- 1. Detach the drive so VmDelete doesn't take the disk down with it.
  detachResp <-
    liftIO $
      runActionAsSubtask state (DiskDetachByDisk vmId diskId) parentTaskId
  case detachResp of
    RespDiskOk -> pure ()
    RespError err -> logWarnN $ "detach artifact drive: " <> err
    _ -> logWarnN "detach artifact drive: unexpected response"
  -- 2. For ifExists: overwrite, delete the now-stale existing disk
  --    so the freshly baked artifact can take its name. The pre-bake
  --    check already verified the disk wasn't attached; this still
  --    refuses if a new VM has attached during the bake.
  overwriteResult <- deleteOverwriteTargetIfNeeded state parentTaskId target
  case overwriteResult of
    Left err -> pure $ Left err
    Right () -> publishArtifactCore state parentTaskId diskId target needFlatten

publishArtifactCore
  :: ServerState
  -> TaskId
  -> Int64
  -> BuildTarget
  -> Bool
  -> LoggingT IO (Either Text ())
publishArtifactCore state parentTaskId diskId target needFlatten = do
  -- Rename the disk in-DB.
  renameRes <-
    liftIO $ renameDiskByName state diskId (btName target)
  case renameRes of
    Left err -> pure $ Left $ "rename artifact: " <> err
    Right () -> do
      -- 3. Flatten if needed.
      flattenResult <-
        if needFlatten
          then do
            r <-
              liftIO $
                runActionAsSubtask
                  state
                  (DiskRebase diskId Nothing False)
                  parentTaskId
            case r of
              RespDiskOk -> pure $ Right ()
              RespError err -> pure $ Left $ "flatten artifact: " <> err
              _ -> pure $ Left "flatten artifact: unexpected response"
          else pure $ Right ()
      case flattenResult of
        Left err -> pure $ Left err
        Right () -> do
          -- 4. Compact if asked.
          when (btCompact target) $ compactDisk state diskId
          -- 5. Relocate to the target's `path:` if set, else to
          --    `<basePath>/<name>.<ext>` (out of the bake VM's dir).
          relocateArtifact state diskId target

-- | Move the artifact qcow2 from wherever the bake VM left it to
-- the location implied by `BuildTarget` ('btPath' / 'btName' /
-- 'btFormat'), then update the 'DiskImage' row's 'filePath' so
-- subsequent lookups resolve to the new location.
--
-- A no-op when the resolved destination matches the current path.
-- Falls back to @copy + delete@ if `renameFile` raises (typically
-- @EXDEV@ — the destination is on a different filesystem from the
-- daemon's disk base; happens with absolute `path:` values).
--
-- The bake-VM's now-empty parent directory is removed best-effort;
-- failures are swallowed (the standard cleanup pass picks up the
-- slack if siblings remain).
relocateArtifact :: ServerState -> Int64 -> BuildTarget -> LoggingT IO (Either Text ())
relocateArtifact state diskId target = do
  basePath <- liftIO $ getEffectiveBasePath (ssQemuConfig state)
  let pool = ssDbPool state
      key = toSqlKey diskId :: DiskImageId
  -- Build artifacts have exactly one placement (the bake VM's
  -- node). Read the row, do the move on that node's filesystem,
  -- and rewrite the placement's path on success.
  mPlacement <- liftIO $ runSqlPool (listDiskImageNodes key) pool
  case mPlacement of
    [] -> pure $ Left "relocate: disk has no recorded placement"
    (Entity _ row : _) -> do
      let nid = diskImageNodeNodeId row
      current <- liftIO $ resolveDiskPath pool (ssQemuConfig state) key nid
      let ext = T.unpack (enumToText (btFormat target))
          fileName = T.unpack (btName target) <> "." <> ext
          desired = resolveDiskFilePathPure basePath (btPath target) fileName
      if current == desired
        then pure $ Right ()
        else do
          logInfoN $ "relocating artifact: " <> T.pack current <> " -> " <> T.pack desired
          liftIO $ createDirectoryIfMissing True (takeDirectory desired)
          moveRes <- liftIO (try (renameFile current desired) :: IO (Either IOError ()))
          case moveRes of
            Right () -> finalize nid basePath current desired
            Left _ -> do
              -- EXDEV (cross-device) or similar: copy then delete.
              copyRes <-
                liftIO
                  ( try
                      (copyFile current desired >> removeFile current)
                      :: IO (Either IOError ())
                  )
              case copyRes of
                Left e -> pure $ Left $ "relocate failed: " <> T.pack (show e)
                Right () -> finalize nid basePath current desired
  where
    finalize nid basePath oldPath newPath = do
      -- Record the new path in the DiskImageNode row so future
      -- 'resolveDiskPath' calls land at the relocated artifact.
      let stored =
            if (basePath ++ "/") `Data.List.isPrefixOf` newPath
              then T.pack (drop (length basePath + 1) newPath)
              else T.pack newPath
      liftIO $
        runSqlPool
          (recordDiskImageNode (toSqlKey diskId) nid stored)
          (ssDbPool state)
      -- Best-effort: drop the now-empty bake-VM directory.
      _ <-
        liftIO
          (try (removeDirectory (takeDirectory oldPath)) :: IO (Either SomeException ()))
      pure $ Right ()

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
  -> BuildTarget
  -> LoggingT IO (Either Text (Maybe Int64))
checkIfExistsPreBake state target = do
  let pool = ssDbPool state
      name = btName target
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
  -> BuildTarget
  -> LoggingT IO (Either Text ())
deleteOverwriteTargetIfNeeded state parentTaskId target
  | btIfExists target /= IfExistsOverwrite = pure $ Right ()
  | otherwise = do
      let pool = ssDbPool state
      mExisting <- liftIO $ runSqlPool (getBy (UniqueDiskImageName (btName target))) pool
      case mExisting of
        Nothing -> pure $ Right ()
        Just (Entity existingId _) -> do
          attachedVms <- liftIO $ runSqlPool (vmsAttachedToDisk existingId) pool
          if null attachedVms
            then do
              logInfoN $ "target.ifExists: deleting existing disk '" <> btName target <> "' (overwrite)"
              resp <-
                liftIO $
                  runActionAsSubtask state (DiskDelete (fromSqlKey existingId)) parentTaskId
              case resp of
                RespDiskOk -> pure $ Right ()
                RespError err ->
                  pure $ Left $ "target.ifExists: delete existing disk: " <> err
                _ -> pure $ Left "target.ifExists: delete existing disk: unexpected response"
            else
              pure $
                Left $
                  "target.ifExists: disk '"
                    <> btName target
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

renameDiskByName :: ServerState -> Int64 -> Text -> IO (Either Text ())
renameDiskByName state diskId newName = do
  case validateName "Disk" newName of
    Left err -> pure $ Left err
    Right () -> do
      mExisting <- runSqlPool (getBy (UniqueDiskImageName newName)) (ssDbPool state)
      case mExisting of
        Just _ -> pure $ Left $ "disk name '" <> newName <> "' already in use"
        Nothing -> do
          runSqlPool
            (update (toSqlKey diskId :: DiskImageId) [DiskImageName =. newName])
            (ssDbPool state)
          pure $ Right ()

-- | Compact a qcow2 by running @qemu-img convert -O qcow2@ in place (atomic
-- via temp file + rename). On any failure this logs at @warn@ and returns
-- without raising — compaction is a size optimisation, not a correctness
-- requirement.
compactDisk :: ServerState -> Int64 -> LoggingT IO ()
compactDisk state diskId = do
  -- TODO(multi-node Phase 3): refine pickNodeForDisk with
  -- DiskImageNode-aware placement instead of first-online-node.
  mNid <- liftIO $ pickNodeForDisk state
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
-- Plain @VmDelete deleteDisks=True@ is too aggressive: 'getExclusiveDisks'
-- treats every disk attached only to this VM as fair game, including
-- user-registered shared disks (the OVMF firmware, the imported base
-- image of a one-off template) that the build did NOT create. Deleting
-- those would also unlink the underlying file from disk.
--
-- Instead we collect the bake VM's drives, classify them by whether the
-- disk's name starts with the @__build_@ prefix (which is how all
-- ephemeral disks created during this build are named), delete only the
-- ephemeral ones explicitly via 'DiskDelete', and then run @VmDelete@
-- with @deleteDisks=False@.
cleanupBakeVm :: ServerState -> TaskId -> Int64 -> IO ()
cleanupBakeVm state parentTaskId vmIdLong = do
  -- Best-effort stop first; VmDelete refuses while running.
  _ <- runActionAsSubtask state (VmStop vmIdLong) parentTaskId
  -- Find ephemeral disks attached to this VM (named __build_*).
  ephemeralIds <- runSqlPool (collectEphemeralDiskIds vmIdLong) (ssDbPool state)
  _ <- runActionAsSubtask state (VmDelete vmIdLong False) parentTaskId
  mapM_ (\did -> runActionAsSubtask state (DiskDelete did) parentTaskId) ephemeralIds

-- | Walk the bake VM's drives and return IDs of disks whose name starts
-- with @__build_@ — those were created by the build pipeline (overlay,
-- cloned firmware vars, generated cloud-init ISO, …) and are safe to
-- delete. Registered base disks and shared firmware are filtered out.
collectEphemeralDiskIds :: Int64 -> SqlPersistT IO [Int64]
collectEphemeralDiskIds vmIdLong = do
  drives <- selectList [DriveVmId ==. toSqlKey vmIdLong] []
  let diskKeys = map (driveDiskImageId . entityVal) drives
  fmap (map fromSqlKey . catEphemerals) (mapM lookupNamed diskKeys)
  where
    lookupNamed key = do
      mDisk <- get key
      pure (key, fmap diskImageName mDisk)
    catEphemerals = map fst . filter isEphemeral
    isEphemeral (_, Just name) = "__build_" `T.isPrefixOf` name
    isEphemeral _ = False

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
