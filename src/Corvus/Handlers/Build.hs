{-# LANGUAGE FlexibleContexts #-}
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
  , handleBuildExecute
  , runBuildPipeline

    -- * Streaming sink
  , BuildSink
  , noOpBuildSink
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Build.Cleanup (CleanupStack, newCleanupStack, push, withCleanup)
import Corvus.Handlers.Build.Provenance (renderBuildInfo)
import Corvus.Handlers.Disk (DiskCreate (..), DiskDelete (..), DiskRebase (..))
import Corvus.Handlers.Disk.Attach (DiskAttach (..), DiskDetachByDisk (..))
import Corvus.Handlers.Disk.Path (resolveDiskPath)
import Corvus.Handlers.Resolve (validateName)
import Corvus.Handlers.Template (TemplateInstantiate (..))
import Corvus.Handlers.Vm (VmDelete (..), VmStart (..), VmStop (..))
import Corvus.Model
import Corvus.Protocol
import Corvus.Qemu.GuestAgent (GuestExecResult (..), guestExec, guestExecWithStdin, guestExecWithTail, guestPing)
import Corvus.Qemu.Image (ImageResult (..), getImageSizeMb, rebaseImage)
import Corvus.Schema.Build
import Corvus.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Version as Version
import Data.Yaml (decodeEither')
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, toSqlKey)
import Paths_corvus (version)

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
-- the same response shape as before. Per-build 'BuildEnd' events are
-- emitted by 'runOneBuildLogged'; the caller is responsible for
-- emitting the terminating 'PipelineEnd' once the response is in hand.
runBuildPipeline :: ServerState -> TaskId -> BuildSink -> Text -> IO Response
runBuildPipeline state parentTaskId sink yamlContent = runServerLogging state $ do
  case decodeEither' (TE.encodeUtf8 yamlContent) of
    Left err -> do
      let msg = T.pack (show err)
      logWarnN $ "Failed to parse build YAML: " <> msg
      pure $ RespError msg
    Right (config :: BuildConfig) ->
      case validateConfig config of
        Left err -> do
          logWarnN $ "Build config validation failed: " <> err
          pure $ RespError err
        Right () -> do
          results <- mapM (runOneBuildLogged state parentTaskId sink) (bcBuilds config)
          pure $ RespBuildResult (BuildResult results)

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

validateConfig :: BuildConfig -> Either Text ()
validateConfig cfg = do
  let names = map buildName (bcBuilds cfg)
  case findDuplicate names of
    Just d -> Left $ "Duplicate build name: " <> d
    Nothing -> pure ()
  mapM_ validateBuild (bcBuilds cfg)
  where
    findDuplicate :: [Text] -> Maybe Text
    findDuplicate [] = Nothing
    findDuplicate (x : xs)
      | x `elem` xs = Just x
      | otherwise = findDuplicate xs

validateBuild :: Build -> Either Text ()
validateBuild b = do
  validateName "Build" (buildName b)
  validateName "Target disk" (btName (buildTarget b))
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
      flavor = buildFlavor b

  -- 1. Resolve template
  templateIdResult <- liftIO $ resolveTemplateIdOrErr state (buildTemplate b)
  case templateIdResult of
    Left err -> pure $ Left err
    Right (templateId, hasGuestAgent) ->
      if not hasGuestAgent
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
                (TemplateInstantiate templateId bakeVmName)
                parentTaskId
          case mVmId of
            RespTemplateInstantiated vmIdLong -> do
              liftIO $
                push stack "bake-vm" $
                  cleanupBakeVm state parentTaskId vmIdLong
              -- 3. From-scratch flavor: create empty target disk + attach
              targetSetup <- case flavor of
                FlavorOverlay -> do
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
                          , True -- needs flatten on overlay flavor
                          )
                FlavorFromScratch -> do
                  let sizeMb = fromIntegral (btSizeGb target) * 1024
                  diskResp <-
                    liftIO $
                      runActionAsSubtask
                        state
                        (DiskCreate targetTmpName (btFormat target) sizeMb Nothing)
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
                  -- 4. Start the bake VM. VmStart blocks until the guest
                  --    agent first-pings, so on success we know provisioners
                  --    can run.
                  startResp <-
                    liftIO $
                      runActionAsSubtask state (VmStart vmIdLong) parentTaskId
                  case classifyStartResp startResp of
                    Left err -> pure $ Left $ "start bake VM: " <> err
                    Right () -> do
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
                              -- 7. Detach artifact, rename, optional flatten + compact
                              publishResult <-
                                publishArtifact
                                  state
                                  parentTaskId
                                  vmIdLong
                                  artifactDiskId
                                  (btName target)
                                  needFlatten
                                  (btCompact target)
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
runProvisioners state parentTaskId sink vmId b startTime =
  go 1 (buildProvisioners b)
  where
    go idx [] = do
      -- Implicit final step: write provenance file.
      let info =
            renderBuildInfo
              (buildName b)
              startTime
              (buildTemplate b)
              (T.pack (Version.showVersion version))
      runProvisioner state parentTaskId sink vmId idx (provenanceProvisioner info)
    go idx (p : ps) = do
      r <- runProvisioner state parentTaskId sink vmId idx p
      case r of
        Left err -> pure $ Left err
        Right () -> go (idx + 1) ps

-- | Synthesize a 'ProvFile' that writes the provenance file. Reusing
-- the file provisioner means provenance gets the same per-step subtask
-- + event treatment as the user's provisioners.
provenanceProvisioner :: Text -> Provisioner
provenanceProvisioner info =
  ProvFile
    FileProv
      { fileFrom = Nothing
      , fileContentBase64 = Just (TE.decodeUtf8 (B64.encode (TE.encodeUtf8 info)))
      , fileTo = "/etc/corvus-build-info"
      , fileMode = Just "0644"
      }

-- | Run a single provisioner: insert a subtask row, invoke the body,
-- finalize the row with the result + (cropped) output, emit the
-- bracketing 'StepStart'/'StepEnd' events.
runProvisioner
  :: ServerState
  -> TaskId
  -> BuildSink
  -> Int64
  -> Int
  -> Provisioner
  -> LoggingT IO (Either Text ())
runProvisioner state parentTaskId sink vmId stepIdx p = do
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
              }
        )
        (ssDbPool state)
  bodyResult <- liftIO $ try $ runProvisionerBody state vmId stepIdx sink p
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
  -> Int
  -> BuildSink
  -> Provisioner
  -> IO (Either (Text, Maybe Text) (Maybe Text))
runProvisionerBody state vmId stepIdx sink p = case p of
  ProvShell sh -> case shellInline sh of
    Nothing ->
      pure $
        Left
          ( "shell: missing inline body (client-side bug)"
          , Just "shell: missing inline body (client-side bug)"
          )
    Just body -> do
      let envPrefix =
            T.concat
              ( map
                  (\(k, v) -> "export " <> k <> "=" <> shellQuote v <> "; ")
                  (shellEnv sh)
              )
          workdirPrefix = case shellWorkdir sh of
            Just d -> "cd " <> shellQuote d <> " && "
            Nothing -> ""
          maxPolls = case shellTimeoutSec sh of
            Just s -> max 60 (s * 10)
            Nothing -> 6000 -- 10 minutes default
          fullCmd = envPrefix <> workdirPrefix <> body
      bufRef <- newIORef BS.empty
      totalRef <- newIORef (0 :: Int)
      let onLine line = do
            sink (StepOutput stepIdx line)
            accumulateLine bufRef totalRef line
      result <-
        guestExecWithTail
          (ssGuestAgentConns state)
          (ssQemuConfig state)
          vmId
          fullCmd
          maxPolls
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
        guestExecWithStdin
          (ssGuestAgentConns state)
          (ssQemuConfig state)
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
      guestPing (ssGuestAgentConns state) (ssQemuConfig state) vmId
  WaitForFile path timeoutSec ->
    loopUntil timeoutSec ("file " <> path) $ do
      r <-
        guestExec
          (ssGuestAgentConns state)
          (ssQemuConfig state)
          vmId
          ("test -e " <> shellQuote path)
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
      r <-
        guestExec
          (ssGuestAgentConns state)
          (ssQemuConfig state)
          vmId
          probe
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
    guestExec
      (ssGuestAgentConns state)
      (ssQemuConfig state)
      vmId
      "(sleep 1; /sbin/reboot || /usr/sbin/reboot || reboot) >/dev/null 2>&1 &"
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
  -> Text
  -- ^ desired final name
  -> Bool
  -- ^ flatten? (overlay flavor)
  -> Bool
  -- ^ compact?
  -> LoggingT IO (Either Text ())
publishArtifact state parentTaskId vmId diskId finalName needFlatten compact = do
  -- 1. Detach the drive so VmDelete doesn't take the disk down with it.
  detachResp <-
    liftIO $
      runActionAsSubtask state (DiskDetachByDisk vmId diskId) parentTaskId
  case detachResp of
    RespDiskOk -> pure ()
    RespError err -> logWarnN $ "detach artifact drive: " <> err
    _ -> logWarnN "detach artifact drive: unexpected response"
  -- 2. Rename the disk in-DB.
  renameRes <-
    liftIO $ renameDiskByName state diskId finalName
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
          when compact $ compactDisk state diskId
          pure $ Right ()

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
  mDisk <- liftIO $ runSqlPool (get (toSqlKey diskId :: DiskImageId)) (ssDbPool state)
  case mDisk of
    Nothing -> logWarnN "compact: disk vanished"
    Just disk -> do
      logInfoN "compact: rebasing onto self with -c (no-op flatten)"
      path <- liftIO $ resolveDiskPath (ssQemuConfig state) disk
      -- A no-op rebase (Nothing → Nothing) effectively rewrites the image
      -- through qemu-img, dropping unused clusters. The rebaseImage helper
      -- already handles the in-place pass.
      result <- liftIO $ rebaseImage path Nothing False
      case result of
        ImageSuccess -> do
          mSize <- liftIO $ getImageSizeMb path
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
