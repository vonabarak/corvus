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
  )
where

import Control.Concurrent (threadDelay)
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
import Corvus.Qemu.GuestAgent (GuestExecResult (..), guestExec, guestExecWithStdin, guestPing)
import Corvus.Qemu.Image (ImageResult (..), getImageSizeMb, rebaseImage)
import Corvus.Schema.Build
import Corvus.Types
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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

-- | Parse YAML, then run each build in sequence. Returns 'RespBuildResult'
-- with one entry per build (including failed ones, so partial successes are
-- visible to the caller).
handleBuildExecute :: ServerState -> TaskId -> Text -> IO Response
handleBuildExecute state parentTaskId yamlContent = runServerLogging state $ do
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
          results <- mapM (runOneBuildLogged state parentTaskId) (bcBuilds config)
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

runOneBuildLogged :: ServerState -> TaskId -> Build -> LoggingT IO BuildOne
runOneBuildLogged state parentTaskId b = do
  logInfoN $ "Starting build: " <> buildName b
  result <- runOneBuild state parentTaskId b
  case result of
    Right diskId -> do
      logInfoN $ "Build '" <> buildName b <> "' completed; artifact disk #" <> T.pack (show diskId)
      pure
        BuildOne
          { boName = buildName b
          , boArtifactDiskId = Just diskId
          , boError = Nothing
          }
    Left err -> do
      logWarnN $ "Build '" <> buildName b <> "' failed: " <> err
      pure
        BuildOne
          { boName = buildName b
          , boArtifactDiskId = Nothing
          , boError = Just err
          }

-- | Run a single build, returning the published artifact disk id or an error.
-- The build's @cleanup:@ mode controls whether ephemeral resources are torn
-- down on failure. The artifact disk's destructor is detached on success.
runOneBuild :: ServerState -> TaskId -> Build -> LoggingT IO (Either Text Int64)
runOneBuild state parentTaskId b = do
  startTime <- liftIO getCurrentTime
  stack <- liftIO newCleanupStack
  outcome <- withCleanup (buildCleanup b) stack (runOneBuildBody state parentTaskId stack startTime b)
  case outcome of
    Right inner -> pure inner
    Left ex -> pure $ Left $ "exception: " <> T.pack (show ex)

runOneBuildBody
  :: ServerState
  -> TaskId
  -> CleanupStack
  -> UTCTime
  -> Build
  -> LoggingT IO (Either Text Int64)
runOneBuildBody state parentTaskId stack startTime b = do
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
                      provResult <- runProvisioners state vmIdLong b startTime
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

runProvisioners
  :: ServerState
  -> Int64
  -> Build
  -> UTCTime
  -> LoggingT IO (Either Text ())
runProvisioners state vmId b startTime = go (buildProvisioners b)
  where
    go [] = do
      -- Implicit final step: write provenance file.
      let info =
            renderBuildInfo
              (buildName b)
              startTime
              (buildTemplate b)
              (T.pack (Version.showVersion version))
      writeProvenance state vmId info
    go (p : ps) = do
      r <- runProvisioner state vmId p
      case r of
        Left err -> pure $ Left err
        Right () -> go ps

runProvisioner :: ServerState -> Int64 -> Provisioner -> LoggingT IO (Either Text ())
runProvisioner state vmId p = case p of
  ProvShell sh -> case shellInline sh of
    Nothing -> pure $ Left "shell: missing inline body (client-side bug)"
    Just body -> do
      let envPrefix = T.concat (map (\(k, v) -> "export " <> k <> "=" <> shellQuote v <> "; ") (shellEnv sh))
          workdirPrefix = case shellWorkdir sh of
            Just d -> "cd " <> shellQuote d <> " && "
            Nothing -> ""
          maxPolls = case shellTimeoutSec sh of
            Just s -> max 60 (s * 10)
            Nothing -> 6000 -- 10 minutes default
          fullCmd = envPrefix <> workdirPrefix <> body
      logInfoN "shell: running"
      result <-
        liftIO $
          guestExecWithStdin
            (ssGuestAgentConns state)
            (ssQemuConfig state)
            vmId
            fullCmd
            BS.empty
            maxPolls
      classifyExec "shell" result
  ProvFile fp -> case fileContentBase64 fp of
    Nothing -> pure $ Left "file: missing content (client-side bug)"
    Just contentB64 -> do
      let mode = fromMaybe "0644" (fileMode fp)
          dest = shellQuote (fileTo fp)
          -- Use printf to deliver base64 payload then base64 -d into the
          -- target. printf vs echo because echo's -n behaviour is
          -- shell-dependent; printf is portable.
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
      logInfoN $ "file: writing " <> fileTo fp
      result <-
        liftIO $
          guestExecWithStdin
            (ssGuestAgentConns state)
            (ssQemuConfig state)
            vmId
            cmd
            payload
            600
      classifyExec "file" result
  ProvWaitFor wf -> waitFor state vmId wf
  ProvReboot rb -> rebootGuest state vmId (rebootTimeoutSec rb)

-- | Lightweight shell quoting: wrap in single quotes and escape any single
-- quotes inside. Sufficient for paths and env values that don't contain
-- newlines.
shellQuote :: Text -> Text
shellQuote t = "'" <> T.replace "'" "'\\''" t <> "'"

classifyExec :: Text -> GuestExecResult -> LoggingT IO (Either Text ())
classifyExec lbl r = case r of
  GuestExecSuccess code stdout stderr ->
    if code == 0
      then do
        unless (T.null stdout) $ logInfoN $ lbl <> " stdout: " <> truncateForLog stdout
        unless (T.null stderr) $ logInfoN $ lbl <> " stderr: " <> truncateForLog stderr
        pure $ Right ()
      else
        pure $
          Left $
            lbl <> " exited " <> T.pack (show code) <> ": " <> truncateForLog (joinOutputs stdout stderr)
  GuestExecError msg -> pure $ Left $ lbl <> " agent error: " <> msg
  GuestExecConnectionFailed msg -> pure $ Left $ lbl <> " agent connection failed: " <> msg

joinOutputs :: Text -> Text -> Text
joinOutputs out err
  | T.null err = out
  | T.null out = err
  | otherwise = out <> " | stderr: " <> err

truncateForLog :: Text -> Text
truncateForLog t
  | T.length t > 4096 = T.take 4096 t <> "...[truncated]"
  | otherwise = t

waitFor :: ServerState -> Int64 -> WaitFor -> LoggingT IO (Either Text ())
waitFor state vmId w = case w of
  WaitForPing timeoutSec ->
    loopUntil timeoutSec "guest-agent ping" $
      liftIO $
        guestPing (ssGuestAgentConns state) (ssQemuConfig state) vmId
  WaitForFile path timeoutSec ->
    loopUntil timeoutSec ("file " <> path) $ do
      r <-
        liftIO $
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
        liftIO $
          guestExec
            (ssGuestAgentConns state)
            (ssQemuConfig state)
            vmId
            probe
      pure $ case r of
        GuestExecSuccess 0 _ _ -> True
        _ -> False
  where
    loopUntil :: Int -> Text -> LoggingT IO Bool -> LoggingT IO (Either Text ())
    loopUntil totalSec lbl probe = go 0
      where
        go elapsed
          | elapsed >= totalSec =
              pure $ Left $ "wait-for " <> lbl <> ": timed out after " <> T.pack (show totalSec) <> "s"
          | otherwise = do
              ok <- probe
              if ok
                then pure $ Right ()
                else do
                  liftIO $ threadDelay 2000000
                  go (elapsed + 2)

rebootGuest :: ServerState -> Int64 -> Int -> LoggingT IO (Either Text ())
rebootGuest state vmId timeoutSec = do
  logInfoN "reboot: requesting via guest-exec"
  -- Use guest-exec rather than guest-shutdown so the agent can come back up
  -- afterwards (guest-shutdown 'powerdown' won't reboot).
  result <-
    liftIO $
      guestExec
        (ssGuestAgentConns state)
        (ssQemuConfig state)
        vmId
        "(sleep 1; /sbin/reboot || /usr/sbin/reboot || reboot) >/dev/null 2>&1 &"
  case result of
    GuestExecConnectionFailed msg -> pure $ Left $ "reboot dispatch: " <> msg
    _ -> do
      -- Wait for the agent socket to disappear briefly, then come back.
      -- Simpler heuristic: keep polling until ping succeeds again.
      liftIO $ threadDelay 5000000 -- 5s grace
      waitFor state vmId (WaitForPing timeoutSec)

writeProvenance :: ServerState -> Int64 -> Text -> LoggingT IO (Either Text ())
writeProvenance state vmId info = do
  logInfoN "provenance: writing /etc/corvus-build-info"
  let payload = TE.encodeUtf8 info
      cmd = "install -d /etc && cat > /etc/corvus-build-info && chmod 0644 /etc/corvus-build-info"
  result <-
    liftIO $
      guestExecWithStdin
        (ssGuestAgentConns state)
        (ssQemuConfig state)
        vmId
        cmd
        payload
        600
  classifyExec "provenance" result

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
