{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Provisioner execution for the build pipeline.
--
-- Runs user provisioners (shell, file, wait-for, reboot) against
-- the bake VM via the nodeagent's guest-exec RPC.
module Corvus.Handlers.Build.Provisioner
  ( provisionerOutputCap
    -- | Run every provisioner in order, stopping on the first failure.
  , runProvisioners
  , runProvisioner
  , buildCorvusEnv
  , buildShellCommand
  , powerShellBootstrap
  , provKind
  , provDesc
  , strategyName
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN)
import Corvus.Action (Action (..), ActionContext (..), cancelRemainingSubtasks, mkActionContext)
import Corvus.Handlers.Build.Qga
  ( agentGuestExec
  , agentGuestExecWithTail
  , agentGuestPing
  , agentGuestReboot
  )
import Corvus.Handlers.GuestFile (GuestFileUpload (..))
import Corvus.Handlers.Vm.Query (getVmDetails)
import Corvus.Model
import Corvus.Node.GuestAgent (GuestExecResult (..))
import Corvus.Protocol (Response (..))
import Corvus.Protocol.Build (BuildEvent (..), BuildSink)
import Corvus.Protocol.Vm (vdName, vdVsockCid)
import Corvus.Rpc.FileTransfer (bytesSource)
import Corvus.Schema.Build
import Corvus.Types
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (getCurrentTime)
import qualified Data.Version as Version
import Database.Persist (insert, update)
import Database.Persist.Sql (runSqlPool, (=.))
import GHC.Clock (getMonotonicTimeNSec)
import Paths_corvus (version)
import System.Timeout (timeout)

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
  -> LoggingT IO (Either Text ())
runProvisioners state parentTaskId sink vmId b = do
  cEnv <- liftIO $ buildCorvusEnv state b vmId parentTaskId
  let sd = buildShellDefaults b
      provs = buildProvisioners b
      go _ [] = pure $ Right ()
      go idx (p : ps) = do
        r <- runProvisioner state parentTaskId sink vmId sd cEnv idx p
        case r of
          Left err -> pure $ Left err
          Right () -> go (idx + 1) ps
  go 1 provs

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
strategyName BuildStrategyOfflineOverlay = "offline-overlay"

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
  bodyResult <- liftIO $ try $ case p of
    ProvReboot rb -> do
      sink (StepOutput stepIdx "rebooting via guest-shutdown")
      result <- actionExecute (mkActionContext state taskKey "system") (BuildGuestReboot vmId (rebootTimeoutSec rb))
      pure $ case result of
        RespOk -> Right (Just "guest reboot confirmed; guest-ping ready")
        RespError err -> Left (err, Just err)
        other -> Left (T.pack (show other), Just (T.pack (show other)))
    _ -> runProvisionerBody (mkActionContext state taskKey "system") vmId sd cEnv stepIdx sink p
  finishedAt <- liftIO getCurrentTime
  let (orchResult, taskMsg, resultStatus) = case bodyResult of
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
          , TaskResult =. resultStatus
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
  :: ActionContext
  -> Int64
  -> ShellDefaults
  -> [(Text, Text)]
  -> Int
  -> BuildSink
  -> Provisioner
  -> IO (Either (Text, Maybe Text) (Maybe Text))
runProvisionerBody ctx vmId sd cEnv stepIdx sink p =
  let state = acState ctx
   in case p of
        ProvShell sh -> case shellInline sh of
          Nothing ->
            pure $
              Left
                ( "shell: missing inline body (client-side bug)"
                , Just "shell: missing inline body (client-side bug)"
                )
          Just body -> do
            let timeoutSec = fromMaybe 600 (shellTimeoutSec sh)
                script = buildShellCommand sd cEnv sh body
                scriptPath = "C:\\Windows\\Temp\\corvus-build-" <> T.pack (show vmId) <> "-" <> T.pack (show stepIdx) <> ".ps1"
                fullCmd = case shellInterpreter sh of
                  ShellSh -> script
                  ShellPowerShell -> powerShellBootstrap scriptPath
            bufRef <- newIORef BS.empty
            totalRef <- newIORef (0 :: Int)
            let onLine line = do
                  sink (StepOutput stepIdx line)
                  accumulateLine bufRef totalRef line
            staged <- case shellInterpreter sh of
              ShellSh -> pure (Right ())
              ShellPowerShell -> uploadBytes ctx vmId scriptPath (TE.encodeUtf8 script)
            result <- case staged of
              Left err -> pure (GuestExecError ("script upload: " <> err))
              Right () -> agentGuestExecWithTail state vmId fullCmd BS.empty (fromIntegral timeoutSec) onLine
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
          Just contentB64 -> case B64.decode (TE.encodeUtf8 contentB64) of
            Left err -> pure $ Left ("file: invalid base64 content", Just (T.pack err))
            Right payload -> do
              let dest = shellQuote (fileTo fp)
                  mode = shellQuote (fromMaybe "0644" (fileMode fp))
                  mkdir = "umask 022; install -d \"$(dirname -- " <> dest <> ")\""
              sink (StepOutput stepIdx ("writing " <> fileTo fp))
              prepared <- agentGuestExec state vmId mkdir 600
              case prepared of
                GuestExecSuccess 0 _ _ -> do
                  staged <- uploadBytes ctx vmId (fileTo fp) payload
                  case staged of
                    Left err -> pure $ Left ("file upload: " <> err, Just err)
                    Right () -> do
                      result <- agentGuestExec state vmId ("chmod -- " <> mode <> " " <> dest) 600
                      pure $ classifyAtomic "file" (fileTo fp) result
                other -> pure $ classifyAtomic "file parent directory" (fileTo fp) other
        ProvWaitFor wf -> waitForIO state stepIdx sink vmId wf
        ProvReboot _ -> pure $ Left ("reboot requires an Action context", Nothing)

uploadBytes :: ActionContext -> Int64 -> Text -> BS.ByteString -> IO (Either Text ())
uploadBytes ctx vmId path bytes = do
  source <- bytesSource bytes
  result <- actionExecute ctx (GuestFileUpload vmId path source)
  pure $ case result of
    RespGuestFileTransferred _ -> Right ()
    other -> Left (T.pack (show other))

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
  WaitForScript sh _ -> provDesc (ProvShell sh)
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
buildShellCommand sd corvusEnv sh body
  | shellInterpreter sh == ShellPowerShell =
      maybe "" (<> "\n") (sdPreamble sd)
        <> T.concat (map psEnv (corvusEnv ++ sdEnv sd ++ shellEnv sh))
        <> maybe "" (\d -> "Set-Location -LiteralPath " <> psQuote d <> "\n") (shellWorkdir sh)
        <> body
  | otherwise =
      preambleBlock
        <> autoEnvBlock
        <> stepEnvBlock
        <> workdirBlock
        <> body
  where
    psEnv (key, value) = "[Environment]::SetEnvironmentVariable(" <> psQuote key <> ", " <> psQuote value <> ", 'Process')\n"
    psQuote value = "'" <> T.replace "'" "''" value <> "'"
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
  WaitForScript sh timeoutSec -> case shellInline sh of
    Nothing -> pure $ Left ("wait-for: missing inline body", Nothing)
    Just body -> do
      let script = buildShellCommand emptyShellDefaults [] sh body
          cmd = case shellInterpreter sh of
            ShellSh -> script
            ShellPowerShell ->
              "powershell.exe -NoLogo -NoProfile -NonInteractive -ExecutionPolicy Bypass -EncodedCommand "
                <> TE.decodeUtf8 (B64.encode (TE.encodeUtf16LE script))
      loopUntil timeoutSec "script" $ do
        r <- agentGuestExec state vmId cmd 10
        pure $ case r of
          GuestExecSuccess 0 _ _ -> True
          _ -> False
  where
    loopUntil totalSec lbl probe = do
      started <- getMonotonicTimeNSec
      let deadline = started + fromIntegral totalSec * 1000000000
      result <- timeout (totalSec * 1000000) (go deadline)
      pure $ fromMaybe (Left (err, Just err)) result
      where
        err = "wait-for " <> lbl <> ": timed out after " <> T.pack (show totalSec) <> "s"
        -- RPC error wrappers can turn a timeout exception into a failed probe.
        -- Check the monotonic deadline too, so such failures cannot restart it.
        go deadline = do
          now <- getMonotonicTimeNSec
          if now >= deadline
            then pure $ Left (err, Just err)
            else do
              ok <- probe
              finished <- getMonotonicTimeNSec
              if finished >= deadline
                then pure $ Left (err, Just err)
                else
                  if ok
                    then pure $ Right (Just (lbl <> " ok"))
                    else do
                      sink (StepOutput stepIdx ("waiting for " <> lbl))
                      threadDelay 2000000
                      go deadline

-- Execute the UTF-8 script staged through QGA file operations, avoiding both
-- Windows command-line quoting and payload limits. Delete it on completion.
powerShellBootstrap :: Text -> Text
powerShellBootstrap path =
  "powershell.exe -NoLogo -NoProfile -NonInteractive -ExecutionPolicy Bypass -EncodedCommand "
    <> TE.decodeUtf8 (B64.encode (TE.encodeUtf16LE bootstrap))
  where
    literal = "'" <> T.replace "'" "''" path <> "'"
    bootstrap =
      "try { $ErrorActionPreference = 'Stop'; $ProgressPreference = 'SilentlyContinue'; [Console]::OutputEncoding = New-Object System.Text.UTF8Encoding; $script = [IO.File]::ReadAllText("
        <> literal
        <> ", [Text.Encoding]::UTF8); & ([ScriptBlock]::Create($script)); exit 0 } catch { [Console]::Error.WriteLine($_.ToString()); exit 1 } finally { Remove-Item -LiteralPath "
        <> literal
        <> " -Force -ErrorAction SilentlyContinue }"

data BuildGuestReboot = BuildGuestReboot Int64 Int

instance Action BuildGuestReboot where
  actionSubsystem _ = SubBuild
  actionCommand _ = "reboot"
  actionEntityId (BuildGuestReboot vmId _) = Just (fromIntegral vmId)
  actionExecute ctx (BuildGuestReboot vmId seconds) = do
    result <- agentGuestReboot (acState ctx) vmId (fromIntegral seconds)
    pure $ either RespError (const RespOk) result
