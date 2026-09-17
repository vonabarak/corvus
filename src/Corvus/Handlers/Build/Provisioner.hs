{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Provisioner execution for the build pipeline.
--
-- Runs user provisioners (shell, file, wait-for, reboot) against
-- the bake VM via the nodeagent's guest-exec RPC.
module Corvus.Handlers.Build.Provisioner
  ( provisionerOutputCap
  , runProvisioners
  , runProvisioner
  , runProvisionerBody
  , buildCorvusEnv
  , buildShellCommand
  , provKind
  , provDesc
  , strategyName
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN)
import Corvus.Handlers.Build.Qga
  ( agentGuestExec
  , agentGuestExecWithStdin
  , agentGuestExecWithTail
  , agentGuestPing
  )
import Corvus.Action (cancelRemainingSubtasks)
import Corvus.Handlers.Vm (getVmDetails)
import Corvus.Model
import Corvus.Node.GuestAgent (GuestExecResult (..))
import Corvus.Protocol.Build (BuildEvent (..), BuildSink)
import Corvus.Protocol.Vm (vdName, vdVsockCid)
import Corvus.Schema.Build
import Corvus.Types (ServerState)
import Paths_corvus (version)
import Corvus.Types
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Version as Version
import Database.Persist (insert, update)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey, (=.))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

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
  -- ^ starting step index (1-based; passing N+1 means "all cached, skip")
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
