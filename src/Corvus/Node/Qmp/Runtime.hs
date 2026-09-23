{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Runtime QMP operations.
module Corvus.Node.Qmp.Runtime where

import Control.Concurrent (threadDelay)
import Corvus.Model (CacheType (..), DriveFormat (..), DriveInterface (..), EnumText (..))
import Corvus.Node.Qmp.Transport (classifyQmpResponse, extractReplyLine, sendQmpCommand, sendQmpRaw)
import Corvus.Node.Qmp.Types (QmpMigrationStatus (..), QmpResult (..))
import Corvus.Node.QmpQQ (qmpQQ)
import Corvus.Node.Runtime (shellQuotePath)
import Corvus.Qemu.Config (QemuConfig)
import qualified Data.Aeson as A
import qualified Data.ByteString as BSWide
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

-- QMP Commands
--------------------------------------------------------------------------------

-- | Send graceful shutdown command via QMP
qmpShutdown :: QemuConfig -> Int64 -> IO QmpResult
qmpShutdown config vmId =
  sendQmpCommand config vmId [qmpQQ| { "execute": "system_powerdown" } |]

-- | Send continue command via QMP (resume paused VM)
qmpContinue :: QemuConfig -> Int64 -> IO QmpResult
qmpContinue config vmId =
  sendQmpCommand config vmId [qmpQQ| { "execute": "cont" } |]

-- | Send stop command via QMP (pause VM)
qmpStop :: QemuConfig -> Int64 -> IO QmpResult
qmpStop config vmId =
  sendQmpCommand config vmId [qmpQQ| { "execute": "stop" } |]

-- | Send the QMP @quit@ command, asking QEMU to exit cleanly. Used
-- by the save flow once the outgoing @migrate@ has reported
-- @completed@.
qmpQuit :: QemuConfig -> Int64 -> IO QmpResult
qmpQuit config vmId =
  sendQmpCommand config vmId [qmpQQ| { "execute": "quit" } |]

--------------------------------------------------------------------------------
-- Migration (save / load coordination)
--------------------------------------------------------------------------------

-- | Issue @migrate "exec:zstd -T0 > <path>"@. QMP returns
-- immediately on accepting the command; the actual transfer
-- happens asynchronously — poll 'qmpQueryMigrate' for completion.
--
-- The @exec:@ URI pipes the migration stream through @zstd@
-- (multi-threaded, default level 3) so the saved RAM image lands
-- compressed. For a typical guest with mostly-zero or
-- highly-redundant RAM this shrinks the on-disk file by ~5–10×
-- and shrinks the cross-host transfer in 'crv vm migrate' by the
-- same factor; CPU cost is comfortably below the network or
-- disk bandwidth on every realistic host.
--
-- The path is single-quoted via 'shellQuotePath' — QEMU spawns
-- the command via @\/bin\/sh -c@, so unquoted spaces or shell
-- metachars in @basePath@ would tear the command apart.
qmpMigrate :: QemuConfig -> Int64 -> FilePath -> IO QmpResult
qmpMigrate config vmId path =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "migrate",
        "arguments": {
          "uri": #{T.pack ("exec:zstd -T0 > " <> shellQuotePath path)}
        }
      }
    |]

-- | Issue @query-migrate@ and classify the response. Detection is
-- substring-based on the JSON payload — matches the existing
-- 'classifyQmpResponse' style and avoids pulling aeson into this
-- module. The QMP wire format guarantees the literal
-- @"status": "<value>"@ key appears in the @return@ object (modulo
-- whitespace, which QEMU does not emit).
qmpQueryMigrate :: QemuConfig -> Int64 -> IO (Either Text QmpMigrationStatus)
qmpQueryMigrate config vmId = do
  result <- sendQmpRaw config vmId [qmpQQ| { "execute": "query-migrate" } |]
  pure $ case result of
    Left e -> Left e
    Right response
      | BS.isInfixOf "\"error\"" response ->
          Left $ T.pack $ BS.unpack response
      | BS.isInfixOf "\"status\": \"completed\"" response ->
          Right MigCompleted
      | BS.isInfixOf "\"status\": \"failed\"" response ->
          Right (MigFailed (T.pack (BS.unpack response)))
      | BS.isInfixOf "\"status\": \"cancelled\"" response ->
          Right (MigFailed "migration cancelled")
      | BS.isInfixOf "\"status\": \"cancelling\"" response ->
          Right (MigFailed "migration cancelling")
      | BS.isInfixOf "\"status\": \"active\"" response
          || BS.isInfixOf "\"status\": \"setup\"" response
          || BS.isInfixOf "\"status\": \"device\"" response
          || BS.isInfixOf "\"status\": \"pre-switchover\"" response
          || BS.isInfixOf "\"status\": \"wait-unplug\"" response ->
          Right MigActive
      | BS.isInfixOf "\"status\": \"none\"" response ->
          Right MigInactive
      -- Anything else (e.g. postcopy variants we don't trigger):
      -- treat as still-running so the caller keeps polling. If it
      -- never resolves, the caller's own timeout fires.
      | otherwise -> Right MigActive

-- | Install a fresh SPICE password on a running VM via QMP. Uses
-- @connected: "keep"@ so an already-connected viewer is not dropped when
-- the password rotates.
qmpSetSpicePassword :: QemuConfig -> Int64 -> Text -> IO QmpResult
qmpSetSpicePassword config vmId password =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "set_password",
        "arguments": {
          "protocol": "spice",
          "password": #{password},
          "connected": "keep"
        }
      }
    |]

-- | Schedule the current SPICE password to expire. Accepts a relative
-- time in seconds (@+N@ semantics — "expire N seconds from now").
qmpExpireSpicePassword :: QemuConfig -> Int64 -> Int -> IO QmpResult
qmpExpireSpicePassword config vmId seconds = do
  let ttl = T.pack ("+" ++ show seconds)
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "expire_password",
        "arguments": {
          "protocol": "spice",
          "time": #{ttl}
        }
      }
    |]

-- | Inject a Ctrl+Alt+Del key combination via QMP @send-key@. This is
-- how 'crv vm view' delivers the escape-prefix @d@ command now that
-- the HMP monitor socket is exclusively held by the daemon's ring
-- buffer thread.
qmpSendCtrlAltDel :: QemuConfig -> Int64 -> IO QmpResult
qmpSendCtrlAltDel config vmId =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "send-key",
        "arguments": {
          "keys": [
            { "type": "qcode", "data": "ctrl" },
            { "type": "qcode", "data": "alt" },
            { "type": "qcode", "data": "delete" }
          ]
        }
      }
    |]

-- | Send a sequence of QEMU @qcode@ keys to the VM as a single chord
-- (all pressed and released together). For the @crv build@ installer
-- strategy this is used to dismiss the "Press any key to boot from CD"
-- prompt at UEFI firmware time. Use one call per key press; the QEMU
-- @qcode@ vocabulary covers @ret@, @esc@, @spc@, @tab@, @up@/@down@,
-- the alphanumerics, etc.
qmpSendKey :: QemuConfig -> Int64 -> Text -> IO QmpResult
qmpSendKey config vmId qcode =
  sendQmpCommand
    config
    vmId
    [qmpQQ|
      {
        "execute": "send-key",
        "arguments": {
          "keys": [
            { "type": "qcode", "data": #{qcode} }
          ]
        }
      }
    |]

--------------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Stats sampling

-- | One row of @query-blockstats@. Names match the QMP wire field
-- names for traceability: @bsrRdBytes@ / @bsrWrBytes@ are cumulative
-- since QEMU launch.
data BlockstatsRow = BlockstatsRow
  { bsrDevice :: !Text
  , bsrRdBytes :: !Word64
  , bsrWrBytes :: !Word64
  , bsrRdOps :: !Word64
  , bsrWrOps :: !Word64
  }
  deriving (Eq, Show)

-- | Issue @query-blockstats@ and decode the per-device counters.
-- Used by the agent's StatusPoller to populate @VmStats.drives@.
qmpQueryBlockstats :: QemuConfig -> Int64 -> IO (Either Text [BlockstatsRow])
qmpQueryBlockstats config vmId = do
  raw <- sendQmpRaw config vmId [qmpQQ| { "execute": "query-blockstats" } |]
  pure $ do
    bs <- raw
    line <- extractReplyLine bs
    case A.eitherDecodeStrict line of
      Left e -> Left (T.pack ("query-blockstats decode: " <> e))
      Right (BlockstatsReply rows) -> Right rows

-- | Issue @query-balloon@. Returns @Right (Just bytes)@ when the
-- VM has an active balloon device, @Right Nothing@ when the VM
-- has no balloon (QEMU error @DeviceNotActive@ / @CommandNotFound@),
-- or @Left@ for transport / decode failures.
qmpQueryBalloon :: QemuConfig -> Int64 -> IO (Either Text (Maybe Word64))
qmpQueryBalloon config vmId = do
  raw <- sendQmpRaw config vmId [qmpQQ| { "execute": "query-balloon" } |]
  pure $ do
    bs <- raw
    line <- extractReplyLine bs
    if BSWide.isInfixOf "\"error\"" line
      then Right Nothing
      else case A.eitherDecodeStrict line of
        Left e -> Left (T.pack ("query-balloon decode: " <> e))
        Right (BalloonReply actual) -> Right (Just actual)

newtype BlockstatsReply = BlockstatsReply [BlockstatsRow]

instance A.FromJSON BlockstatsReply where
  parseJSON = A.withObject "BlockstatsReply" $ \o ->
    BlockstatsReply <$> o A..: "return"

instance A.FromJSON BlockstatsRow where
  parseJSON = A.withObject "BlockstatsRow" $ \o -> do
    dev <- o A..: "device"
    stats <- o A..: "stats"
    BlockstatsRow dev
      <$> stats A..: "rd_bytes"
      <*> stats A..: "wr_bytes"
      <*> stats A..: "rd_operations"
      <*> stats A..: "wr_operations"

newtype BalloonReply = BalloonReply Word64

instance A.FromJSON BalloonReply where
  parseJSON = A.withObject "BalloonReply" $ \o -> do
    ret <- o A..: "return"
    BalloonReply <$> ret A..: "actual"

--------------------------------------------------------------------------------
-- QMP capability, readiness, and asynchronous jobs
--------------------------------------------------------------------------------

-- | Probe the set of QMP commands the running QEMU supports via
-- @query-commands@. Used to reject 'qmpSnapshotSave' / -Load /
-- -Delete cleanly on QEMU < 6.0 instead of producing a cryptic
-- @CommandNotFound@ deep in the job lifecycle.
qmpQueryCommands :: QemuConfig -> Int64 -> IO (Either Text (Set.Set Text))
qmpQueryCommands config vmId = do
  raw <- sendQmpRaw config vmId [qmpQQ| { "execute": "query-commands" } |]
  pure $ do
    bs <- raw
    line <- extractReplyLine bs
    case A.eitherDecodeStrict line of
      Left e -> Left (T.pack ("query-commands decode: " <> e))
      Right (QueryCommandsReply names) -> Right (Set.fromList names)

newtype QueryCommandsReply = QueryCommandsReply [Text]

instance A.FromJSON QueryCommandsReply where
  parseJSON = A.withObject "QueryCommandsReply" $ \o -> do
    rows <- o A..: "return"
    QueryCommandsReply
      <$> mapM (A.withObject "QmpCommand" (A..: "name")) rows

-- | Async-job poll loop. Repeatedly issues 'query-jobs' until the
-- given job-id reaches @concluded@, then issues 'job-dismiss'.
--
-- Polling starts at 100ms, doubles up to a 1-second cap. Total
-- wall-clock budget is bounded by 'maxIterations' iterations
-- (default 300, i.e. ~5 minutes given the geometric backoff —
-- plenty for a multi-GB vmstate save under typical disk throughput).
-- A job that vanishes from query-jobs before concluding (which
-- should not happen but did once during the spike if dismiss was
-- raced) returns @Left "job vanished"@. The dismiss call always
-- runs in @finally@ semantics: even on iteration cap exhaustion we
-- clean up the job record.
pollQmpJob :: QemuConfig -> Int64 -> Text -> IO (Either Text ())
pollQmpJob config vmId jobId = go 0 (100000 :: Int)
  where
    maxIterations = 300 :: Int
    capUs = 1000000 :: Int
    go n delayUs
      | n >= maxIterations = do
          _ <- dismissBestEffort
          pure $
            Left $
              "QMP job "
                <> jobId
                <> " did not conclude within "
                <> T.pack (show maxIterations)
                <> " poll iterations (~5 minutes)"
      | otherwise = do
          threadDelay delayUs
          js <- queryJobs config vmId
          case js of
            Left err -> pure (Left err)
            Right jobs -> case lookupJob jobs of
              Nothing ->
                pure $
                  Left $
                    "QMP job "
                      <> jobId
                      <> " vanished from query-jobs before concluding"
              Just j
                | jobStatus j == "concluded" -> do
                    _ <- dismissBestEffort
                    case jobError j of
                      Just msg | not (T.null msg) -> pure $ Left msg
                      _ -> pure (Right ())
                | otherwise ->
                    go (n + 1) (min capUs (delayUs * 2))
    lookupJob = lookup jobId . map (\j -> (jobIdField j, j))
    dismissBestEffort =
      sendQmpCommand
        config
        vmId
        ( LBS.toStrict $
            A.encode $
              A.object
                [ "execute" A..= A.String "job-dismiss"
                , "arguments" A..= A.object ["id" A..= A.String jobId]
                ]
        )

data QmpJob = QmpJob
  { jobIdField :: !Text
  , jobStatus :: !Text
  , jobError :: !(Maybe Text)
  }
  deriving (Eq, Show)

newtype QueryJobsReply = QueryJobsReply [QmpJob]

instance A.FromJSON QueryJobsReply where
  parseJSON = A.withObject "QueryJobsReply" $ \o ->
    QueryJobsReply <$> o A..: "return"

instance A.FromJSON QmpJob where
  parseJSON = A.withObject "QmpJob" $ \o ->
    QmpJob
      <$> o A..: "id"
      <*> o A..: "status"
      <*> o A..:? "error"

queryJobs :: QemuConfig -> Int64 -> IO (Either Text [QmpJob])
queryJobs config vmId = do
  raw <- sendQmpRaw config vmId [qmpQQ| { "execute": "query-jobs" } |]
  pure $ do
    bs <- raw
    line <- extractReplyLine bs
    case A.eitherDecodeStrict line of
      Left e -> Left (T.pack ("query-jobs decode: " <> e))
      Right (QueryJobsReply jobs) -> Right jobs

-- | Poll until QMP responds for @vmId@, or 10 s elapses.
--
-- QEMU binds the QMP listen socket during init, but there's a
-- small window between @forkProcess@ returning a pid (the agent's
-- vmStart RPC returns) and the socket being ready to accept
-- connections. A daemon-side caller that immediately invokes
-- a QMP command (e.g. @snapshot-load@ in the build-cache memory-
-- mode resume path) hits "ENOENT" on the socket path during that
-- window. This helper bridges the gap with a bounded poll: try
-- @query-commands@; on a connect-style failure, sleep and retry;
-- on any other outcome (success, structured error), return.
--
-- Returns @()@ unconditionally — by the time the timeout
-- expires, the caller's next QMP call will surface a clearer
-- error than this one would. Best-effort readiness gate, not a
-- correctness gate.
waitForQmpReady :: QemuConfig -> Int64 -> IO ()
waitForQmpReady config vmId = go (40 :: Int)
  where
    -- 40 * 250 ms = 10 s.
    intervalUs :: Int
    intervalUs = 250000
    go 0 = pure ()
    go n = do
      r <- qmpQueryCommands config vmId
      case r of
        Right _ -> pure ()
        Left err
          | isConnectFailure err -> do
              threadDelay intervalUs
              go (n - 1)
          | otherwise -> pure ()
    isConnectFailure t =
      T.isInfixOf "does not exist" t
        || T.isInfixOf "Connection refused" t
        || T.isInfixOf "No such file or directory" t
