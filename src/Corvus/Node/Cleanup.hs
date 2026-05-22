{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Startup + shutdown cleanup for `corvus-nodeagent`.
--
-- The agent is stateless: nothing persists to disk across
-- restarts. Every fresh process starts with a guaranteed-empty
-- world by reaping any QEMU + virtiofsd processes whose argv
-- references our runtime-directory layout, and then scrubbing
-- the per-VM runtime directories under @\$XDG_RUNTIME_DIR/corvus/@.
--
-- 'cleanupCorvusProcesses' runs once during
-- 'Corvus.Node.Server.runNodeAgentServer' before the listener
-- binds, and once from the @SIGTERM@ / @SIGINT@ handler in
-- @app/nodeagent/Main.hs@ before the process exits. Symmetric
-- so restarts and explicit stops both terminate with no agent-
-- owned subprocesses left on the host.
--
-- Identification: every QEMU and virtiofsd subprocess we spawn
-- has at least one argument containing the substring
-- @/corvus/@ (it's part of the socket / runtime-dir paths). We
-- combine that with a method-specific marker
-- (@monitor.sock@ for QEMU, @virtiofsd-@ for virtiofsd) so the
-- cleanup pkill doesn't sweep unrelated processes.
module Corvus.Node.Cleanup
  ( cleanupCorvusProcesses
  )
where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN, runStderrLoggingT)
import qualified Data.Text as T
import System.Directory
  ( doesDirectoryExist
  , listDirectory
  , removePathForcibly
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Posix.Signals (sigKILL, sigTERM, signalProcess)
import System.Posix.Types (CPid (..), ProcessID)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

-- | @pgrep -f@ pattern that matches QEMU processes spawned by an
-- agent running with this runtime dir. QEMU command lines always
-- contain @unix:\$XDG_RUNTIME_DIR/corvus/<vmId>/monitor.sock@ in
-- their argv; we anchor the match on the *actual* runtime dir
-- rather than a generic @/corvus/@ substring so a sandboxed agent
-- (e.g. a unit test under a temp 'XDG_RUNTIME_DIR') only ever
-- targets its own children — never a sibling daemon's QEMUs on
-- the same host.
corvusQemuArgvPattern :: FilePath -> T.Text
corvusQemuArgvPattern runtimeDir =
  T.pack runtimeDir <> "/.*monitor.sock"

-- | @pgrep -f@ pattern that matches virtiofsd processes spawned
-- by an agent running with this runtime dir. Virtiofsd command
-- lines always contain
-- @--socket-path=\$XDG_RUNTIME_DIR/corvus/<vmId>/virtiofsd-<tag>.sock@.
corvusVirtiofsdArgvPattern :: FilePath -> T.Text
corvusVirtiofsdArgvPattern runtimeDir =
  "--socket-path=" <> T.pack runtimeDir <> "/.*virtiofsd-"

-- | Best-effort full cleanup. Each step logs + swallows failures
-- so a half-broken host still gets as much cleaned up as
-- possible. Order matters: kill virtiofsd before QEMU
-- (virtiofsd's QEMU client is already going away; killing
-- virtiofsd first prevents it from logging chardev errors during
-- QEMU's last gasp), and scrub runtime dirs last (the sockets
-- inside them are FDs the subprocesses had open).
cleanupCorvusProcesses :: IO ()
cleanupCorvusProcesses = runStderrLoggingT $ do
  runtimeDir <- liftIO corvusRuntimeDir
  vPids <- liftIO (pgrepArgv (corvusVirtiofsdArgvPattern runtimeDir))
  unless (null vPids) $
    logInfoN $
      "[nodeagent] cleanup: killing "
        <> tshow (length vPids)
        <> " virtiofsd pid(s)"
  mapM_ (liftIO . signalAndReap "virtiofsd") vPids

  qPids <- liftIO (pgrepArgv (corvusQemuArgvPattern runtimeDir))
  unless (null qPids) $
    logInfoN $
      "[nodeagent] cleanup: killing "
        <> tshow (length qPids)
        <> " QEMU pid(s)"
  mapM_ (liftIO . signalAndReap "qemu") qPids

  -- Scrub per-VM runtime dirs. Restrict the wipe to the
  -- @vms\/@ subdirectory ('Corvus.Node.Runtime.getVmRuntimeDir'
  -- builds @\$XDG_RUNTIME_DIR\/corvus\/vms\/<vmId>\/@), NOT the
  -- entire @corvus\/@ directory. The daemon's Unix-socket
  -- listener lives at @\$XDG_RUNTIME_DIR\/corvus\/corvus.sock@
  -- — a sibling of @vms\/@ — and the previous indiscriminate
  -- wipe removed it whenever the agent restarted, leaving the
  -- daemon process alive but its listener path orphaned
  -- ("ENOENT" on every subsequent client dial). Touching only
  -- @vms\/@ keeps the daemon's socket intact.
  let vmsDir = runtimeDir </> "vms"
  exists <- liftIO (doesDirectoryExist vmsDir)
  when exists $ do
    entries <- liftIO (listDirectory vmsDir)
    mapM_ (rmTreeWarn vmsDir) entries

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

-- ---------------------------------------------------------------------------
-- Process control

-- | SIGTERM, poll for exit up to ~1s, SIGKILL if still alive.
-- We don't waitpid — the agent process didn't fork these
-- children itself (we're in a fresh process; PID 1 reaps).
signalAndReap :: T.Text -> ProcessID -> IO ()
signalAndReap kind pid = do
  ignoreErr (signalProcess sigTERM pid)
  pollGone 10
  where
    pollGone :: Int -> IO ()
    pollGone 0 = do
      runStderrLoggingT $
        logWarnN $
          "[nodeagent] cleanup: "
            <> kind
            <> " pid "
            <> tshow pid
            <> " survived SIGTERM; sending SIGKILL"
      ignoreErr (signalProcess sigKILL pid)
    pollGone n = do
      alive <- isAlive pid
      when alive (threadDelay 100000 >> pollGone (n - 1))
    ignoreErr :: IO () -> IO ()
    ignoreErr = E.handle (\(_ :: E.SomeException) -> pure ())

isAlive :: ProcessID -> IO Bool
isAlive (CPid pid) = doesDirectoryExist ("/proc/" <> show pid)

-- | Run @pgrep -f@ for a pattern. @pgrep@ exits 1 when there are
-- no matches; we treat that as the common case and return @[]@.
pgrepArgv :: T.Text -> IO [ProcessID]
pgrepArgv argPat = do
  r <-
    E.try @E.SomeException $
      readProcessWithExitCode "pgrep" ["-f", T.unpack argPat] ""
  case r of
    Left _ -> pure []
    Right (ExitSuccess, out, _) -> pure (parsePgrep out)
    Right (ExitFailure _, _, _) -> pure []
  where
    parsePgrep raw =
      [CPid n | line <- lines raw, Just n <- [readMaybe line]]

-- ---------------------------------------------------------------------------
-- Runtime directory scrub

-- | The base runtime dir corvus uses, matching
-- 'Corvus.Qemu.Config.getEffectiveRuntimeDir' for the default
-- ('qcRuntimeDir = Nothing'): @\$XDG_RUNTIME_DIR/corvus@, or
-- @\/tmp\/corvus-<uid>@ when @XDG_RUNTIME_DIR@ is unset.
--
-- We hard-code this because the agent doesn't have a
-- 'QemuConfig' at startup-cleanup time — the daemon supplies
-- one per-RPC, and we want cleanup to run before any RPC.
corvusRuntimeDir :: IO FilePath
corvusRuntimeDir = do
  mXdg <- lookupEnv "XDG_RUNTIME_DIR"
  case mXdg of
    Just x -> pure (x </> "corvus")
    Nothing -> do
      mUid <- lookupEnv "UID"
      let uid = mUid >>= readMaybe :: Maybe Int
      pure ("/tmp/corvus-" <> maybe "0" show uid)

rmTreeWarn :: FilePath -> FilePath -> LoggingT IO ()
rmTreeWarn parent entry = do
  let path = parent </> entry
  r <- liftIO (E.try @E.SomeException (removePathForcibly path))
  case r of
    Right () -> pure ()
    Left e ->
      logWarnN
        ( "[nodeagent] cleanup: failed to remove "
            <> T.pack path
            <> ": "
            <> T.pack (show e)
        )
