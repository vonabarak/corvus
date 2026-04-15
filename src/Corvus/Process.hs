{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Shared lifecycle helpers for external processes spawned by Corvus.
--
-- Subsystems that spawn processes (QEMU itself, virtiofsd, dnsmasq, pasta,
-- etc.) all have the same three concerns: wait for the process to become
-- ready, stop it gracefully rather than @SIGKILL@-ing it, and check whether
-- a PID is still alive. This module provides those primitives so each
-- subsystem doesn't reinvent them (often with different — and worse —
-- semantics).
--
-- Spawning itself is intentionally /not/ abstracted: callers use
-- @createProcess@, @nsSpawn@, or host-with-ns-target spawns depending on
-- where the process needs to live. Forcing a common spawn API would paper
-- over real differences between those worlds.
module Corvus.Process
  ( -- * Readiness checks
    waitForSocketFile

    -- * Liveness checks
  , isProcessAlive
  , waitForExit

    -- * Graceful shutdown
  , StopResult (..)
  , stopProcess
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logDebugN, logInfoN, logWarnN)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Posix.Signals (sigKILL, sigTERM, signalProcess)
import System.Posix.Types (ProcessID)

--------------------------------------------------------------------------------
-- Readiness
--------------------------------------------------------------------------------

-- | Poll for a socket (or any file) to appear on disk.
--
-- Polls every 100ms up to @timeoutMs@ milliseconds. Returns 'True' if the
-- file appeared within the timeout, 'False' otherwise. Callers decide
-- whether absence means the process is broken or just still warming up.
waitForSocketFile :: FilePath -> Int -> IO Bool
waitForSocketFile path timeoutMs = go (max 0 timeoutMs)
  where
    go remaining
      | remaining <= 0 = pure False
      | otherwise = do
          exists <- doesFileExist path
          if exists
            then pure True
            else do
              threadDelay 100000 -- 100ms
              go (remaining - 100)

--------------------------------------------------------------------------------
-- Liveness
--------------------------------------------------------------------------------

-- | Check whether a PID refers to a running (non-zombie) process.
--
-- @kill(pid, 0)@ alone is not enough: it returns success for zombies too
-- (the PID is still valid until the parent reaps it), so a dead-but-
-- unreaped child would look alive. We additionally read
-- @\/proc\/<pid>\/status@ and reject the process if its @State@ line starts
-- with @Z@.
isProcessAlive :: ProcessID -> IO Bool
isProcessAlive pid = do
  signalOk <- try $ signalProcess 0 pid
  case signalOk of
    Left (_ :: SomeException) -> pure False
    Right () -> do
      let statusPath = "/proc/" ++ show pid ++ "/status"
      statusRead <- try $ readFile statusPath
      case statusRead of
        Left (_ :: SomeException) ->
          -- /proc vanished between kill and read → process is gone.
          pure False
        Right contents -> pure $ not (isZombie contents)
  where
    isZombie :: String -> Bool
    isZombie contents = any isZombieLine (lines contents)
    isZombieLine line = case words line of
      ("State:" : state : _) -> case state of
        ('Z' : _) -> True
        _ -> False
      _ -> False

-- | Wait up to @timeoutSec@ seconds for a process to exit.
--
-- Polls every 100ms. Returns 'True' if the process exited within the
-- timeout, 'False' if it's still alive when the timer runs out.
waitForExit :: ProcessID -> Int -> IO Bool
waitForExit pid timeoutSec = go (max 0 timeoutSec * 10)
  where
    go remaining
      | remaining <= 0 = not <$> isProcessAlive pid
      | otherwise = do
          alive <- isProcessAlive pid
          if not alive
            then pure True
            else do
              threadDelay 100000 -- 100ms
              go (remaining - 1)

--------------------------------------------------------------------------------
-- Graceful shutdown
--------------------------------------------------------------------------------

-- | Outcome of 'stopProcess'.
data StopResult
  = -- | Process exited after the graceful action (e.g. QMP @quit@).
    StoppedGracefully
  | -- | Process exited in response to @SIGTERM@.
    StoppedByTerm
  | -- | Process survived @SIGTERM@ and was @SIGKILL@-ed.
    StoppedByKill
  | -- | Process was already gone when we started.
    NotRunning
  | -- | Signalling failed for a reason other than "no such process".
    StopFailed !Text
  deriving (Eq, Show)

-- | Stop a process in three escalating stages:
--
--   1. Run the optional graceful hook (e.g. QMP @quit@ for QEMU, dbus
--      shutdown, writing a command to a pipe, …). Wait up to
--      @gracefulWaitSec@ seconds for the process to exit.
--   2. If still alive, send @SIGTERM@ and wait up to @termWaitSec@ seconds.
--   3. If still alive, send @SIGKILL@.
--
-- At each stage we probe with @kill -0@; a process that died between polls
-- is reported as the earliest stage that could have caused it.
--
-- The graceful hook is skipped when 'Nothing' is passed. For processes
-- without a control channel (virtiofsd, dnsmasq, pasta), pass @Nothing@
-- and a small @termWaitSec@.
stopProcess
  :: (MonadIO m, MonadLogger m)
  => Text
  -- ^ Human-readable name for logs (e.g. @"virtiofsd(tag=shared)"@).
  -> ProcessID
  -- ^ Target PID.
  -> Maybe (IO ())
  -- ^ Graceful action, run before any signal is sent.
  -> Int
  -- ^ Seconds to wait after the graceful hook before escalating.
  -> Int
  -- ^ Seconds to wait after @SIGTERM@ before falling back to @SIGKILL@.
  -> m StopResult
stopProcess name pid mGraceful gracefulWaitSec termWaitSec = do
  -- Bail early if the process is already gone.
  alive0 <- liftIO $ isProcessAlive pid
  if not alive0
    then do
      logDebugN $ name <> " (pid " <> T.pack (show pid) <> ") not running"
      pure NotRunning
    else tryGraceful
  where
    pidText = T.pack (show pid)

    tryGraceful = case mGraceful of
      Nothing -> trySigTerm
      Just action -> do
        logDebugN $ "Gracefully stopping " <> name <> " (pid " <> pidText <> ")"
        r <- liftIO $ try action
        case r of
          Left (e :: SomeException) ->
            logWarnN $ "Graceful hook for " <> name <> " failed: " <> T.pack (show e)
          Right () -> pure ()
        exited <- liftIO $ waitForExit pid gracefulWaitSec
        if exited
          then do
            logInfoN $ name <> " exited gracefully"
            pure StoppedGracefully
          else do
            logDebugN $
              name
                <> " still alive after "
                <> T.pack (show gracefulWaitSec)
                <> "s; escalating to SIGTERM"
            trySigTerm

    trySigTerm = do
      r <- liftIO $ try $ signalProcess sigTERM pid
      case r of
        Left (e :: SomeException) -> handleSignalFailure "SIGTERM" e
        Right () -> do
          exited <- liftIO $ waitForExit pid termWaitSec
          if exited
            then do
              logInfoN $ name <> " exited after SIGTERM"
              pure StoppedByTerm
            else do
              logWarnN $
                name
                  <> " survived SIGTERM for "
                  <> T.pack (show termWaitSec)
                  <> "s; sending SIGKILL"
              trySigKill

    trySigKill = do
      r <- liftIO $ try $ signalProcess sigKILL pid
      case r of
        Left (e :: SomeException) -> handleSignalFailure "SIGKILL" e
        Right () -> do
          -- Give the kernel a brief moment to reap the process entry; even
          -- SIGKILL is asynchronous. We only log; we do not upgrade to a
          -- stronger signal because there isn't one.
          _ <- liftIO $ waitForExit pid 2
          logInfoN $ name <> " killed"
          pure StoppedByKill

    handleSignalFailure sig e = do
      let errMsg = T.pack (show e)
      if "does not exist" `T.isInfixOf` errMsg || "No such process" `T.isInfixOf` errMsg
        then do
          logDebugN $ name <> " already gone when sending " <> sig
          pure NotRunning
        else do
          logWarnN $ "Failed to send " <> sig <> " to " <> name <> ": " <> errMsg
          pure $ StopFailed errMsg
