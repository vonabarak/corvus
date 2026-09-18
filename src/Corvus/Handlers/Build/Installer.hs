{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Installer-strategy phase for the build pipeline.
--
-- Handles the installer strategy's middle: dispatch boot keys, wait for
-- the guest to power itself off, then publish the artifact.
module Corvus.Handlers.Build.Installer
  ( runInstallerPhase
  , runBootKeys
  , waitForBakeVmShutdown
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN)
import Corvus.Handlers.Build.Artifact (publishArtifact)
import Corvus.Model
import Corvus.Node.Qmp (QmpResult (..), qmpSendKey)
import Corvus.Protocol.Build (BuildEvent (BuildLogLine), BuildSink (..))
import Corvus.Schema.Build (BootKey (..), Build (..), BuildTarget (..), btFormat, buildBootKeys, buildName, buildWaitForShutdownSec)
import Corvus.Types
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist (get)
import Database.Persist.Sql (fromSqlKey, runSqlPool, toSqlKey)

-- | The installer strategy's middle: dispatch boot keys, wait for the
-- guest to power itself off, then publish the artifact. No QGA, no
-- provisioners — the autounattend (or equivalent vendor mechanism) on
-- prepared media inside the bake VM drives everything.
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
