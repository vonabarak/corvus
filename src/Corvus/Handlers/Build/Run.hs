{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Run-time build orchestration: start bake VM, run provisioners,
-- stop bake VM, publish artifact.
--
-- This module is intentionally separate from 'Corvus.Handlers.Build'
-- to break a module cycle: 'Build.hs' imports 'CacheResume.hs',
-- and 'CacheResume.hs' needs these run functions.  'Run.hs' does NOT
-- import 'Build.hs' or 'CacheResume.hs'.
module Corvus.Handlers.Build.Run
  ( runBakeAndPublish
  , runProvisionersStopAndPublish
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN)
import Corvus.Action (mkActionContext, runActionAsSubtask)
import qualified Corvus.Build.Cache.Hash as H
import Corvus.Handlers.Build.Artifact (publishArtifact)
import Corvus.Handlers.Build.Cache (snapshotCachedStep, writableCacheDisks)
import Corvus.Handlers.Build.Installer (runInstallerPhase)
import Corvus.Handlers.Build.Provisioner (runProvisioners)
import Corvus.Handlers.Vm (VmStart (..), VmStop (..))
import Corvus.Model
import Corvus.Protocol
import Corvus.Protocol.Build (BuildSink (..))
import Corvus.Schema.Build (Build (..), BuildCacheMode (..), BuildStrategy (..), BuildTarget (..), buildCacheMode, buildName, buildStrategy, buildTarget)
import Corvus.Types (BuildOptions (..), ServerState)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist.Sql (runSqlPool)

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
            dr <- writableCacheDisks state vmId artifactDiskId
            case dr of
              Left err -> pure (Left ("cache: enumerate disks: " <> err))
              Right disks -> do
                snapResult <-
                  snapshotCachedStep
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

classifyStartResp :: Response -> Either Text ()
classifyStartResp r = case r of
  RespVmStateChanged _ -> Right ()
  RespError err -> Left err
  RespInvalidTransition _ msg -> Left msg
  _ -> Left $ "unexpected response: " <> T.pack (show r)

classifyStopResp :: Response -> Either Text ()
classifyStopResp r = case r of
  RespVmStateChanged _ -> Right ()
  RespError err -> Left err
  RespInvalidTransition _ msg -> Left msg
  _ -> Left $ "unexpected response: " <> T.pack (show r)
