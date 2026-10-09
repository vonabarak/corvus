{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Run-time build orchestration: start bake VM, run provisioners,
-- stop bake VM, publish artifact.
module Corvus.Handlers.Build.Run
  ( runBakeAndPublish
  , runProvisionersStopAndPublish
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT)
import Corvus.Action (mkActionContext, runActionAsSubtask)
import Corvus.Handlers.Build.Artifact (publishArtifact)
import Corvus.Handlers.Build.Installer (runInstallerPhase)
import Corvus.Handlers.Build.Provisioner (runProvisioners)
import Corvus.Handlers.Vm (VmStart (..), VmStop (..))
import Corvus.Model
import Corvus.Protocol
import Corvus.Schema.Build (Build, BuildStrategy (..), BuildTarget)
import Corvus.Types (ServerState)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T

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
  -> Build
  -> LoggingT IO (Either Text Int64)
runBakeAndPublish state parentTaskId sink vmIdLong strategy artifactDiskId target needFlatten b = do
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
          b

-- | Run provisioners over QGA, stop the bake VM, and publish the artifact.
runProvisionersStopAndPublish
  :: ServerState
  -> TaskId
  -> BuildSink
  -> Int64
  -> Int64
  -> BuildTarget
  -> Bool
  -> Build
  -> LoggingT IO (Either Text Int64)
runProvisionersStopAndPublish state parentTaskId sink vmIdLong artifactDiskId _target needFlatten b = do
  provResult <- runProvisioners state parentTaskId sink vmIdLong b
  case provResult of
    Left err -> pure $ Left err
    Right () -> do
      stopResp <-
        liftIO $
          runActionAsSubtask (mkActionContext state parentTaskId "system") (VmStop vmIdLong 300)
      case classifyStopResp stopResp of
        Left err -> pure $ Left $ "stop bake VM: " <> err
        Right () -> do
          publishResult <-
            publishArtifact
              state
              parentTaskId
              vmIdLong
              artifactDiskId
              b
              needFlatten
          case publishResult of
            Left err -> pure $ Left err
            Right publishedId -> pure $ Right publishedId

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
