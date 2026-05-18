{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Startup + shutdown cleanup for `corvus-nodeagent`.
--
-- The agent is stateless: nothing persists to disk. Every fresh
-- process starts with a guaranteed-empty world by reaping any
-- orphaned local-machine processes matching the agent's
-- conventional argv prefixes, and removing per-VM runtime
-- socket dirs.
--
-- Phase 1 ships a no-op stub: there's nothing for the agent to
-- spawn yet. The real implementation (pkill QEMU/virtiofsd
-- patterns, scrub @\$XDG_RUNTIME_DIR/corvus/*@) lands in Phase 3
-- with the VM-lifecycle move.
--
-- 'cleanupCorvusProcesses' is called once during
-- 'Corvus.Node.Server.runNodeAgentServer' before the listener
-- binds, and once from the @SIGTERM@ / @SIGINT@ handler in
-- @app/nodeagent/Main.hs@ before the process exits.
module Corvus.Node.Cleanup
  ( cleanupCorvusProcesses
  , corvusQemuPrefix
  , corvusVirtiofsdHint
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, runStderrLoggingT)
import qualified Data.Text as T

-- | Naming prefixes the agent reserves on argv. The startup /
-- shutdown pass treats anything matching as fair game; anything
-- else is left alone.
corvusQemuPrefix, corvusVirtiofsdHint :: T.Text
corvusQemuPrefix = "corvus-qemu/"
corvusVirtiofsdHint = "--shared-dir=" -- combined with $XDG_RUNTIME_DIR/corvus/ later

-- | Best-effort full cleanup. Phase 1: log + noop. Phase 3
-- replaces the body with the real pkill + runtime-dir scrub.
cleanupCorvusProcesses :: IO ()
cleanupCorvusProcesses = runStderrLoggingT $ do
  liftIO (pure ()) -- placeholder until Phase 3 wires VM spawning
  logInfoN "[nodeagent] cleanup: no spawned processes yet (Phase 1 stub)"
