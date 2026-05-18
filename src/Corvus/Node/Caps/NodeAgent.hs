{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | NodeAgent bootstrap capability for `corvus-nodeagent`.
--
-- The 'NodeAgentCap' is the root cap exported on every accepted
-- TCP connection. It implements three methods:
--
--   * @ping@ — liveness check, returns immediately.
--   * @version@ — semver + advertised capability flags.
--   * @session(owner)@ — returns a 'SessionCap' scoped to the
--     caller-claimed owner tag.
module Corvus.Node.Caps.NodeAgent
  ( NodeAgentCap (..)
  , newNodeAgentCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Nodeagent as CGNA
import Capnp.Rpc.Server (SomeServer)
import Control.Monad.Logger (logInfoN, runStderrLoggingT)
import Corvus.Node.Caps.Session (newSessionCap)
import qualified Corvus.Node.Ledger as L
import Corvus.Rpc.Common (handleParsed)
import qualified Data.Text as T
import Supervisors (Supervisor)

-- | Bootstrap-cap state. Holds the supervisor for child caps and
-- the two process-wide ledgers (legacy PID-keyed for the
-- surviving @processSpawn*@ / @processStop@ surface; vmId-keyed
-- for the VM-abstraction surface).
data NodeAgentCap = NodeAgentCap
  { nacSup :: !Supervisor
  , nacProcLedger :: !L.ProcessLedger
  , nacVmLedger :: !L.VmLedger
  }

newNodeAgentCap :: Supervisor -> L.ProcessLedger -> L.VmLedger -> IO NodeAgentCap
newNodeAgentCap sup procLedger vmLedger =
  pure
    NodeAgentCap
      { nacSup = sup
      , nacProcLedger = procLedger
      , nacVmLedger = vmLedger
      }

instance SomeServer NodeAgentCap

instance CGNA.NodeAgent'server_ NodeAgentCap where
  nodeAgent'ping _ = handleParsed $ \_ -> do
    logLine "ping"
    pure CGNA.NodeAgent'ping'results

  nodeAgent'version _ = handleParsed $ \_ -> do
    logLine "version"
    let info =
          CGNA.AgentInfo
            { CGNA.semver = T.pack agentSemver
            , CGNA.capabilities = capnpCapabilities
            }
    pure CGNA.NodeAgent'version'results {CGNA.info = info}

  nodeAgent'session nac =
    handleParsed $ \CGNA.NodeAgent'session'params {CGNA.owner = owner} -> do
      logLine ("session owner=" <> owner)
      impl <- newSessionCap owner (nacProcLedger nac) (nacVmLedger nac)
      client <- export @CGNA.Session (nacSup nac) impl
      pure CGNA.NodeAgent'session'results {CGNA.session = client}

-- ----------------------------------------------------------------------
-- Constants

-- | Advertised semver for the agent wire protocol. Phase 1 ships
-- 0.1.0; bumps as the schema grows.
agentSemver :: String
agentSemver = "0.1.0"

-- | Feature flags returned by @version@. Phase 1 ships none;
-- "disk", "vm", "console", "status" get added as phases land.
capnpCapabilities :: [T.Text]
capnpCapabilities = []

-- ----------------------------------------------------------------------

logLine :: T.Text -> IO ()
logLine msg = runStderrLoggingT $ logInfoN ("[nodeagent] " <> msg)
