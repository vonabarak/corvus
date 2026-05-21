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
import Control.Concurrent.STM (TVar)
import Control.Monad.Logger (logInfoN, runStderrLoggingT)
import Corvus.Node.Caps.Session (newSessionCap)
import qualified Corvus.Node.GuestAgent as NGA
import qualified Corvus.Node.Ledger as L
import qualified Corvus.Node.StatusPoller as SP
import qualified Corvus.Node.Transfer as NTr
import Corvus.Rpc.Common (handleParsed)
import qualified Corvus.Tls as Tls
import Corvus.Types (SocketBufferHandle)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Supervisors (Supervisor)

-- | Bootstrap-cap state. Holds the supervisor for child caps, the
-- process-wide VM ledger, the status-subscriber registry, the
-- agent-wide QGA connection cache, and the per-VM chardev ring
-- buffers (serial + HMP monitor) the agent exposes via
-- @openSerialConsole@ / @openHmpMonitor@.
data NodeAgentCap = NodeAgentCap
  { nacSup :: !Supervisor
  , nacVmLedger :: !L.VmLedger
  , nacSubs :: !SP.Subscribers
  , nacQgaConns :: !NGA.GuestAgentConns
  , nacSerialBuffers :: !(TVar (Map.Map Int64 SocketBufferHandle))
  , nacMonitorBuffers :: !(TVar (Map.Map Int64 SocketBufferHandle))
  , nacTransferTokens :: !NTr.TokenRegistry
  -- ^ Process-wide token → DiskReader cap map for the inter-agent
  -- transfer flow. Populated by @diskOpenRead@; consumed by
  -- @attachReader@ (typically called by another agent's
  -- 'diskImportFromPeer' on a different session).
  , nacTlsConfig :: !(Maybe Tls.TlsConfig)
  -- ^ The agent's own TLS material. 'Just' when the agent was
  -- started with TLS enabled; reused as the outbound TLS client
  -- material when the agent dials a peer agent during a
  -- 'diskImportFromPeer'. 'Nothing' for @--no-tls@ deployments.
  }

newNodeAgentCap
  :: Supervisor
  -> L.VmLedger
  -> SP.Subscribers
  -> NGA.GuestAgentConns
  -> TVar (Map.Map Int64 SocketBufferHandle)
  -> TVar (Map.Map Int64 SocketBufferHandle)
  -> NTr.TokenRegistry
  -> Maybe Tls.TlsConfig
  -> IO NodeAgentCap
newNodeAgentCap sup vmLedger subs qgaConns serialBufs monitorBufs tokens tlsCfg =
  pure
    NodeAgentCap
      { nacSup = sup
      , nacVmLedger = vmLedger
      , nacSubs = subs
      , nacQgaConns = qgaConns
      , nacSerialBuffers = serialBufs
      , nacMonitorBuffers = monitorBufs
      , nacTransferTokens = tokens
      , nacTlsConfig = tlsCfg
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
      impl <-
        newSessionCap
          owner
          (nacSup nac)
          (nacVmLedger nac)
          (nacSubs nac)
          (nacQgaConns nac)
          (nacSerialBuffers nac)
          (nacMonitorBuffers nac)
          (nacTransferTokens nac)
          (nacTlsConfig nac)
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
