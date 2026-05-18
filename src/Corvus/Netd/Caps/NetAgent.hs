{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | NetAgent bootstrap capability for `corvus-netd`.
--
-- The 'NetAgentCap' is the root cap exported on every accepted TCP
-- connection. It implements three methods:
--
--   * @ping@ — liveness check, returns immediately.
--   * @version@ — semver + advertised capability flags. Used by
--     daemons to negotiate features without parsing log output.
--   * @session(owner)@ — returns a 'SessionCap' scoped to the
--     caller-claimed owner tag.
--
-- Phase 1: all methods are real (so the smoke test passes); the
-- privileged methods on 'SessionCap' are still stubs.
module Corvus.Netd.Caps.NetAgent
  ( NetAgentCap (..)
  , newNetAgentCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Netagent as CGN
import Capnp.Rpc.Server (SomeServer)
import Control.Monad.Logger (logInfoN, runStderrLoggingT)
import Corvus.Netd.Caps.Session (newSessionCap)
import Corvus.Rpc.Common (handleParsed)
import qualified Data.Text as T
import Supervisors (Supervisor)

-- | Bootstrap-cap state.  No shared mutable state in Phase 1 — the
-- ledger and rtnetlink watcher get plumbed through here in Phase 2.
newtype NetAgentCap = NetAgentCap
  { nacSup :: Supervisor
  }

newNetAgentCap :: Supervisor -> IO NetAgentCap
newNetAgentCap sup = pure (NetAgentCap sup)

instance SomeServer NetAgentCap

instance CGN.NetAgent'server_ NetAgentCap where
  netAgent'ping _ = handleParsed $ \_ -> do
    logLine "ping"
    pure CGN.NetAgent'ping'results

  netAgent'version _ = handleParsed $ \_ -> do
    logLine "version"
    let info =
          CGN.AgentInfo
            { CGN.semver = T.pack agentSemver
            , CGN.capabilities = capnpCapabilities
            }
    pure CGN.NetAgent'version'results {CGN.info = info}

  netAgent'session (NetAgentCap sup) =
    handleParsed $ \CGN.NetAgent'session'params {CGN.owner = owner} -> do
      logLine ("session owner=" <> owner)
      impl <- newSessionCap owner sup
      client <- export @CGN.Session sup impl
      pure CGN.NetAgent'session'results {CGN.session = client}

-- ----------------------------------------------------------------------
-- Constants

-- | Advertised semver for the agent wire protocol. Phase 1 is 0.1.x;
-- bump on every breaking schema change.
agentSemver :: String
agentSemver = "0.1.0"

-- | Feature flags returned by @version@. Phase 1: none yet — the
-- daemon should treat an empty list as "talk only to ping/version".
-- Phase 2 adds "bridge", "tap", "nat", "dnsmasq".
capnpCapabilities :: [T.Text]
capnpCapabilities = []

-- ----------------------------------------------------------------------
-- Logging shim. The real agent uses the same MonadLogger setup as
-- the daemon (see app/netd/Main.hs); this helper lets the cap
-- impls just `logLine "foo"` without threading the logging context.
logLine :: T.Text -> IO ()
logLine msg = runStderrLoggingT $ logInfoN ("[netd] " <> msg)
