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
--     caller-claimed owner tag, sharing the agent's process-wide
--     resource ledger.
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
import qualified Corvus.Netd.Ledger as L
import Corvus.Rpc.Common (handleParsed)
import qualified Data.Text as T
import Supervisors (Supervisor)

-- | Bootstrap-cap state.
data NetAgentCap = NetAgentCap
  { nacSup :: !Supervisor
  , nacLedger :: !L.Ledger
  }

newNetAgentCap :: Supervisor -> L.Ledger -> IO NetAgentCap
newNetAgentCap sup ledger = pure (NetAgentCap sup ledger)

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

  netAgent'session (NetAgentCap sup ledger) =
    handleParsed $ \CGN.NetAgent'session'params {CGN.owner = owner} -> do
      logLine ("session owner=" <> owner)
      impl <- newSessionCap owner sup ledger
      client <- export @CGN.Session sup impl
      pure CGN.NetAgent'session'results {CGN.session = client}

-- ----------------------------------------------------------------------
-- Constants

-- | Advertised semver for the agent wire protocol. Phase 2 bumps
-- the patch as features land; the protocol shape is unchanged
-- from Phase 1 so the major.minor stays at 0.1.
agentSemver :: String
agentSemver = "0.1.3"

-- | Feature flags returned by @version@. Subsequent Phase 2
-- slices add "dnsmasq", "events".
capnpCapabilities :: [T.Text]
capnpCapabilities = ["bridge", "tap", "nat", "ip-forwarding"]

-- ----------------------------------------------------------------------

logLine :: T.Text -> IO ()
logLine msg = runStderrLoggingT $ logInfoN ("[netd] " <> msg)
