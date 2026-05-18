{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | NetAgent bootstrap capability for `corvus-netd`.
--
-- The 'NetAgentCap' is the root cap exported on every accepted
-- TCP connection. It implements three methods:
--
--   * @ping@ — liveness check, returns immediately.
--   * @version@ — semver + advertised capability flags.
--   * @session(owner)@ — returns a 'SessionCap' scoped to the
--     caller-claimed owner tag, sharing the agent's
--     process-wide ledgers and subscriber registry.
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
import qualified Corvus.Netd.Events as Ev
import qualified Corvus.Netd.Ledger as L
import qualified Corvus.Netd.Network as Net
import qualified Corvus.Netd.Tap as Tap
import Corvus.Rpc.Common (handleParsed)
import qualified Data.Text as T
import Supervisors (Supervisor)

-- | Bootstrap-cap state.
data NetAgentCap = NetAgentCap
  { nacSup :: !Supervisor
  , nacNetworkLedger :: !(L.NetworkLedger Net.NetworkSpec Net.NetworkLiveState)
  , nacTapLedger :: !(L.TapLedger Tap.TapSpec)
  , nacSubs :: !Ev.Subscribers
  }

newNetAgentCap
  :: Supervisor
  -> L.NetworkLedger Net.NetworkSpec Net.NetworkLiveState
  -> L.TapLedger Tap.TapSpec
  -> Ev.Subscribers
  -> IO NetAgentCap
newNetAgentCap sup nl tl subs =
  pure (NetAgentCap sup nl tl subs)

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

  netAgent'session nac =
    handleParsed $ \CGN.NetAgent'session'params {CGN.owner = owner} -> do
      logLine ("session owner=" <> owner)
      impl <-
        newSessionCap
          owner
          (nacSup nac)
          (nacNetworkLedger nac)
          (nacTapLedger nac)
          (nacSubs nac)
      client <- export @CGN.Session (nacSup nac) impl
      pure CGN.NetAgent'session'results {CGN.session = client}

-- ----------------------------------------------------------------------
-- Constants

-- | Advertised semver for the agent wire protocol. Phase 2.5
-- bumps to 0.2 since the schema is a breaking change.
agentSemver :: String
agentSemver = "0.2.0"

-- | Feature flags returned by @version@.
capnpCapabilities :: [T.Text]
capnpCapabilities = ["network", "tap", "ip-forwarding", "events"]

-- ----------------------------------------------------------------------

logLine :: T.Text -> IO ()
logLine msg = runStderrLoggingT $ logInfoN ("[netd] " <> msg)
