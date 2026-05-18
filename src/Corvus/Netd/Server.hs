{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Cap'n Proto RPC listener for `corvus-netd`.
--
-- TCP only — the daemon always reaches the agent over loopback.
-- The bootstrap cap is 'NetAgentCap'.
--
-- Phase 2 wiring:
--
--   * One process-wide 'Ledger' tracks every resource the agent
--     created so cap-drop teardown is idempotent and reconnecting
--     daemons can re-adopt resources via 'claim*'.
--   * One process-wide 'Subscribers' registry holds every
--     EventSink cap the daemon supplied via 'subscribeEvents'.
--   * One background 'runIpMonitor' thread parses
--     @ip monitor link@ output and fires 'onResourceVanished'
--     for every ifname that disappears AND is in the ledger.
--   * Each accepted connection still gets its own per-connection
--     'Supervisor' so dropping the TCP session drops every cap
--     it exported and fires 'SomeServer.shutdown' → kernel
--     cleanup.
module Corvus.Netd.Server
  ( runNetdServer
  , defaultNetdHost
  , defaultNetdPort
  )
where

import Capnp (export)
import qualified Capnp.Gen.Netagent as CGN
import Capnp.Rpc
  ( ConnConfig (..)
  , handleConn
  , socketTransport
  , toClient
  )
import Capnp.TraversalLimit (defaultLimit)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, atomically)
import Control.Monad (void)
import Corvus.Netd.Caps.NetAgent (newNetAgentCap)
import qualified Corvus.Netd.Events as Ev
import qualified Corvus.Netd.IpMonitor as Mon
import qualified Corvus.Netd.Ledger as L
import qualified Data.Default as Def
import qualified Data.Text as T
import qualified Network.Simple.TCP as TCP
import Supervisors (withSupervisor)

defaultNetdHost :: String
defaultNetdHost = "127.0.0.1"

defaultNetdPort :: Int
defaultNetdPort = 9877

-- | Run the agent's Cap'n Proto server on the given host/port.
-- Allocates process-wide state (ledger + subscriber registry),
-- forks the rtnetlink-style watcher, and then accepts TCP
-- connections with per-connection cap supervisors.
runNetdServer :: String -> Int -> IO ()
runNetdServer host port = do
  ledger <- L.newLedger
  subs <- Ev.newSubscribers
  -- Start the ip-monitor watcher. Each delete event is filtered
  -- against the ledger: only resources the agent owns generate
  -- onResourceVanished. Unrelated kernel mutations (a SLIRP
  -- interface flapping, an admin's manual probe) are silently
  -- ignored.
  void . forkIO $
    Mon.runIpMonitor $ \(Mon.LinkDeleted ifname) -> do
      mResource <-
        atomically $
          findByName ledger ifname
      case mResource of
        Nothing -> pure ()
        Just (kind, name) ->
          Ev.dispatchVanished subs (kindText kind) name
  TCP.serve (TCP.Host host) (show port) $ \(sock, _peer) ->
    withSupervisor $ \sup -> do
      netAgentCap <- newNetAgentCap sup ledger subs
      bootClient <- export @CGN.NetAgent sup netAgentCap
      handleConn
        (socketTransport sock defaultLimit)
        Def.def
          { debugMode = False
          , bootstrap = Just (toClient bootClient)
          }

-- | Scan the ledger for any resource whose name matches @ifname@.
-- TAP and Bridge entries store the ifname directly in 'rName';
-- the other kinds (NAT, dnsmasq) use synthetic names that never
-- collide with kernel ifnames, so a name-only match is unambiguous.
findByName :: L.Ledger -> T.Text -> STM (Maybe (L.Kind, T.Text))
findByName ledger ifname = do
  rs <- L.listAll ledger
  pure $
    case filter (\r -> L.rName r == ifname) rs of
      (r : _) -> Just (L.rKind r, L.rName r)
      [] -> Nothing

-- | Wire-side name for a Kind, used as the @kind@ field of
-- 'onResourceVanished'.
kindText :: L.Kind -> T.Text
kindText L.KBridge = "bridge"
kindText L.KTap = "tap"
kindText L.KNat = "nat"
kindText L.KDnsmasq = "dnsmasq"
