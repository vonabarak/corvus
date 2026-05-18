{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Cap'n Proto RPC listener for `corvus-netd`.
--
-- Phase 2.5: stateless agent. Startup runs
-- 'cleanupCorvusKernelState' before binding the TCP listener,
-- guaranteeing a clean kernel state on every (re)start. No
-- orphan-window sweeper; no on-disk persistence; no
-- per-connection cap supervision (the declarative API doesn't
-- expose resource caps).
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
import Corvus.Netd.Cleanup (cleanupCorvusKernelState)
import qualified Corvus.Netd.Events as Ev
import qualified Corvus.Netd.IpMonitor as Mon
import qualified Corvus.Netd.Ledger as L
import qualified Corvus.Netd.Network as Net
import qualified Corvus.Netd.Tap as Tap
import qualified Data.Default as Def
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Network.Simple.TCP as TCP
import Supervisors (withSupervisor)

defaultNetdHost :: String
defaultNetdHost = "127.0.0.1"

defaultNetdPort :: Int
defaultNetdPort = 9877

-- | Run the agent's Cap'n Proto server.
--
-- 1. Run startup cleanup so we begin from an empty world.
-- 2. Allocate the process-wide network + TAP ledgers and the
--    subscriber registry.
-- 3. Fork the rtnetlink watcher (drift events).
-- 4. Accept TCP connections.
runNetdServer :: String -> Int -> IO ()
runNetdServer host port = do
  cleanupCorvusKernelState
  netLedger <- L.newNetworkLedger
  tapLedger <- L.newTapLedger
  subs <- Ev.newSubscribers
  -- Background drift watcher: parse `ip monitor link`, fire
  -- onResourceVanished for any deleted name we own.
  void . forkIO $
    Mon.runIpMonitor $ \(Mon.LinkDeleted ifname) -> do
      mKind <- atomically $ resolveKind netLedger tapLedger ifname
      case mKind of
        Nothing -> pure ()
        Just kind -> Ev.dispatchVanished subs kind ifname
  TCP.serve (TCP.Host host) (show port) $ \(sock, _peer) ->
    withSupervisor $ \sup -> do
      netAgentCap <- newNetAgentCap sup netLedger tapLedger subs
      bootClient <- export @CGN.NetAgent sup netAgentCap
      handleConn
        (socketTransport sock defaultLimit)
        Def.def
          { debugMode = False
          , bootstrap = Just (toClient bootClient)
          }

-- | Look up an ifname in both ledgers; return the 'kind' label
-- used in 'onResourceVanished' events.
resolveKind
  :: L.NetworkLedger Net.NetworkSpec Net.NetworkLiveState
  -> L.TapLedger Tap.TapSpec
  -> T.Text
  -> STM (Maybe T.Text)
resolveKind nl tl name = do
  nets <- L.readNetworks nl
  taps <- L.readTaps tl
  pure $
    if Map.member name nets
      then Just "network"
      else
        if Map.member name taps
          then Just "tap"
          else Nothing
