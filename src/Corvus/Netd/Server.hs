{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Cap'n Proto RPC listener for `corvus-netd`.
--
-- TCP only — the daemon always reaches the agent over loopback.
-- The bootstrap cap is 'NetAgentCap'.
--
-- Phase 2 change: each accepted connection gets its OWN
-- 'Supervisor'. When the connection ends, its supervisor exits and
-- every cap exported through that supervisor is dropped, firing
-- 'SomeServer.shutdown' on each. That gives us automatic kernel
-- cleanup when a daemon disconnects without explicit `destroy`
-- calls.
--
-- The ledger, on the other hand, is process-global so resources
-- survive disconnect (until `claim*` re-adopts them or the
-- supervisor's drop fires the teardown). Phase 2.x will add a
-- 60-second orphan window before drops finalise; Phase 2's first
-- slice fires teardown immediately on cap drop.
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
import Corvus.Netd.Caps.NetAgent (newNetAgentCap)
import qualified Corvus.Netd.Ledger as L
import qualified Data.Default as Def
import qualified Network.Simple.TCP as TCP
import Supervisors (withSupervisor)

defaultNetdHost :: String
defaultNetdHost = "127.0.0.1"

defaultNetdPort :: Int
defaultNetdPort = 9877

-- | Run the agent's Cap'n Proto server on the given host/port.
-- Allocates a process-wide ledger that every accepted connection
-- shares; per-connection supervisors scope cap lifetimes to the
-- connection.
runNetdServer :: String -> Int -> IO ()
runNetdServer host port = do
  ledger <- L.newLedger
  TCP.serve (TCP.Host host) (show port) $ \(sock, _peer) ->
    withSupervisor $ \sup -> do
      netAgentCap <- newNetAgentCap sup ledger
      bootClient <- export @CGN.NetAgent sup netAgentCap
      handleConn
        (socketTransport sock defaultLimit)
        Def.def
          { debugMode = False
          , bootstrap = Just (toClient bootClient)
          }
