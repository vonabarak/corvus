{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Cap'n Proto RPC listener for `corvus-netd`.
--
-- Mirrors "Corvus.Rpc.Server" but trimmed: TCP only (the daemon
-- always reaches the agent over loopback), and the bootstrap cap is
-- 'NetAgentCap' rather than 'DaemonCap'.
--
-- One supervisor per process (not per connection) — sub-caps
-- exported from 'NetAgentCap.session' need to outlive the bootstrap
-- response but are still scoped by Cap'n Proto's per-connection
-- lifecycle. Same pattern the daemon uses in Phase 1.
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
import qualified Data.Default as Def
import qualified Network.Simple.TCP as TCP
import Supervisors (withSupervisor)

-- | Default loopback bind address.
defaultNetdHost :: String
defaultNetdHost = "127.0.0.1"

-- | Default TCP port. One above the daemon's 9876.
defaultNetdPort :: Int
defaultNetdPort = 9877

-- | Run the agent's Cap'n Proto server on the given host/port.
-- Blocks the calling thread; spawns a fresh handler per connection.
runNetdServer :: String -> Int -> IO ()
runNetdServer host port = withSupervisor $ \sup ->
  TCP.serve (TCP.Host host) (show port) $ \(sock, _peer) -> do
    netAgentCap <- newNetAgentCap sup
    bootClient <- export @CGN.NetAgent sup netAgentCap
    handleConn
      (socketTransport sock defaultLimit)
      Def.def
        { debugMode = False
        , bootstrap = Just (toClient bootClient)
        }
