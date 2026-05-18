{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Cap'n Proto RPC listener for `corvus-nodeagent`.
--
-- Phase 1: stateless agent. Startup runs
-- 'cleanupCorvusProcesses' before binding the TCP listener,
-- guaranteeing a clean local state on every (re)start (no-op in
-- Phase 1; real reaping lands in Phase 3 with the VM-lifecycle
-- move). No on-disk persistence; no per-connection cap
-- supervision.
module Corvus.Node.Server
  ( runNodeAgentServer
  , defaultNodeAgentHost
  , defaultNodeAgentPort
  )
where

import Capnp (export)
import qualified Capnp.Gen.Nodeagent as CGNA
import Capnp.Rpc
  ( ConnConfig (..)
  , handleConn
  , socketTransport
  , toClient
  )
import Capnp.TraversalLimit (defaultLimit)
import Corvus.Node.Caps.NodeAgent (newNodeAgentCap)
import Corvus.Node.Cleanup (cleanupCorvusProcesses)
import qualified Data.Default as Def
import qualified Network.Simple.TCP as TCP
import Supervisors (withSupervisor)

defaultNodeAgentHost :: String
defaultNodeAgentHost = "127.0.0.1"

defaultNodeAgentPort :: Int
defaultNodeAgentPort = 9878

-- | Run the agent's Cap'n Proto server.
--
-- 1. Run startup cleanup so we begin from an empty world.
-- 2. Accept TCP connections; export a fresh 'NodeAgentCap' per
--    connection (one supervisor per connection).
runNodeAgentServer :: String -> Int -> IO ()
runNodeAgentServer host port = do
  cleanupCorvusProcesses
  TCP.serve (TCP.Host host) (show port) $ \(sock, _peer) ->
    withSupervisor $ \sup -> do
      nodeAgentCap <- newNodeAgentCap sup
      bootClient <- export @CGNA.NodeAgent sup nodeAgentCap
      handleConn
        (socketTransport sock defaultLimit)
        Def.def
          { debugMode = False
          , bootstrap = Just (toClient bootClient)
          }
