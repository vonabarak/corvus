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
  , Transport
  , handleConn
  , socketTransport
  , toClient
  )
import Capnp.TraversalLimit (defaultLimit)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (newTVarIO)
import Control.Exception (catch)
import Corvus.Node.Caps.NodeAgent (newNodeAgentCap)
import Corvus.Node.Cleanup (cleanupCorvusProcesses)
import qualified Corvus.Node.Ledger as L
import qualified Corvus.Node.StatusPoller as SP
import Corvus.Qemu.Config (defaultQemuConfig)
import qualified Corvus.Tls as Tls
import qualified Data.Default as Def
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.Simple.TCP as TCP
import Network.Socket (Socket)
import Supervisors (Supervisor, withSupervisor)
import System.IO (stderr)
import System.IO.Error (IOError)

defaultNodeAgentHost :: String
defaultNodeAgentHost = "127.0.0.1"

defaultNodeAgentPort :: Int
defaultNodeAgentPort = 9878

-- | Run the agent's Cap'n Proto server.
--
-- 1. Run startup cleanup so we begin from an empty world.
-- 2. Allocate the process-wide 'VmLedger' (vmId-keyed live state
--    for QEMU + virtiofsd PIDs). Shared across every connection.
-- 3. Accept TCP connections; export a fresh 'NodeAgentCap' per
--    connection (one supervisor per connection) that shares the
--    ledger.
--
-- When @'Just' tlsCfg@ is supplied, every accepted socket is
-- wrapped with mutual TLS and the peer's CN is validated against
-- the @corvus-daemon:@ prefix before any RPC frame is exchanged.
-- 'Nothing' (i.e. @--no-tls@) falls back to plain TCP.
runNodeAgentServer :: String -> Int -> Maybe Tls.TlsConfig -> IO ()
runNodeAgentServer host port mTlsCfg = do
  cleanupCorvusProcesses
  vmLedger <- L.newVmLedger
  -- Agent-wide subscriber registry, QGA connection cache, and
  -- chardev ring-buffer registries (serial + HMP monitor).
  subs <- SP.newSubscribers
  qgaConns <- newTVarIO Map.empty
  serialBufs <- newTVarIO Map.empty
  monitorBufs <- newTVarIO Map.empty
  -- Run the status-push ticker for the lifetime of the listener.
  withAsync (SP.runStatusPoller defaultQemuConfig vmLedger qgaConns subs 10000) $ \_ ->
    TCP.serve (TCP.Host host) (show port) $ \(sock, _peer) ->
      withSupervisor $ \sup -> do
        nodeAgentCap <-
          newNodeAgentCap sup vmLedger subs qgaConns serialBufs monitorBufs
        bootClient <- export @CGNA.NodeAgent sup nodeAgentCap
        let handle transport =
              handleConn
                transport
                Def.def
                  { debugMode = False
                  , bootstrap = Just (toClient bootClient)
                  }
        runOneConn mTlsCfg sock handle

-- | Run one Cap'n Proto session, optionally wrapping the socket
-- with TLS + validating the peer CN first.
runOneConn
  :: Maybe Tls.TlsConfig
  -> Socket
  -> (Transport -> IO ())
  -> IO ()
runOneConn mTlsCfg sock handle = case mTlsCfg of
  Nothing -> handle (socketTransport sock defaultLimit)
  Just cfg -> do
    r <-
      (Right <$> Tls.wrapServerSocket cfg sock)
        `catch` \(e :: IOError) -> pure (Left (show e))
    case r of
      Left e -> logTlsRejection "nodeagent: TLS handshake failed" e
      Right (ctx, ref) -> do
        v <- Tls.validatePeerCN cfg ref
        case v of
          Left msg -> do
            logTlsRejection "nodeagent: TLS peer rejected" (T.unpack msg)
            Tls.closeTlsContext ctx
          Right () -> do
            transport <- Tls.tlsTransport ctx defaultLimit
            handle transport `catch` \(e :: IOError) ->
              TIO.hPutStrLn stderr $
                T.pack ("nodeagent: TLS connection error: " <> show e)
            Tls.closeTlsContext ctx

logTlsRejection :: String -> String -> IO ()
logTlsRejection what why =
  TIO.hPutStrLn stderr $ T.pack (what <> ": " <> why)
