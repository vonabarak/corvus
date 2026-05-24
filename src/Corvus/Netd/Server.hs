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
  , Transport
  , handleConn
  , socketTransport
  , toClient
  )
import Capnp.TraversalLimit (defaultLimit)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (catch)
import Control.Monad (void)
import Corvus.Netd.Caps.NetAgent (newNetAgentCap)
import Corvus.Netd.Cleanup (cleanupCorvusKernelState)
import qualified Corvus.Netd.Events as Ev
import qualified Corvus.Netd.IpMonitor as Mon
import qualified Corvus.Netd.Ledger as L
import qualified Corvus.Netd.Network as Net
import qualified Corvus.Netd.Tap as Tap
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

-- | Default bind address. Mirrors the daemon's default of binding
-- on every interface — multi-node deploys need netd reachable
-- from the daemon's host, which on a remote node is not
-- loopback. mTLS gates who can speak to it. Operators who want
-- a tighter bind (loopback-only single-host setups, behind-
-- firewall hosts) pass @--host 127.0.0.1@.
defaultNetdHost :: String
defaultNetdHost = "0.0.0.0"

defaultNetdPort :: Int
defaultNetdPort = 9877

-- | Run the agent's Cap'n Proto server.
--
-- 1. Run startup cleanup so we begin from an empty world.
-- 2. Allocate the process-wide network + TAP ledgers and the
--    subscriber registry.
-- 3. Fork the rtnetlink watcher (drift events).
-- 4. Accept TCP connections.
--
-- When @'Just' tlsCfg@ is supplied, every accepted socket is
-- wrapped with mutual TLS and the peer's CN is validated against
-- the @corvus-daemon:@ prefix before any RPC frame is exchanged.
-- 'Nothing' (@--no-tls@) falls back to plain TCP.
runNetdServer :: String -> Int -> Maybe Tls.TlsConfig -> IO ()
runNetdServer host port mTlsCfg = do
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
      Left e -> logTlsRejection "netd: TLS handshake failed" e
      Right (ctx, ref) -> do
        v <- Tls.validatePeerCN cfg ref
        case v of
          Left msg -> do
            logTlsRejection "netd: TLS peer rejected" (T.unpack msg)
            Tls.closeTlsContext ctx
          Right () -> do
            transport <- Tls.tlsTransport ctx defaultLimit
            handle transport `catch` \(e :: IOError) ->
              TIO.hPutStrLn stderr $
                T.pack ("netd: TLS connection error: " <> show e)
            Tls.closeTlsContext ctx

logTlsRejection :: String -> String -> IO ()
logTlsRejection what why =
  TIO.hPutStrLn stderr $ T.pack (what <> ": " <> why)

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
