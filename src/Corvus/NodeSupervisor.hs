{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Per-node connection supervisor.
--
-- Each registered 'Node' has one supervisor 'Async' that holds
-- open both its @corvus-nodeagent@ and @corvus-netd@ connections
-- and reconciles per-node state (running networks, VM monitors)
-- on every (re)connect. The supervisor list lives in
-- 'ssAgents'; cancelling the supervisor closes its Cap'n Proto
-- brackets.
--
-- The daemon process spawns supervisors at boot (via
-- 'spawnAllNodeSupervisors') and on @crv node add@
-- (via 'spawnNodeSupervisor' from 'Handlers/Node.hs'). They tear
-- down on daemon shutdown (cancelled by 'app/daemon/Main.hs') or
-- on @crv node delete@ (cancelled by 'Handlers/Node.hs').
module Corvus.NodeSupervisor
  ( spawnAllNodeSupervisors
  , spawnNodeSupervisor
  , reapplyRunningNetworks
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Nodeagent as CGNA
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race_)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import qualified Control.Exception as E
import Control.Monad (forM_, unless, when)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Handlers.Network (autostartNetworksOnNode)
import qualified Corvus.Handlers.Network.PeerSpec as PS
import Corvus.Handlers.Vm.Monitor (reattachVmMonitors)
import Corvus.Handlers.Vm.Start (autostartVmsOnNode)
import Corvus.Handlers.VmStatusSink (newDaemonVmStatusSink)
import qualified Corvus.Model as M
import qualified Corvus.NetAgentClient as NA
import Corvus.Node.StatusFreshness (newStatusFreshness, waitForStaleStatus)
import qualified Corvus.NodeAgentClient as NOA
import qualified Corvus.Tls as Tls
import Corvus.Types
  ( NodeConns (..)
  , ServerState (..)
  , claimAutostartSlot
  , clearNetConn
  , clearNodeConn
  , newAutostartFlags
  , registerNodeConns
  , runFilteredLogging
  )
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Database.Persist (Entity (..), selectList, (==.))
import Database.Persist.Sql (SqlPersistT, fromSqlKey, runSqlPool)
import GHC.Clock (getMonotonicTime)
import Supervisors (withSupervisor)
import System.Posix.User (getRealUserID)
import System.Timeout (timeout)

-- | Boot-time: read every 'Node' row and fork its supervisor.
-- Returns the list of supervisor 'Async's so the daemon can
-- cancel them at shutdown. Each supervisor also self-registers
-- in 'ssAgents' (see 'spawnNodeSupervisor') so handlers can
-- find them at runtime.
spawnAllNodeSupervisors :: ServerState -> IO [Async ()]
spawnAllNodeSupervisors state = do
  nodes <- runSqlPool (selectList [] []) (ssDbPool state)
  runFilteredLogging (ssLogLevel state) $
    logInfoN $
      "Spawning supervisors for " <> T.pack (show (length nodes)) <> " node(s)"
  mapM (spawnNodeSupervisor state) nodes

-- | Fork one reconnect-loop supervisor for a single 'Node' row.
-- The supervisor maintains two parallel connect-and-hold async
-- children: one for the node's @corvus-nodeagent@, one for its
-- @corvus-netd@. Both pump updates into the 'NodeConns' entry
-- in 'ssAgents'.
--
-- The returned 'Async' is what the daemon (or @crv node delete@)
-- cancels to tear everything down for this node.
spawnNodeSupervisor :: ServerState -> Entity M.Node -> IO (Async ())
spawnNodeSupervisor state (Entity nodeKey node) = do
  sup <- async (runNodeSupervisor state nodeKey node)
  (vmAS, netAS) <- newAutostartFlags
  registerNodeConns
    state
    nodeKey
    NodeConns
      { ncNodeAgent = Nothing
      , ncNetAgent = Nothing
      , ncSupervisor = sup
      , ncVmAutostartFired = vmAS
      , ncNetAutostartFired = netAS
      }
  pure sup

-- | Per-node supervisor body: fan out two reconnect loops, one
-- for nodeagent, one for netd. Block until shutdown so the
-- daemon's 'cancel' on this 'Async' tears down both children.
runNodeSupervisor :: ServerState -> M.NodeId -> M.Node -> IO ()
runNodeSupervisor state nodeKey node = do
  uid <- getRealUserID
  let owner = T.pack (show uid)
      host = T.unpack (M.nodeHost node)
      noaPort = M.nodeNodeAgentPort node
      netPort = M.nodeNetAgentPort node
      nodeLabel = M.nodeName node
      -- Specialise the daemon's TLS config to expect this node's
      -- @corvus-node:<name>@ / @corvus-netd:<name>@ peer cert.
      nodeagentTls =
        Tls.withPeerExpectation Tls.RoleNode (Just nodeLabel)
          <$> ssTlsConfig state
      netdTls =
        Tls.withPeerExpectation Tls.RoleNetd (Just nodeLabel)
          <$> ssTlsConfig state
  let netdDisabled = M.nodeNetdDisabled node
  runFilteredLogging (ssLogLevel state) $
    logInfoN $
      "node "
        <> nodeLabel
        <> " supervisor starting (nodeagent "
        <> T.pack (show noaPort)
        <> ", netd "
        <> (if netdDisabled then "disabled" else T.pack (show netPort))
        <> ")"
  nodeAgentChild <-
    async $ runNodeAgentLoop state nodeKey nodeLabel host noaPort owner nodeagentTls
  netdChild <-
    if netdDisabled
      then async (pure ())
      else async $ runNetdLoop state nodeKey nodeLabel host netPort owner netdTls
  blockUntilShutdown state
  cancel nodeAgentChild
  cancel netdChild

-- | Single-node nodeagent connect-and-hold loop.
runNodeAgentLoop
  :: ServerState
  -> M.NodeId
  -> T.Text
  -- ^ node display name (for logs)
  -> String
  -> Int
  -> T.Text
  -- ^ owner
  -> Maybe Tls.TlsConfig
  -- ^ per-node TLS config (peer = @corvus-node:<name>@) or
  -- 'Nothing' for plaintext
  -> IO ()
runNodeAgentLoop state nodeKey nodeLabel host port owner mTlsCfg = loop
  where
    -- 'withNodeAgentClient' rethrows exceptions raised AFTER the
    -- body cap was handed in (e.g. the @withConn@ transport
    -- tearing down when the remote nodeagent restarts). That's
    -- the right shape for the test fixture, where we want the
    -- real failure to surface; here in production we MUST keep
    -- the reconnect loop alive, so wrap the call in 'try' and
    -- treat any escape as "retry in 5s" — same shape as a
    -- 'Left'-returning dial failure.
    loop = do
      result <-
        E.try @E.SomeException $
          NOA.withNodeAgentClient host port owner mTlsCfg onConnect
      shouldStop <- readTVarIO (ssShutdownFlag state)
      if shouldStop
        then pure ()
        else do
          case result of
            Right (Right ()) -> pure ()
            Right (Left _) -> threadDelay 5000000
            Left e -> do
              runFilteredLogging (ssLogLevel state) $
                logWarnN $
                  "node "
                    <> nodeLabel
                    <> " nodeagent session ended: "
                    <> T.pack (show e)
                    <> "; reconnecting in 5s"
              threadDelay 5000000
          loop

    onConnect (Left e) = do
      runFilteredLogging (ssLogLevel state) $
        logWarnN $
          "node "
            <> nodeLabel
            <> " nodeagent dial failed: "
            <> T.pack (show e)
            <> "; retrying in 5s"
      pure (Left ())
    onConnect (Right noac) = do
      pingResult <- timeout 10000000 (NOA.sessionPing noac)
      case pingResult of
        Nothing -> do
          runFilteredLogging (ssLogLevel state) $
            logWarnN $
              "node " <> nodeLabel <> " nodeagent session ping timed out; retrying in 5s"
          pure (Left ())
        Just (Left e) -> do
          runFilteredLogging (ssLogLevel state) $
            logWarnN $
              "node "
                <> nodeLabel
                <> " nodeagent session ping failed: "
                <> T.pack (show e)
          pure (Left ())
        Just (Right ()) -> do
          runFilteredLogging (ssLogLevel state) $
            logInfoN $
              "node "
                <> nodeLabel
                <> " nodeagent dial succeeded, owner="
                <> NOA.nacOwner noac
          atomically $
            modifyTVar' (ssAgents state) $ \m -> case Map.lookup nodeKey m of
              Nothing -> m
              Just nc -> Map.insert nodeKey nc {ncNodeAgent = Just noac} m
          holdNodeAgentConnection state nodeKey nodeLabel noac
            `E.finally` clearNodeConn state nodeKey
          -- Both registration failure and stale status must back off before redial.
          pure (Left ())

-- | Missing snapshots indicate a lost subscription even if RPC pings still work.
-- Keep the watcher active during reconciliation/autostart as well as steady state.
holdNodeAgentConnection :: ServerState -> M.NodeId -> T.Text -> NOA.NodeAgentClient -> IO ()
holdNodeAgentConnection state nodeKey nodeLabel noac = withSupervisor $ \sup -> do
  freshness <- newStatusFreshness getMonotonicTime
  sinkClient <- C.export @CGNA.VmStatusSink sup (newDaemonVmStatusSink state nodeKey freshness)
  result <- timeout 10000000 (NOA.subscribeVmStatus noac sinkClient)
  case result of
    Nothing -> warn "subscribeVmStatus timed out after 10s; reconnecting in 5s"
    Just (Left err) -> warn ("subscribeVmStatus failed: " <> T.pack (show err) <> "; reconnecting in 5s")
    Just (Right ()) -> do
      runFilteredLogging (ssLogLevel state) $
        logInfoN $
          "node " <> nodeLabel <> " subscribed to VM status push"
      race_ (watch freshness) $ do
        reattachVmMonitors state
        -- The lifetime guard survives reconnects; never repeat autostart on a flap.
        shouldFire <- claimAutostartSlot state nodeKey ncVmAutostartFired
        when shouldFire $ autostartVmsOnNode state nodeKey
        blockUntilShutdown state
  where
    warn message = runFilteredLogging (ssLogLevel state) $ logWarnN $ "node " <> nodeLabel <> " " <> message
    watch freshness = do
      elapsed <- waitForStaleStatus freshness (readTVarIO (ssShutdownFlag state)) (threadDelay 5000000) 45
      forM_ elapsed $ \seconds ->
        warn ("no processed VM status snapshot for " <> T.pack (show seconds) <> "s; reconnecting in 5s")

-- | Single-node netd connect-and-hold loop.
runNetdLoop
  :: ServerState
  -> M.NodeId
  -> T.Text
  -- ^ node display name
  -> String
  -> Int
  -> T.Text
  -- ^ owner
  -> Maybe Tls.TlsConfig
  -- ^ per-node TLS config (peer = @corvus-netd:<name>@) or
  -- 'Nothing' for plaintext
  -> IO ()
runNetdLoop state nodeKey nodeLabel host port owner mTlsCfg = loop
  where
    loop = do
      result <- NA.withNetAgentClient host port owner mTlsCfg onConnect
      shouldStop <- readTVarIO (ssShutdownFlag state)
      if shouldStop
        then pure ()
        else do
          case result of
            Right () -> pure ()
            Left _ -> threadDelay 5000000
          loop

    onConnect (Left e) = do
      runFilteredLogging (ssLogLevel state) $
        logWarnN $
          "node "
            <> nodeLabel
            <> " netd dial failed: "
            <> T.pack (show e)
            <> "; retrying in 5s"
      pure (Left ())
    onConnect (Right nac) = do
      runFilteredLogging (ssLogLevel state) $
        logInfoN $
          "node "
            <> nodeLabel
            <> " netd dial succeeded, owner="
            <> NA.nacOwner nac
      atomically $
        modifyTVar' (ssAgents state) $ \m -> case Map.lookup nodeKey m of
          Nothing -> m
          Just nc -> Map.insert nodeKey nc {ncNetAgent = Just nac} m
      reapplyRunningNetworks state nodeKey nodeLabel nac
      -- First-connect-only per-node network autostart. Mirrors
      -- the VM-autostart hook on the nodeagent side: gated by
      -- 'claimAutostartSlot' so a netd flap doesn't re-issue
      -- 'network start' on networks the operator may have
      -- stopped post-startup. 'reapplyRunningNetworks' above
      -- already covers the "was-running before the flap" set;
      -- this hook strictly handles "marked autostart but not
      -- yet running".
      shouldFireNetAutostart <-
        claimAutostartSlot state nodeKey ncNetAutostartFired
      when shouldFireNetAutostart $
        autostartNetworksOnNode state nodeKey
      -- Block until the daemon shuts down OR the netd socket
      -- dies. The 'finally' is load-bearing: when the underlying
      -- TCP connection drops, 'withConn' (further up the stack
      -- in 'withNetAgentClient') interrupts this body with an
      -- async exception. Without 'finally', 'clearNetConn'
      -- would be skipped on that path and the cap would remain
      -- in 'ssAgents' — exactly the bug 'blockUntilShutdownOr-
      -- NetdDead' is meant to close.
      blockUntilShutdownOrNetdDead state nodeLabel nac
        `E.finally` clearNetConn state nodeKey
      pure (Right ())

-- | Re-apply every running network this node participates in (as
-- owner or peer). The agent is stateless, so this rebuilds its
-- kernel-side ledger from the daemon's DB intent: bridge, VXLAN
-- VTEP, BUM FDB, dnsmasq, NAT. For peer-side specs we strip CIDR /
-- NAT / DHCP (only the owner runs those), so the agent sees the
-- L2-only flavor.
reapplyRunningNetworks
  :: ServerState -> M.NodeId -> T.Text -> NA.NetAgentClient -> IO ()
reapplyRunningNetworks state nodeKey nodeLabel nac = do
  (targets, skippedPeerRows) <- loadReapplyTargets
  -- If any peer rows referenced networks that no longer exist, warn but
  -- continue with what we have. The supervisor will re-apply on next connect.
  when skippedPeerRows $
    runFilteredLogging (ssLogLevel state) $
      logWarnN $
        "node " <> nodeLabel <> " peer network row(s) disappeared from DB; skipping"
  forM_ targets $ \(role, Entity nwKey nw) -> reapplyOne role nwKey nw
  where
    -- Load owner and peer targets together, preserving the existing
    -- best-effort treatment of peer rows whose network vanished mid-read.
    loadReapplyTargets = do
      let pool = ssDbPool state
      ownedRows <-
        runSqlPool
          ( selectList
              [M.NetworkRunning ==. True, M.NetworkNodeId ==. nodeKey]
              []
          )
          pool
      peerRows <- runSqlPool peerNetworksFor pool
      let nwMap = Map.fromList [(enw, nw) | Entity enw nw <- ownedRows]
          owned = [(PS.RoleOwner, e) | e <- ownedRows]
          peered =
            [ (PS.RolePeer, Entity (M.networkPeerNetworkId (entityVal e)) nw)
            | e <- peerRows
            , let k = M.networkPeerNetworkId (entityVal e)
            , Just nw <- [Map.lookup k nwMap]
            ]
      pure (owned <> peered, length peered < length peerRows)

    peerNetworksFor :: SqlPersistT IO [Entity M.NetworkPeer]
    peerNetworksFor = do
      selectList [M.NetworkPeerNodeId ==. nodeKey] []

    reapplyOne :: PS.PeerRole -> M.NetworkId -> M.Network -> IO ()
    reapplyOne role nwKey nw = do
      collected <-
        runSqlPool (PS.collectNetworkMembers nw nwKey) (ssDbPool state)
      case collected of
        Left err -> warn ("collect members: " <> err) nw nwKey
        Right (owner, peers) -> do
          specResult <- buildReapplySpec role nwKey nw owner peers
          case specResult of
            Left err -> warn err nw nwKey
            Right spec -> do
              result <- NA.applyNetwork nac spec
              case result of
                Right _ -> info ("re-applied network " <> M.networkName nw)
                Left e ->
                  warn
                    ("applyNetwork failed: " <> T.pack (show e))
                    nw
                    nwKey

    buildReapplySpec role nwKey nw owner peers = do
      reservations <- case role of
        PS.RoleOwner ->
          runSqlPool (PS.collectHostReservations nwKey) (ssDbPool state)
        PS.RolePeer -> pure []
      -- The Node row passed to buildPeerSpec needs to be THIS node's row,
      -- not the owner's. Find it in members.
      let thisMember =
            case role of
              PS.RoleOwner -> Just owner
              PS.RolePeer -> case filter ((== nodeKey) . fst) peers of
                (m : _) -> Just m
                [] -> Nothing
      pure $ case thisMember of
        Nothing -> Left "this node has no NetworkPeer row"
        Just member ->
          case PS.buildPeerSpec nw (fromSqlKey nwKey) role member (owner : peers) reservations of
            Left err -> Left ("build spec: " <> err)
            Right spec -> Right spec

    warn msg _nw nwKey =
      runFilteredLogging (ssLogLevel state) $
        logWarnN $
          "node "
            <> nodeLabel
            <> " re-apply network "
            <> T.pack (show (fromSqlKey nwKey))
            <> ": "
            <> msg
    info msg =
      runFilteredLogging (ssLogLevel state) $
        logInfoN $
          "node " <> nodeLabel <> " " <> msg

-- | Spin until 'ssShutdownFlag' flips. Lets the per-node loops
-- block their bracket bodies so the TCP sockets stay open until
-- daemon shutdown.
blockUntilShutdown :: ServerState -> IO ()
blockUntilShutdown state = do
  shouldStop <- readTVarIO (ssShutdownFlag state)
  unless shouldStop $ do
    threadDelay 200000
    blockUntilShutdown state

-- | Variant of 'blockUntilShutdown' that also returns when the
-- supplied netd client stops answering pings. The daemon's
-- 'handleNodeList' surfaces a node as netd-connected iff
-- 'ncNetAgent' is 'Just', so when the netd process dies the
-- supervisor needs to clear that cap and re-enter the dial loop
-- — otherwise the registry holds a stale capability and
-- @crv node list@ reports a dead agent as "online".
--
-- We poll 'NA.ping' every 5 s (matches the existing 5 s
-- reconnect backoff). The ping itself is bounded by 'timeout':
-- when the peer dies ungracefully (SIGKILL, network drop) the
-- TLS read can stall indefinitely waiting for a 'close_notify'
-- that will never arrive — without the timeout, the loop would
-- block on the first failed ping and the cap would never be
-- cleared. Any ping error or a 10 s no-response is treated as
-- "dead", causes the caller's body to return so 'clearNetConn'
-- runs and the outer loop reconnects.
blockUntilShutdownOrNetdDead
  :: ServerState -> T.Text -> NA.NetAgentClient -> IO ()
blockUntilShutdownOrNetdDead state nodeLabel nac = loop
  where
    pingTimeoutMicros = 10000000 -- 10 s
    loop = do
      shouldStop <- readTVarIO (ssShutdownFlag state)
      unless shouldStop $ do
        threadDelay 5000000
        r <- timeout pingTimeoutMicros (NA.ping nac)
        case r of
          Nothing ->
            runFilteredLogging (ssLogLevel state) $
              logWarnN $
                "node "
                  <> nodeLabel
                  <> " netd liveness ping timed out (>"
                  <> T.pack (show (pingTimeoutMicros `div` 1000000))
                  <> "s); dropping stale cap"
          Just (Left e) ->
            runFilteredLogging (ssLogLevel state) $
              logWarnN $
                "node "
                  <> nodeLabel
                  <> " netd liveness ping failed: "
                  <> T.pack (show e)
                  <> "; dropping stale cap"
          Just (Right ()) -> loop
