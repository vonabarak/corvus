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
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Monad (filterM, forM_, unless)
import Control.Monad.Logger (logInfoN, logWarnN)
import qualified Corvus.Handlers.Network.PeerSpec as PS
import Corvus.Handlers.Vm (reattachVmMonitors)
import Corvus.Handlers.VmStatusSink (newDaemonVmStatusSink)
import qualified Corvus.Model as M
import qualified Corvus.NetAgentClient as NA
import qualified Corvus.NodeAgentClient as NOA
import qualified Corvus.Tls as Tls
import Corvus.Types
  ( NodeConns (..)
  , ServerState (..)
  , clearNetConn
  , clearNodeConn
  , registerNodeConns
  , runFilteredLogging
  )
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.Persist (Entity (..), get, selectList, (==.))
import Database.Persist.Postgresql (SqlPersistT, runSqlPool)
import Database.Persist.Sql (fromSqlKey)
import Supervisors (withSupervisor)
import System.Posix.User (getRealUserID)

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
  registerNodeConns
    state
    nodeKey
    NodeConns {ncNodeAgent = Nothing, ncNetAgent = Nothing, ncSupervisor = sup}
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
    loop = do
      result <-
        NOA.withNodeAgentClient host port owner mTlsCfg onConnect
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
            <> " nodeagent dial failed: "
            <> T.pack (show e)
            <> "; retrying in 5s"
      pure (Left ())
    onConnect (Right noac) = do
      pingResult <- NOA.sessionPing noac
      case pingResult of
        Left e -> do
          runFilteredLogging (ssLogLevel state) $
            logWarnN $
              "node "
                <> nodeLabel
                <> " nodeagent session ping failed: "
                <> T.pack (show e)
          pure (Left ())
        Right () -> do
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
          withSupervisor $ \sup -> do
            sinkClient <-
              C.export @CGNA.VmStatusSink sup (newDaemonVmStatusSink state nodeKey)
            subResult <- NOA.subscribeVmStatus noac sinkClient
            case subResult of
              Left e ->
                runFilteredLogging (ssLogLevel state) $
                  logWarnN $
                    "node "
                      <> nodeLabel
                      <> " subscribeVmStatus failed: "
                      <> T.pack (show e)
              Right () ->
                runFilteredLogging (ssLogLevel state) $
                  logInfoN $
                    "node " <> nodeLabel <> " subscribed to VM status push"
            reattachVmMonitors state
            blockUntilShutdown state
          clearNodeConn state nodeKey
          pure (Right ())

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
      blockUntilShutdown state
      clearNetConn state nodeKey
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
  let pool = ssDbPool state
  -- Networks where this node is the owner.
  ownedRows <-
    runSqlPool
      ( selectList
          [M.NetworkRunning ==. True, M.NetworkNodeId ==. nodeKey]
          []
      )
      pool
  -- Networks where this node is a peer (lookup via NetworkPeer).
  peerRows <- runSqlPool peerNetworksFor pool
  let owned = [(PS.RoleOwner, e) | e <- ownedRows]
      peered = [(PS.RolePeer, e) | e <- peerRows]
  forM_ (owned <> peered) $ \(role, Entity nwKey nw) -> reapplyOne role nwKey nw
  where
    peerNetworksFor :: SqlPersistT IO [Entity M.Network]
    peerNetworksFor = do
      pe <- selectList [M.NetworkPeerNodeId ==. nodeKey] []
      let nwKeys = map (M.networkPeerNetworkId . entityVal) pe
      -- Persistent doesn't have a built-in 'in' filter against keys
      -- that's compatible across our setup; do an in-memory lookup.
      mapM (\k -> Entity k . fromMaybe (error "missing network row") <$> get k)
        =<< filterM
          ( \k -> do
              mNw <- get k
              pure (maybe False M.networkRunning mNw)
          )
          nwKeys

    reapplyOne :: PS.PeerRole -> M.NetworkId -> M.Network -> IO ()
    reapplyOne role nwKey nw = do
      let nwIdInt = fromSqlKey nwKey
      collected <-
        runSqlPool (PS.collectNetworkMembers nw nwKey) (ssDbPool state)
      case collected of
        Left err -> warn ("collect members: " <> err) nw nwKey
        Right (owner, peers) -> do
          reservations <- case role of
            PS.RoleOwner ->
              runSqlPool (PS.collectHostReservations nwKey) (ssDbPool state)
            PS.RolePeer -> pure []
          -- The Node row passed to buildPeerSpec needs to be THIS
          -- node's row, not the owner's. Find it in members.
          let thisMember =
                case role of
                  PS.RoleOwner -> Just owner
                  PS.RolePeer -> case filter ((== nodeKey) . fst) peers of
                    (m : _) -> Just m
                    [] -> Nothing
          case thisMember of
            Nothing -> warn "this node has no NetworkPeer row" nw nwKey
            Just m ->
              case PS.buildPeerSpec nw nwIdInt role m (owner : peers) reservations of
                Left err -> warn ("build spec: " <> err) nw nwKey
                Right spec -> do
                  result <- NA.applyNetwork nac spec
                  case result of
                    Right _ ->
                      info ("re-applied network " <> M.networkName nw)
                    Left e ->
                      warn
                        ("applyNetwork failed: " <> T.pack (show e))
                        nw
                        nwKey

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
