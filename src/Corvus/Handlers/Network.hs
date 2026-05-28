{-# LANGUAGE OverloadedStrings #-}

-- | Virtual network management handlers.
--
-- Phase 3: networks are managed by the privileged @corvus-netd@
-- agent. The daemon calls 'NA.applyNetwork' / 'NA.deleteNetwork'
-- with a spec derived from the network's DB row. There's no
-- fallback to the legacy user-ns path; if the agent isn't
-- reachable, network operations fail with "netd unavailable".
module Corvus.Handlers.Network
  ( -- * Action types
    NetworkCreate (..)
  , NetworkDelete (..)
  , NetworkStart (..)
  , NetworkStop (..)
  , NetworkEdit (..)
  , NetworkAttachNode (..)
  , NetworkDetachNode (..)

    -- * Handlers
  , handleNetworkCreate
  , handleNetworkDelete
  , handleNetworkStart
  , handleNetworkStop
  , handleNetworkList
  , handleNetworkShow
  , handleNetworkEdit
  , handleNetworkAttachNode
  , handleNetworkDetachNode

    -- * Shared helpers (used by NetIf + NodeSupervisor)
  , pushNetworkToAllMembers
  , autostartNetworksOnNode
  )
where

import Corvus.Action

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import qualified Corvus.Handlers.Network.Ipam as Ipam
import qualified Corvus.Handlers.Network.PeerSpec as PS
import Corvus.Handlers.Resolve (resolveNode, validateName)
import Corvus.Handlers.Scheduler (pickNodeForNetwork)
import Corvus.Model (Network (..), TaskId, TaskResult (..), TaskSubsystem (..), Vm (..), VmStatus (..))
import qualified Corvus.Model as M
import qualified Corvus.NetAgentClient as NA
import Corvus.NetAgentClient.Spec (corvusBridgeName, networkToSpec)
import Corvus.NodeRouting (withNetworkNetAgent)
import Corvus.Protocol
import Corvus.Types (ServerState (..), runServerLogging, withNetAgent)
import Corvus.Utils.Network (validateSubnet)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlPersistT, fromSqlKey, toSqlKey)

--------------------------------------------------------------------------------
-- Network Handlers
--------------------------------------------------------------------------------

-- | Create a new virtual network
handleNetworkCreate
  :: ServerState
  -> Text
  -- ^ name
  -> Text
  -- ^ node ref (name or numeric id)
  -> Text
  -- ^ subnet
  -> Bool
  -> Bool
  -> Bool
  -> IO Response
handleNetworkCreate state name nodeRefText subnet dhcp nat autostart =
  case validateName "Network" name of
    Left err -> pure $ RespNetworkError err
    Right () -> do
      let pool = ssDbPool state
      -- Empty text or capnp's unset-EntityRef default ('byId 0')
      -- both mean "no explicit placement" — defer to the scheduler.
      eNodeKey <-
        if T.null nodeRefText || nodeRefText == "0"
          then pickNodeForNetwork state
          else do
            r <- resolveNode (Ref nodeRefText) pool
            pure $ fmap (M.toSqlKey :: Int64 -> M.NodeId) r
      case eNodeKey of
        Left err -> pure $ RespNetworkError err
        Right nodeKey -> do
          mNode <- runSqlPool (get nodeKey) pool
          case mNode of
            Just n
              | M.nodeNetdDisabled n ->
                  pure $
                    RespNetworkError $
                      "Node '"
                        <> M.nodeName n
                        <> "' has netdDisabled=true; managed networks are not allowed."
            _ -> createWithSubnet nodeKey
  where
    createWithSubnet nodeKey = do
      let pool = ssDbPool state
          validatedSubnet
            | T.null subnet = Right ""
            | otherwise = validateSubnet subnet
      case validatedSubnet of
        Left err -> pure $ RespNetworkError $ "Invalid subnet: " <> err
        Right normalizedSubnet -> do
          if dhcp && T.null normalizedSubnet
            then pure $ RespNetworkError "DHCP requires a subnet"
            else
              if nat && T.null normalizedSubnet
                then pure $ RespNetworkError "NAT requires a subnet"
                else do
                  now <- getCurrentTime
                  let network =
                        Network
                          { networkName = name
                          , networkNodeId = nodeKey
                          , networkSubnet = normalizedSubnet
                          , networkDhcp = dhcp
                          , networkNat = nat
                          , networkRunning = False
                          , networkDnsmasqPid = Nothing
                          , networkCreatedAt = now
                          , networkAutostart = autostart
                          , networkVni = Nothing
                          }
                  result <- runSqlPool (insertUnique network) pool
                  case result of
                    Nothing -> pure $ RespNetworkError $ "Network with name '" <> name <> "' already exists"
                    Just key -> pure $ RespNetworkCreated $ fromSqlKey key

-- | Delete a virtual network
handleNetworkDelete :: ServerState -> Int64 -> IO Response
handleNetworkDelete state networkId = do
  let key = toSqlKey networkId :: M.NetworkId
  result <- runSqlPool (getAndCheckDelete key) (ssDbPool state)
  case result of
    Nothing -> pure RespNetworkNotFound
    Just False -> pure RespNetworkInUse
    Just True -> pure RespNetworkDeleted
  where
    getAndCheckDelete key = do
      mNetwork <- get key
      case mNetwork of
        Nothing -> pure Nothing
        Just network -> do
          -- Check if any network interfaces reference this network
          refs <- count [M.NetworkInterfaceNetworkId ==. Just key]
          if refs > 0
            then pure $ Just False
            else do
              -- Check network is not running
              if networkRunning network
                then pure $ Just False
                else do
                  delete key
                  pure $ Just True

-- | Start a virtual network. Pushes the spec to every member
-- (owner + peers) so a multi-node network goes live everywhere at
-- once. For single-node networks this reduces to the original
-- one-shot 'applyNetwork' against the owner.
handleNetworkStart :: ServerState -> Int64 -> TaskId -> IO Response
handleNetworkStart state networkId _parentTaskId = runServerLogging state $ do
  let key = toSqlKey networkId :: M.NetworkId
      pool = ssDbPool state
      nwName = T.pack (show networkId)
  mNetwork <- liftIO $ runSqlPool (get key) pool
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network
      | networkRunning network -> pure RespNetworkAlreadyRunning
      | otherwise ->
          -- The legacy single-node start used networkToSpec to fail
          -- fast on bad CIDR. Keep that gate before we touch any
          -- agents — invalid specs surface as a clean error rather
          -- than a partial multi-node push.
          case networkToSpec networkId network of
            Left err -> do
              logWarnN $ "Bad network spec for " <> nwName <> ": " <> err
              pure $ RespNetworkError err
            Right _ -> do
              logInfoN $ "Starting network " <> nwName <> " via netd"
              pushRes <- liftIO $ pushNetworkToAllMembers state key
              case pushRes of
                Left err -> do
                  logWarnN $ "netd unavailable; cannot start network: " <> err
                  pure $ RespNetworkError err
                Right () -> do
                  -- Re-read the owner's NetworkInfo so we can record
                  -- its dnsmasq pid in the DB. This is best-effort;
                  -- if listNetworks fails we still flip running=True.
                  pid <- liftIO $ ownerDnsmasqPid state networkId network
                  liftIO $
                    runSqlPool
                      ( update
                          key
                          [ M.NetworkRunning =. True
                          , M.NetworkDnsmasqPid =. pid
                          ]
                      )
                      pool
                  logInfoN $ "Network " <> nwName <> " started"
                  pure RespNetworkStarted

-- | Ask the owner's netd for the network's current dnsmasq PID.
-- Returns 'Nothing' if the network has no DHCP, the netd is
-- unreachable, or the network isn't in the agent's ledger.
ownerDnsmasqPid :: ServerState -> Int64 -> Network -> IO (Maybe Int)
ownerDnsmasqPid state networkId network = do
  outer <- withNetAgent state (M.networkNodeId network) NA.listNetworks
  let bridgeName = corvusBridgeName networkId
  pure $ case outer of
    Right (Right infos) ->
      case [NA.niDnsmasqPid i | i <- infos, NA.nsName (NA.niSpec i) == bridgeName] of
        (p : _) | p /= 0 -> Just (fromIntegral p)
        _ -> Nothing
    _ -> Nothing

-- | Stop a virtual network. Phase 3: delegates to the agent
-- via 'NA.deleteNetwork'.
handleNetworkStop :: ServerState -> Int64 -> Bool -> TaskId -> IO Response
handleNetworkStop state networkId force _parentTaskId = runServerLogging state $ do
  let key = toSqlKey networkId :: M.NetworkId
      pool = ssDbPool state
  mNetwork <- liftIO $ runSqlPool (get key) pool
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network
      | not (networkRunning network) -> pure RespNetworkNotRunning
      | otherwise -> do
          if not force
            then do
              hasRunningVms <- liftIO $ runSqlPool (checkRunningVms key) pool
              if hasRunningVms
                then pure RespNetworkInUse
                else doStop key network
            else doStop key network
  where
    doStop key _network = do
      logInfoN $ "Stopping network " <> T.pack (show networkId)
      outer <- liftIO $ withNetworkNetAgent state networkId $ \nac ->
        NA.deleteNetwork nac (corvusBridgeName networkId)
      case outer of
        Left err -> do
          logWarnN $ "netd unavailable; cannot stop network: " <> err
          pure $ RespNetworkError err
        Right result -> do
          case result of
            Left e ->
              logWarnN $
                "deleteNetwork failed for "
                  <> T.pack (show networkId)
                  <> ": "
                  <> T.pack (show e)
            Right () -> pure ()
          -- Clear DB state regardless of agent response — the agent's
          -- destroy is idempotent and a stale ledger entry that
          -- happens to survive will be reaped by its next
          -- 'applyNetwork' delta or by an agent restart's cleanup pass.
          liftIO $
            runSqlPool
              (update key [M.NetworkRunning =. False, M.NetworkDnsmasqPid =. Nothing])
              (ssDbPool state)
          logInfoN $ "Network " <> T.pack (show networkId) <> " stopped"
          pure RespNetworkStopped

    checkRunningVms key = do
      netIfs <- selectList [M.NetworkInterfaceNetworkId ==. Just key] []
      let vmKeys = map (M.networkInterfaceVmId . entityVal) netIfs
      vms <- mapM get vmKeys
      let runningVms = filter isRunning (zip vmKeys vms)
      pure $ not (null runningVms)

    isRunning (_, Just vm) = vmStatus vm == VmRunning || vmStatus vm == M.VmPaused
    isRunning (_, Nothing) = False

-- | List all virtual networks
handleNetworkList :: ServerState -> IO Response
handleNetworkList state = do
  let pool = ssDbPool state
  networks <- runSqlPool (selectList [] [Asc M.NetworkName]) pool
  infos <-
    mapM
      ( \(Entity key network) -> do
          peers <- runSqlPool (peerIdsFor key) pool
          pure $ toNetworkInfoWith (fromSqlKey key) network peers
      )
      networks
  pure $ RespNetworkList infos

-- | Show virtual network details
handleNetworkShow :: ServerState -> Int64 -> IO Response
handleNetworkShow state networkId = do
  let key = toSqlKey networkId :: M.NetworkId
      pool = ssDbPool state
  mNetwork <- runSqlPool (get key) pool
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network -> do
      peers <- runSqlPool (peerIdsFor key) pool
      pure $ RespNetworkDetails $ toNetworkInfoWith networkId network peers

peerIdsFor :: M.NetworkId -> SqlPersistT IO [Int64]
peerIdsFor key = do
  rows <- selectList [M.NetworkPeerNetworkId ==. key] []
  pure
    [ fromSqlKey (M.networkPeerNodeId (entityVal r))
    | r <- rows
    ]

-- | Edit virtual network properties.
-- Subnet, DHCP, and NAT require the network to be stopped.
-- Autostart can be toggled regardless of state.
handleNetworkEdit :: ServerState -> Int64 -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> IO Response
handleNetworkEdit state networkId mSubnet mDhcp mNat mAutostart = do
  let key = toSqlKey networkId :: M.NetworkId
      pool = ssDbPool state
  mNetwork <- runSqlPool (get key) pool
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network -> do
      let hasNonAutostartEdits = isJust mSubnet || isJust mDhcp || isJust mNat
      if hasNonAutostartEdits && networkRunning network
        then pure $ RespNetworkError "Network must be stopped to change subnet/dhcp/nat"
        else do
          -- Validate and normalize subnet if provided
          mNormalizedSubnet <- case mSubnet of
            Just s
              | T.null s -> pure $ Right (Just "")
              | otherwise -> case validateSubnet s of
                  Left err -> pure $ Left $ "Invalid subnet: " <> err
                  Right normalized -> pure $ Right (Just normalized)
            Nothing -> pure $ Right Nothing
          case mNormalizedSubnet of
            Left err -> pure $ RespNetworkError err
            Right mSub -> do
              let effectiveSubnet = fromMaybe (networkSubnet network) mSub
                  effectiveDhcp = fromMaybe (networkDhcp network) mDhcp
                  effectiveNat = fromMaybe (networkNat network) mNat
              if effectiveDhcp && T.null effectiveSubnet
                then pure $ RespNetworkError "DHCP requires a subnet"
                else
                  if effectiveNat && T.null effectiveSubnet
                    then pure $ RespNetworkError "NAT requires a subnet"
                    else do
                      let updates =
                            maybe [] (\s -> [M.NetworkSubnet =. s]) mSub
                              ++ maybe [] (\d -> [M.NetworkDhcp =. d]) mDhcp
                              ++ maybe [] (\n -> [M.NetworkNat =. n]) mNat
                              ++ maybe [] (\a -> [M.NetworkAutostart =. a]) mAutostart
                      case updates of
                        [] -> pure RespNetworkEdited
                        us -> do
                          runSqlPool (update key us) pool
                          pure RespNetworkEdited

--------------------------------------------------------------------------------
-- Peer management
--------------------------------------------------------------------------------

-- | Resolve a node ref and add it to @NetworkPeer@ if not already
-- present. Allocates a VNI on first attach so single-node networks
-- stay VNI-less until they actually need one. After the DB update
-- we re-push the spec to every existing member so their peer lists
-- include the new node, and push the initial spec to the new peer.
handleNetworkAttachNode :: ServerState -> Int64 -> Text -> IO Response
handleNetworkAttachNode state networkId nodeRefText = runServerLogging state $ do
  let networkKey = toSqlKey networkId :: M.NetworkId
      pool = ssDbPool state
  mNetwork <- liftIO $ runSqlPool (get networkKey) pool
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network -> do
      resolved <- liftIO $ resolveNode (Ref nodeRefText) pool
      case resolved of
        Left err -> pure $ RespNetworkError err
        Right peerNodeIdRaw -> do
          let peerNodeId = toSqlKey peerNodeIdRaw :: M.NodeId
          mPeerNode <- liftIO $ runSqlPool (get peerNodeId) pool
          case mPeerNode of
            Nothing -> pure $ RespNetworkError "node row vanished after resolve"
            Just peerNode
              | peerNodeId == M.networkNodeId network ->
                  pure $ RespNetworkError "node is the network's owner"
              | M.nodeAdminState peerNode /= M.NodeOnline ->
                  pure $
                    RespNetworkError
                      ("destination node " <> M.nodeName peerNode <> " is not online")
              | otherwise -> attachInDb network peerNodeId peerNode
  where
    attachInDb network peerNodeId _peerNode = do
      let networkKey = toSqlKey networkId :: M.NetworkId
          pool = ssDbPool state
      already <-
        liftIO $
          runSqlPool
            (count [M.NetworkPeerNetworkId ==. networkKey, M.NetworkPeerNodeId ==. peerNodeId])
            pool
      if already > 0
        then pure $ RespNetworkError "node is already a peer of this network"
        else do
          -- Allocate a VNI if the network doesn't have one yet, in
          -- the same transaction as the NetworkPeer insert so a
          -- crash doesn't leave the peer without a VNI to talk on.
          liftIO $
            runSqlPool
              ( do
                  case M.networkVni network of
                    Just _ -> pure ()
                    Nothing -> do
                      used <- usedVnis
                      let v = Ipam.vniFromPool used
                      update networkKey [M.NetworkVni =. Just v]
                  insert_ (M.NetworkPeer networkKey peerNodeId)
              )
              pool
          if networkRunning network
            then do
              pushRes <- liftIO $ pushNetworkToAllMembers state networkKey
              case pushRes of
                Left err -> do
                  logWarnN $
                    "attach-node "
                      <> T.pack (show networkId)
                      <> " spec push failed: "
                      <> err
                  pure $ RespNetworkError err
                Right () -> pure RespNetworkPeerAttached
            else pure RespNetworkPeerAttached

    usedVnis :: SqlPersistT IO [Int]
    usedVnis = do
      ns <- selectList [] []
      pure $ mapMaybe (M.networkVni . entityVal) (ns :: [Entity Network])

-- | Remove a peer node from a network. After the DB change, tells
-- the departing peer's netd to forget the network and re-pushes
-- updated specs (one fewer peer in the flood list) to the rest.
handleNetworkDetachNode :: ServerState -> Int64 -> Text -> IO Response
handleNetworkDetachNode state networkId nodeRefText = runServerLogging state $ do
  let networkKey = toSqlKey networkId :: M.NetworkId
      pool = ssDbPool state
  mNetwork <- liftIO $ runSqlPool (get networkKey) pool
  case mNetwork of
    Nothing -> pure RespNetworkNotFound
    Just network -> do
      resolved <- liftIO $ resolveNode (Ref nodeRefText) pool
      case resolved of
        Left err -> pure $ RespNetworkError err
        Right peerNodeIdRaw ->
          let peerNodeId = toSqlKey peerNodeIdRaw :: M.NodeId
           in if peerNodeId == M.networkNodeId network
                then
                  pure $
                    RespNetworkError "cannot detach the network's owner; delete the network instead"
                else detachInDb network peerNodeId
  where
    detachInDb network peerNodeId = do
      let networkKey = toSqlKey networkId :: M.NetworkId
          pool = ssDbPool state
      deleted <-
        liftIO $
          runSqlPool
            ( deleteWhereCount
                [ M.NetworkPeerNetworkId ==. networkKey
                , M.NetworkPeerNodeId ==. peerNodeId
                ]
            )
            pool
      if deleted == 0
        then pure $ RespNetworkError "node is not a peer of this network"
        else do
          -- Tell the departing peer to drop the network entirely
          -- (best effort — if its netd is down the cleanup pass
          -- will reap on the next agent restart).
          dropRes <- liftIO $ withNetAgent state peerNodeId $ \nac ->
            NA.deleteNetwork nac (corvusBridgeName networkId)
          case dropRes of
            Left err ->
              logWarnN $
                "detach-node: dropping network on departing peer failed: " <> err
            Right (Left e) ->
              logWarnN $
                "detach-node: peer netd deleteNetwork errored: " <> T.pack (show e)
            Right (Right ()) -> pure ()
          -- Re-push to the remaining members so they prune the
          -- departed peer from their flood list.
          if networkRunning network
            then do
              pushRes <- liftIO $ pushNetworkToAllMembers state networkKey
              case pushRes of
                Left err -> do
                  logWarnN $ "detach-node spec re-push failed: " <> err
                  pure $ RespNetworkError err
                Right () -> pure RespNetworkPeerDetached
            else pure RespNetworkPeerDetached

    -- Persistent's deleteWhereCount returns the number of rows
    -- removed; we use it to detect "no such peer" without an
    -- extra read.
    deleteWhereCount
      :: [Filter M.NetworkPeer]
      -> SqlPersistT IO Int
    deleteWhereCount fs = do
      ns <- selectList fs []
      mapM_ (delete . entityKey) ns
      pure (length ns)

-- | Build the per-node 'NA.NetworkSpec' for every member and push
-- | Per-node network autostart pass. Called by the per-node
-- supervisor's netd @onConnect@ callback the FIRST time it lands
-- a successful dial — see 'claimAutostartSlot' for the
-- once-per-supervisor-lifetime gate.
--
-- By the time we get here, @ncNetAgent@ for this node is in
-- 'ssAgents', so 'NetworkStart' downstream can reach the netd
-- without the daemon-startup race the global autostart loop hit.
-- 'reapplyRunningNetworks' has also already run, re-applying
-- kernel state for networks marked @running=True@; autostart
-- strictly handles the @running=False, autostart=True@ side.
autostartNetworksOnNode :: ServerState -> M.NodeId -> IO ()
autostartNetworksOnNode state nodeId = do
  let pool = ssDbPool state
  nws <-
    runSqlPool
      ( selectList
          [ M.NetworkAutostart ==. True
          , M.NetworkNodeId ==. nodeId
          , M.NetworkRunning ==. False
          ]
          [Asc M.NetworkName]
      )
      pool
  runServerLogging state $
    unless (null nws) $ do
      logInfoN $
        "Autostarting "
          <> T.pack (show (length nws))
          <> " network(s) on node "
          <> T.pack (show (M.fromSqlKey nodeId))
      forM_ nws $ \(Entity nwKey nw) -> do
        resp <- liftIO $ runAction state autostartClientName (NetworkStart (M.fromSqlKey nwKey))
        case classifyResponse resp of
          (TaskError, Just err) ->
            logWarnN $ "Failed to autostart network " <> M.networkName nw <> ": " <> err
          _ -> logInfoN $ "Autostarted network " <> M.networkName nw

-- | @client_name@ on task rows produced by per-node autostart.
-- Surfaces in @crv task history@ so operators can tell
-- autostart-driven starts apart from operator-issued ones.
autostartClientName :: Text
autostartClientName = "system-autostart"

-- via 'withNetAgent'. Called from 'handleNetworkStart' (initial
-- apply), 'handleNetworkAttachNode' / 'handleNetworkDetachNode' (peer
-- set changed), and the NodeSupervisor reconnect path (slice 8).
pushNetworkToAllMembers :: ServerState -> M.NetworkId -> IO (Either Text ())
pushNetworkToAllMembers state networkKey = do
  let pool = ssDbPool state
  mNetwork <- runSqlPool (get networkKey) pool
  case mNetwork of
    Nothing -> pure (Left "network not found")
    Just network -> do
      collected <- runSqlPool (PS.collectNetworkMembers network networkKey) pool
      case collected of
        Left err -> pure (Left err)
        Right (owner, peers) -> do
          reservations <- runSqlPool (PS.collectHostReservations networkKey) pool
          let nwIdInt = fromSqlKey networkKey
              ownerSpec =
                PS.buildPeerSpec
                  network
                  nwIdInt
                  PS.RoleOwner
                  owner
                  (owner : peers)
                  reservations
              peerSpecs =
                [ ( pid
                  , PS.buildPeerSpec
                      network
                      nwIdInt
                      PS.RolePeer
                      (pid, prow)
                      (owner : peers)
                      []
                  )
                | (pid, prow) <- peers
                ]
          case ownerSpec of
            Left err -> pure (Left ("owner spec: " <> err))
            Right spec -> do
              -- Push to the owner first (it carries the
              -- authoritative dnsmasq). Failures on a single peer
              -- don't roll back — the supervisor will retry on
              -- reconnect.
              ownerRes <- pushOne (fst owner) spec
              forM_ peerSpecs $ \(pid, espec) ->
                case espec of
                  Left err ->
                    runServerLogging state $
                      logWarnN $
                        "peer spec for node "
                          <> T.pack (show (fromSqlKey pid))
                          <> ": "
                          <> err
                  Right ps -> do
                    r <- pushOne pid ps
                    case r of
                      Left err ->
                        runServerLogging state $
                          logWarnN $
                            "applyNetwork on peer "
                              <> T.pack (show (fromSqlKey pid))
                              <> " failed: "
                              <> err
                      Right () -> pure ()
              pure ownerRes
  where
    pushOne nid spec = do
      outer <- withNetAgent state nid $ \nac -> NA.applyNetwork nac spec
      case outer of
        Left err -> pure (Left err)
        Right (Left e) -> pure (Left (T.pack (show e)))
        Right (Right _) -> pure (Right ())

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

toNetworkInfoWith :: Int64 -> Network -> [Int64] -> NetworkInfo
toNetworkInfoWith nwId network peerIds =
  NetworkInfo
    { nwiId = nwId
    , nwiName = networkName network
    , nwiSubnet = networkSubnet network
    , nwiDhcp = networkDhcp network
    , nwiNat = networkNat network
    , nwiRunning = networkRunning network
    , nwiDnsmasqPid = networkDnsmasqPid network
    , nwiCreatedAt = networkCreatedAt network
    , nwiAutostart = networkAutostart network
    , nwiVni = networkVni network
    , nwiPeerNodeIds = peerIds
    }

--------------------------------------------------------------------------------
-- Action Types
--------------------------------------------------------------------------------

data NetworkCreate = NetworkCreate
  { ncrName :: Text
  , ncrNodeRef :: Text
  -- ^ Reference to the target node. Required as of multi-node
  -- slice 1c — no scheduler yet.
  , ncrSubnet :: Text
  , ncrDhcp :: Bool
  , ncrNat :: Bool
  , ncrAutostart :: Bool
  }

instance Action NetworkCreate where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "create"
  actionEntityName = Just . ncrName
  actionExecute ctx a = handleNetworkCreate (acState ctx) (ncrName a) (ncrNodeRef a) (ncrSubnet a) (ncrDhcp a) (ncrNat a) (ncrAutostart a)

newtype NetworkDelete = NetworkDelete {ndelNetworkId :: Int64}

instance Action NetworkDelete where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "delete"
  actionEntityId = Just . fromIntegral . ndelNetworkId
  actionExecute ctx a = handleNetworkDelete (acState ctx) (ndelNetworkId a)

data NetworkEdit = NetworkEdit
  { nedNetworkId :: Int64
  , nedSubnet :: Maybe Text
  , nedDhcp :: Maybe Bool
  , nedNat :: Maybe Bool
  , nedAutostart :: Maybe Bool
  }

instance Action NetworkEdit where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "edit"
  actionEntityId = Just . fromIntegral . nedNetworkId
  actionExecute ctx a = handleNetworkEdit (acState ctx) (nedNetworkId a) (nedSubnet a) (nedDhcp a) (nedNat a) (nedAutostart a)

-- Complex handlers with subtask creation

newtype NetworkStart = NetworkStart {nstartNetworkId :: Int64}

instance Action NetworkStart where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "start"
  actionEntityId = Just . fromIntegral . nstartNetworkId
  actionExecute ctx a = handleNetworkStart (acState ctx) (nstartNetworkId a) (acTaskId ctx)

data NetworkStop = NetworkStop
  { nstopNetworkId :: Int64
  , nstopForce :: Bool
  }

instance Action NetworkStop where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "stop"
  actionEntityId = Just . fromIntegral . nstopNetworkId
  actionExecute ctx a = handleNetworkStop (acState ctx) (nstopNetworkId a) (nstopForce a) (acTaskId ctx)

data NetworkAttachNode = NetworkAttachNode
  { nanNetworkId :: Int64
  , nanNodeRef :: Text
  }

instance Action NetworkAttachNode where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "attach-node"
  actionEntityId = Just . fromIntegral . nanNetworkId
  actionExecute ctx a =
    handleNetworkAttachNode (acState ctx) (nanNetworkId a) (nanNodeRef a)

data NetworkDetachNode = NetworkDetachNode
  { ndnNetworkId :: Int64
  , ndnNodeRef :: Text
  }

instance Action NetworkDetachNode where
  actionSubsystem _ = SubNetwork
  actionCommand _ = "detach-node"
  actionEntityId = Just . fromIntegral . ndnNetworkId
  actionExecute ctx a =
    handleNetworkDetachNode (acState ctx) (ndnNetworkId a) (ndnNodeRef a)
