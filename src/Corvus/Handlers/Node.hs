{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Node-management handlers (multi-node Phase 1).
--
-- A 'Node' row is the daemon's record of one host it orchestrates:
-- agent endpoints, administrative state, and the agent-pushed
-- capacity snapshot. This module provides the CRUD surface for
-- @crv node …@; per-node connection state, scheduling, and the
-- agent push payload land in later slices.
module Corvus.Handlers.Node
  ( -- * Action types
    NodeAdd (..)
  , NodeDelete (..)
  , NodeForget (..)
  , nodeForgetPreview
  , NodeEdit (..)
  , NodeSetState (..)

    -- * Handlers
  , handleNodeList
  , handleNodePing
  , handleNodeShow
  )
where

import Control.Concurrent.Async (cancel)
import Control.Concurrent.STM (atomically, modifyTVar', readTVar, readTVarIO)
import Control.Exception (mask_)
import qualified Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Network (pushNetworkToAllMembersWithWarnings)
import Corvus.Handlers.Resolve (validateName)
import Corvus.Handlers.Vm.Delete (deleteVm)
import Corvus.Model (Node (..))
import qualified Corvus.Model as M
import qualified Corvus.NetAgentClient as Net
import Corvus.NetAgentClient.Spec (corvusBridgeName)
import qualified Corvus.NodeAgentClient as NAC
import Corvus.NodeSupervisor (spawnNodeSupervisor)
import Corvus.Protocol
import qualified Corvus.Tls as Tls
import Corvus.Types
  ( NodeConns (..)
  , ServerState (..)
  , checkTaskCommit
  , lookupNodeAgent
  , removeNodeConns
  , runServerLogging
  , ssTlsConfig
  , withNetAgent
  )
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (SqlPersistT, fromSqlKey, runSqlPool, toSqlKey)
import qualified Database.Persist.Sql
import System.Posix.User (getRealUserID)
import System.Timeout (timeout)

--------------------------------------------------------------------------------
-- handleNodeAdd
--------------------------------------------------------------------------------

-- | Register a new node row. The connection-registry spawner
-- (slice 1c) will fork its reconnect loop on the next daemon
-- start; for now this just persists the metadata.
handleNodeAdd
  :: ServerState
  -> Text
  -- ^ name (unique)
  -> Text
  -- ^ host (ip or dns)
  -> Int
  -- ^ corvus-nodeagent TCP port
  -> Int
  -- ^ corvus-netd TCP port
  -> Text
  -- ^ daemon basePath on the node (where VM images live)
  -> Maybe Text
  -- ^ free-form description
  -> M.NodeAdminState
  -> Bool
  -- ^ netd-disabled
  -> IO Response
handleNodeAdd state name host nodeAgentPort netAgentPort basePath mDesc adminState netdDisabled =
  case validateName "Node" name of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Adding node: " <> name <> " (" <> host <> ":" <> T.pack (show nodeAgentPort) <> ")"
      now <- liftIO getCurrentTime
      resolved <-
        if not (T.null basePath)
          then pure (Right basePath)
          else
            if adminState == M.NodeOffline
              then pure (Left "Offline nodes require an explicit base path")
              else liftIO $ resolveBasePathFromAgent state name host nodeAgentPort
      case resolved of
        Left err -> pure (RespError err)
        Right effectiveBasePath -> do
          let node =
                Node
                  { nodeName = name
                  , nodeHost = host
                  , nodeNodeAgentPort = nodeAgentPort
                  , nodeNetAgentPort = netAgentPort
                  , nodeBasePath = effectiveBasePath
                  , nodeDescription = mDesc
                  , nodeAdminState = adminState
                  , nodeCreatedAt = now
                  , nodeCpuCount = Nothing
                  , nodeRamTotal = Nothing
                  , nodeRamFree = Nothing
                  , nodeStorageBytesTotal = Nothing
                  , nodeStorageBytesFree = Nothing
                  , nodeLoadAvg1 = Nothing
                  , nodeLoadAvg5 = Nothing
                  , nodeLoadAvg15 = Nothing
                  , nodeKernelRelease = Nothing
                  , nodeAgentVersion = Nothing
                  , nodeNodeAgentHealthcheck = Nothing
                  , nodeNetAgentHealthcheck = Nothing
                  , nodeNetdDisabled = netdDisabled
                  }
          result <- liftIO $ runSqlPool (insertUnique node) (ssDbPool state)
          case result of
            Just key -> do
              -- Fork the per-node reconnect supervisor right away so
              -- the operator doesn't need to restart the daemon
              -- before the new node starts accepting workloads.
              Control.Monad.when (adminState /= M.NodeOffline) $
                liftIO $
                  Control.Monad.void $
                    spawnNodeSupervisor state (Entity key node)
              pure $ RespNodeCreated (fromSqlKey key)
            Nothing ->
              pure $
                RespError $
                  "Node with name '"
                    <> name
                    <> "' or address '"
                    <> host
                    <> ":"
                    <> T.pack (show nodeAgentPort)
                    <> "' already exists"

-- | Dial the remote nodeagent at @host:port@ and ask it for its
-- preferred @basePath@. Used by 'handleNodeAdd' when the operator
-- did not pass @--base-path@.
--
-- The TLS config is the daemon's own material with the peer's
-- expected CN swapped to @corvus-node:<name>@, mirroring what
-- 'Corvus.NodeSupervisor.runNodeSupervisor' does for the
-- steady-state connect loop. A registration that arrives before
-- the agent's cert is deployed will fail the TLS handshake — the
-- operator gets a clear "TLS peer rejected" diagnostic and can
-- re-run after running @corvus-admin deploy node@.
resolveBasePathFromAgent
  :: ServerState
  -> Text
  -- ^ node name (for the TLS peer expectation)
  -> Text
  -- ^ host
  -> Int
  -- ^ port
  -> IO (Either Text Text)
resolveBasePathFromAgent state nodeLabel host port = do
  uid <- getRealUserID
  let owner = T.pack (show uid)
      mTls = Tls.withPeerExpectation Tls.RoleNode (Just nodeLabel) <$> ssTlsConfig state
  NAC.withNodeAgentClient (T.unpack host) port owner mTls $ \case
    Left e ->
      pure
        ( Left
            ( "could not reach nodeagent at "
                <> host
                <> ":"
                <> T.pack (show port)
                <> ": "
                <> T.pack (show e)
            )
        )
    Right nac -> do
      r <- NAC.agentDefaultBasePath nac
      case r of
        Left e -> pure (Left ("nodeagent rejected defaultBasePath: " <> T.pack (show e)))
        Right p
          | T.null p -> pure (Left "nodeagent returned an empty basePath")
          | otherwise -> pure (Right p)

--------------------------------------------------------------------------------
-- handleNodeDelete
--------------------------------------------------------------------------------

-- | Remove a node. Refuses while any VM, network, or
-- 'DiskImageNode' join row still references it; slice 1c will
-- also cancel the per-node reconnect loop.
handleNodeDelete :: ServerState -> Int64 -> IO Response
handleNodeDelete state nodeId = runServerLogging state $ do
  let key = toSqlKey nodeId :: M.NodeId
      pool = ssDbPool state
  mNode <- liftIO $ runSqlPool (get key) pool
  case mNode of
    Nothing -> pure RespNodeNotFound
    Just node -> do
      vmCount <- liftIO $ runSqlPool (count [M.VmNodeId ==. key]) pool
      netCount <- liftIO $ runSqlPool (count [M.NetworkNodeId ==. key]) pool
      diskCount <- liftIO $ runSqlPool (count [M.DiskImageNodeNodeId ==. key]) pool
      peerCount <- liftIO $ runSqlPool (count [M.NetworkPeerNodeId ==. key]) pool
      let total = vmCount + netCount + diskCount + peerCount
      if total > 0
        then do
          let msg =
                "Node '"
                  <> nodeName node
                  <> "' is still referenced: "
                  <> T.pack (show vmCount)
                  <> " VM(s), "
                  <> T.pack (show netCount)
                  <> " network(s), "
                  <> T.pack (show diskCount)
                  <> " disk placement(s), "
                  <> T.pack (show peerCount)
                  <> " peer membership(s). Remove them first."
          logWarnN msg
          pure $ RespNodeInUse msg
        else do
          -- Cancel the per-node reconnect supervisor before
          -- dropping the registry entry — otherwise the
          -- supervisor's next 'modifyTVar' on 'ssAgents' would
          -- silently re-install an empty 'NodeConns' for a node
          -- that's already gone from the DB.
          liftIO $ cancelNodeSupervisor state key
          liftIO $ runSqlPool (delete key) pool
          logInfoN $ "Deleted node: " <> nodeName node
          pure RespNodeDeleted

--------------------------------------------------------------------------------
-- handleNodeEdit
--------------------------------------------------------------------------------

-- | Mutate selected node fields. 'Nothing' for any argument
-- leaves the corresponding column unchanged.
handleNodeEdit
  :: ServerState
  -> Int64
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> Maybe (Maybe Text)
  -- ^ description: 'Nothing' = leave unchanged; 'Just Nothing' = clear; 'Just (Just t)' = set.
  -> Maybe Bool
  -- ^ netd-disabled: 'Nothing' = leave unchanged.
  -> IO Response
handleNodeEdit state nodeId mName mHost mNodeAgentPort mNetAgentPort mBasePath mDesc mNetdDisabled = runServerLogging state $ do
  let key = toSqlKey nodeId :: M.NodeId
      pool = ssDbPool state
  mNode <- liftIO $ runSqlPool (get key) pool
  case mNode of
    Nothing -> pure RespNodeNotFound
    Just node -> do
      let flippingToDisabled =
            mNetdDisabled == Just True && not (M.nodeNetdDisabled node)
      blockReason <-
        if flippingToDisabled
          then liftIO $ checkNetdDisableAllowed pool key
          else pure Nothing
      case blockReason of
        Just msg -> do
          logWarnN msg
          pure (RespError msg)
        Nothing -> do
          let updates =
                maybe [] (\v -> [M.NodeName =. v]) mName
                  ++ maybe [] (\v -> [M.NodeHost =. v]) mHost
                  ++ maybe [] (\v -> [M.NodeNodeAgentPort =. v]) mNodeAgentPort
                  ++ maybe [] (\v -> [M.NodeNetAgentPort =. v]) mNetAgentPort
                  ++ maybe [] (\v -> [M.NodeBasePath =. v]) mBasePath
                  ++ maybe [] (\v -> [M.NodeDescription =. v]) mDesc
                  ++ maybe [] (\v -> [M.NodeNetdDisabled =. v]) mNetdDisabled
          case updates of
            [] -> pure RespNodeEdited
            us -> do
              liftIO $ runSqlPool (update key us) pool
              -- A host/port change moves the agent endpoint, and
              -- flipping netdDisabled changes which child loops
              -- the supervisor needs to fork. Either way, cancel
              -- and respawn against the fresh row so the daemon
              -- picks up the new shape immediately. Other fields
              -- (name, description, admin-state, basePath) leave
              -- the connection alone.
              let supervisorMoved =
                    Data.Maybe.isJust mHost
                      || Data.Maybe.isJust mNodeAgentPort
                      || Data.Maybe.isJust mNetAgentPort
                      || Data.Maybe.isJust mNetdDisabled
              Control.Monad.when (supervisorMoved && nodeAdminState node /= M.NodeOffline) $ do
                liftIO $ cancelNodeSupervisor state key
                mNodeRefreshed <- liftIO $ runSqlPool (get key) pool
                case mNodeRefreshed of
                  Just refreshed ->
                    liftIO $
                      Control.Monad.void $
                        spawnNodeSupervisor state (Entity key refreshed)
                  Nothing -> pure ()
              pure RespNodeEdited

--------------------------------------------------------------------------------
-- handleNodeSetState
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Toggle-block check
--------------------------------------------------------------------------------

-- | Pre-condition for flipping 'NodeNetdDisabled' from 'False' to
-- 'True'. Refuses while the node still owns managed networks or
-- has VMs with netd-dependent NIC types attached, because those
-- resources stop working the moment netd is disabled. Returns a
-- caller-displayable error message, or 'Nothing' if the flip is
-- safe.
checkNetdDisableAllowed
  :: Database.Persist.Sql.ConnectionPool
  -> M.NodeId
  -> IO (Maybe Text)
checkNetdDisableAllowed pool nid = do
  nets <-
    runSqlPool
      (selectList [M.NetworkNodeId ==. nid] [Asc M.NetworkName, LimitTo 5])
      pool
  if not (null nets)
    then
      pure $
        Just $
          "Cannot disable netd: node still owns "
            <> T.pack (show (length nets))
            <> "+ managed network(s): "
            <> T.intercalate ", " (map (M.networkName . entityVal) nets)
            <> ". Delete them first."
    else do
      vmsHere <- runSqlPool (selectList [M.VmNodeId ==. nid] []) pool
      let vmKeys = map entityKey vmsHere
      offendingNics <-
        runSqlPool
          ( selectList
              [ M.NetworkInterfaceVmId <-. vmKeys
              , M.NetworkInterfaceInterfaceType
                  <-. [M.NetManaged, M.NetTap, M.NetBridge, M.NetMacvtap]
              ]
              [LimitTo 5]
          )
          pool
      if not (null offendingNics)
        then
          pure $
            Just $
              "Cannot disable netd: node has "
                <> T.pack (show (length offendingNics))
                <> "+ VM network interface(s) of netd-dependent type "
                <> "(managed/tap/bridge/macvtap). Detach them first."
        else pure Nothing

--------------------------------------------------------------------------------
-- handleNodeList
--------------------------------------------------------------------------------

handleNodeList :: ServerState -> IO Response
handleNodeList state = do
  nodes <- runSqlPool (selectList [] [Asc M.NodeName]) (ssDbPool state)
  agents <- readTVarIO (ssAgents state)
  let connected k = case Map.lookup k agents of
        Just nc -> Data.Maybe.isJust (ncNetAgent nc)
        Nothing -> False
  pure $ RespNodeList $ map (\e -> toNodeInfo (connected (entityKey e)) e) nodes

handleNodeShow :: ServerState -> Int64 -> IO Response
handleNodeShow state nodeId = do
  let key = toSqlKey nodeId :: M.NodeId
  mNode <- runSqlPool (get key) (ssDbPool state)
  case mNode of
    Nothing -> pure RespNodeNotFound
    Just node -> do
      agents <- readTVarIO (ssAgents state)
      let connected = case Map.lookup key agents of
            Just nc -> Data.Maybe.isJust (ncNetAgent nc)
            Nothing -> False
      pure $ RespNodeDetails $ toNodeDetails nodeId node connected

--------------------------------------------------------------------------------
-- Supervisor lifecycle
--------------------------------------------------------------------------------

-- | Cancel the per-node reconnect supervisor and drop the
-- registry entry. Called by 'handleNodeDelete' and (with a
-- fresh respawn) by 'handleNodeEdit' on host/port change. A
-- missing registry entry is a no-op — the row may pre-date the
-- daemon boot if a follow-up patch ever lets handlers run
-- before 'spawnAllNodeSupervisors'.
cancelNodeSupervisor :: ServerState -> M.NodeId -> IO ()
cancelNodeSupervisor state nid = do
  m <- readTVarIO (ssAgents state)
  case Map.lookup nid m of
    Just nc -> mapM_ cancel (ncSupervisor nc)
    Nothing -> pure ()
  removeNodeConns state nid

--------------------------------------------------------------------------------
-- DTO converters
--------------------------------------------------------------------------------

toNodeInfo :: Bool -> Entity Node -> NodeInfo
toNodeInfo netdConnected (Entity key node) =
  NodeInfo
    { noiId = fromSqlKey key
    , noiName = nodeName node
    , noiHost = nodeHost node
    , noiNodeAgentPort = nodeNodeAgentPort node
    , noiNetAgentPort = nodeNetAgentPort node
    , noiAdminState = nodeAdminState node
    , noiCreatedAt = nodeCreatedAt node
    , noiCpuCount = nodeCpuCount node
    , noiRamTotal = nodeRamTotal node
    , noiRamFree = nodeRamFree node
    , noiStorageBytesTotal = nodeStorageBytesTotal node
    , noiStorageBytesFree = nodeStorageBytesFree node
    , noiLoadAvg1 = nodeLoadAvg1 node
    , noiLastNodeAgentPushAt = nodeNodeAgentHealthcheck node
    , noiLastNetAgentPushAt = nodeNetAgentHealthcheck node
    , noiNetdDisabled = nodeNetdDisabled node
    , -- A disabled node never has a live netd cap (the supervisor
      -- skips the loop); clamp the derived flag to false so a stray
      -- registry entry can't show "online" against an operator's
      -- explicit decision.
      noiNetdConnected = not (nodeNetdDisabled node) && netdConnected
    }

toNodeDetails :: Int64 -> Node -> Bool -> NodeDetails
toNodeDetails nodeId node netdConnected =
  NodeDetails
    { nodId = nodeId
    , nodName = nodeName node
    , nodHost = nodeHost node
    , nodNodeAgentPort = nodeNodeAgentPort node
    , nodNetAgentPort = nodeNetAgentPort node
    , nodBasePath = nodeBasePath node
    , nodDescription = nodeDescription node
    , nodAdminState = nodeAdminState node
    , nodCreatedAt = nodeCreatedAt node
    , nodCpuCount = nodeCpuCount node
    , nodRamTotal = nodeRamTotal node
    , nodRamFree = nodeRamFree node
    , nodStorageBytesTotal = nodeStorageBytesTotal node
    , nodStorageBytesFree = nodeStorageBytesFree node
    , nodLoadAvg1 = nodeLoadAvg1 node
    , nodLoadAvg5 = nodeLoadAvg5 node
    , nodLoadAvg15 = nodeLoadAvg15 node
    , nodKernelRelease = nodeKernelRelease node
    , nodAgentVersion = nodeAgentVersion node
    , nodLastNodeAgentPushAt = nodeNodeAgentHealthcheck node
    , nodLastNetAgentPushAt = nodeNetAgentHealthcheck node
    , nodNetdDisabled = nodeNetdDisabled node
    , nodNetdConnected = not (nodeNetdDisabled node) && netdConnected
    }

--------------------------------------------------------------------------------
-- Action types
--------------------------------------------------------------------------------

data NodeAdd = NodeAdd
  { naName :: !Text
  , naHost :: !Text
  , naNodeAgentPort :: !Int
  , naNetAgentPort :: !Int
  , naBasePath :: !Text
  , naDescription :: !(Maybe Text)
  , naAdminState :: !M.NodeAdminState
  , naNetdDisabled :: !Bool
  }

instance Action NodeAdd where
  actionResources _ a = pure [(NodeEndpoint (naHost a) (naNodeAgentPort a), ExclusiveAccess)]
  actionSubsystem _ = M.SubNode
  actionCommand _ = "add"
  actionEntityName = Just . naName
  actionExecute ctx a =
    handleNodeAdd
      (acState ctx)
      (naName a)
      (naHost a)
      (naNodeAgentPort a)
      (naNetAgentPort a)
      (naBasePath a)
      (naDescription a)
      (naAdminState a)
      (naNetdDisabled a)

newtype NodeDelete = NodeDelete {ndelNodeId :: Int64}

instance Action NodeDelete where
  actionSubsystem _ = M.SubNode
  actionCommand _ = "delete"
  actionEntityId = Just . fromIntegral . ndelNodeId
  actionExecute ctx a = handleNodeDelete (acState ctx) (ndelNodeId a)

data NodeEdit = NodeEdit
  { nedNodeId :: !Int64
  , nedName :: !(Maybe Text)
  , nedHost :: !(Maybe Text)
  , nedNodeAgentPort :: !(Maybe Int)
  , nedNetAgentPort :: !(Maybe Int)
  , nedBasePath :: !(Maybe Text)
  , nedDescription :: !(Maybe (Maybe Text))
  -- ^ 'Nothing' = leave unchanged; 'Just Nothing' = clear; 'Just (Just t)' = set.
  , nedNetdDisabled :: !(Maybe Bool)
  }

instance Action NodeEdit where
  actionResources state a = do
    node <- runSqlPool (get (toSqlKey (nedNodeId a) :: M.NodeId)) (ssDbPool state)
    pure $ case node of
      Nothing -> []
      Just row -> [(ObjectName NodeObject Nothing (Data.Maybe.fromMaybe (nodeName row) (nedName a)), ExclusiveAccess), (NodeEndpoint (Data.Maybe.fromMaybe (nodeHost row) (nedHost a)) (Data.Maybe.fromMaybe (nodeNodeAgentPort row) (nedNodeAgentPort a)), ExclusiveAccess)]
  actionSubsystem _ = M.SubNode
  actionCommand _ = "edit"
  actionEntityId = Just . fromIntegral . nedNodeId
  actionExecute ctx a = do
    let pool = ssDbPool (acState ctx)
        key = toSqlKey (nedNodeId a) :: M.NodeId
    current <- runSqlPool (get key) pool
    case current of
      Nothing -> pure RespNodeNotFound
      Just row -> do
        let name = Data.Maybe.fromMaybe (nodeName row) (nedName a)
            host = Data.Maybe.fromMaybe (nodeHost row) (nedHost a)
            port = Data.Maybe.fromMaybe (nodeNodeAgentPort row) (nedNodeAgentPort a)
        duplicates <- runSqlPool ((,) <$> getBy (M.UniqueNodeName name) <*> getBy (M.UniqueNodeAddress host port)) pool
        if any (maybe False ((/= key) . entityKey)) [fst duplicates, snd duplicates]
          then pure $ RespError "A node with this name or endpoint already exists"
          else
            handleNodeEdit
              (acState ctx)
              (nedNodeId a)
              (nedName a)
              (nedHost a)
              (nedNodeAgentPort a)
              (nedNetAgentPort a)
              (nedBasePath a)
              (nedDescription a)
              (nedNetdDisabled a)

data NodeSetState = NodeSetState Int64 M.NodeAdminState

instance Action NodeSetState where
  actionSubsystem _ = M.SubNode
  actionCommand _ = "set-state"
  actionEntityId (NodeSetState ident _) = Just (fromIntegral ident)
  actionExecute ctx (NodeSetState ident desired) = mask_ $ do
    let state = acState ctx
        key = toSqlKey ident
        pool = ssDbPool state
    runSqlPool (get key) pool >>= \case
      Nothing -> pure RespNodeNotFound
      Just row -> do
        let previous = nodeAdminState row
        Control.Monad.when (previous == M.NodeOffline && desired /= M.NodeOffline) $
          suspendNodeSupervisor state key
        Control.Monad.when (previous /= desired) $
          runSqlPool (update key [M.NodeAdminState =. desired]) pool
        if desired == M.NodeOffline
          then suspendNodeSupervisor state key
          else
            Control.Monad.when (previous == M.NodeOffline) $
              Control.Monad.void $
                spawnNodeSupervisor state (Entity key row {nodeAdminState = desired})
        pure RespNodeEdited

-- | Keep the first-connect autostart flags for the next resume. Retain
-- the worker handle until cancellation completes so a retry can join it.
suspendNodeSupervisor :: ServerState -> M.NodeId -> IO ()
suspendNodeSupervisor state key = do
  let agents = ssAgents state
  supervisor <- atomically $ do
    current <- readTVar agents
    modifyTVar' agents $ Map.adjust (\nc -> nc {ncNodeAgent = Nothing, ncNetAgent = Nothing}) key
    pure (Map.lookup key current >>= ncSupervisor)
  mapM_ cancel supervisor
  atomically $ modifyTVar' agents $ Map.adjust (\nc -> nc {ncSupervisor = Nothing}) key

-- | Probe only the supervisor-owned session; never allocate a task or resource.
handleNodePing :: ServerState -> Int64 -> IO (Either Response (Bool, Text))
handleNodePing state nodeId = do
  let key = toSqlKey nodeId :: M.NodeId
  found <- runSqlPool (get key) (ssDbPool state)
  case found of
    Nothing -> pure (Left RespNodeNotFound)
    Just row | nodeAdminState row == M.NodeOffline -> pure (Right (False, "Node is administratively offline"))
    Just _ -> do
      agent <- lookupNodeAgent state key
      case agent of
        Left message -> pure (Right (False, message))
        Right nac -> do
          result <- timeout 2000000 (NAC.sessionPing nac)
          pure $ Right $ case result of
            Nothing -> (False, "Nodeagent ping timed out")
            Just (Left err) -> (False, T.pack (show err))
            Just (Right ()) -> (True, "Nodeagent reachable")

data NodeForget = NodeForget Int64 Bool

instance Action NodeForget where
  actionSubsystem _ = M.SubNode
  actionCommand _ = "forget"
  actionEntityId (NodeForget ident _) = Just (fromIntegral ident)
  actionVmDependencies state (NodeForget ident _) =
    runSqlPool (map fromSqlKey <$> selectKeysList [M.VmNodeId ==. toSqlKey ident] []) (ssDbPool state)
  actionResources state (NodeForget ident _) = do
    result <- nodeForgetPreview state ident
    case result of
      Left _ -> pure []
      Right impact -> do
        vms <- concat <$> mapM (objectResources state ExclusiveAccess VmObject . nrId) (nfiVms impact)
        nets <- concat <$> mapM (objectResources state ExclusiveAccess NetworkObject . nrId) (nfiOwnedNetworks impact ++ nfiPeerNetworks impact)
        disks <- concat <$> mapM (imageResources state ExclusiveAccess . nrId) (nfiDiskImages impact)
        pure (vms ++ nets ++ disks)
  actionExecute ctx (NodeForget ident confirmed)
    | confirmed = forgetNode ctx ident
    | otherwise = pure (RespError "Confirm that the host is fenced before forgetting it")

nodeForgetPreview :: ServerState -> Int64 -> IO (Either Response NodeForgetImpact)
nodeForgetPreview state ident = runSqlPool (loadForgetImpact (toSqlKey ident)) (ssDbPool state)

loadForgetImpact :: M.NodeId -> SqlPersistT IO (Either Response NodeForgetImpact)
loadForgetImpact key = do
  node <- get key
  case node of
    Nothing -> pure (Left RespNodeNotFound)
    Just _ -> do
      vms <- selectList [M.VmNodeId ==. key] [Asc M.VmId]
      owned <- selectList [M.NetworkNodeId ==. key] [Asc M.NetworkId]
      memberships <- selectList [M.NetworkPeerNodeId ==. key] [Asc M.NetworkPeerNetworkId]
      peers <- selectList [M.NetworkId <-. map (M.networkPeerNetworkId . entityVal) memberships] [Asc M.NetworkId]
      placements <- selectList [M.DiskImageNodeNodeId ==. key] [Asc M.DiskImageNodeDiskImageId]
      let imageIds = map (M.diskImageNodeDiskImageId . entityVal) placements
      images <- selectList [M.DiskImageId <-. imageIds] [Asc M.DiskImageId]
      replicas <- selectList [M.DiskImageNodeDiskImageId <-. imageIds, M.DiskImageNodeNodeId !=. key] []
      nics <- selectList [M.NetworkInterfaceNetworkId <-. map (Just . entityKey) owned] []
      blockers <- selectList [M.VmId <-. map (M.networkInterfaceVmId . entityVal) nics, M.VmNodeId !=. key] [Asc M.VmId]
      let vmRef (Entity k vm) = NamedRef (fromSqlKey k) (M.vmName vm)
          networkRef (Entity k network) = NamedRef (fromSqlKey k) (M.networkName network)
          imageRef (Entity k image) = NamedRef (fromSqlKey k) (M.diskImageName image)
          replicated = Map.fromList [(M.diskImageNodeDiskImageId row, ()) | Entity _ row <- replicas]
      pure $
        Right $
          NodeForgetImpact
            (map vmRef vms)
            (map networkRef owned)
            (map networkRef peers)
            (map imageRef images)
            (map imageRef (filter (\row -> Map.notMember (entityKey row) replicated) images))
            (map vmRef blockers)

forgetNode :: ActionContext -> Int64 -> IO Response
forgetNode ctx ident = do
  planned <- nodeForgetPreview state ident
  case planned of
    Left err -> pure err
    Right impact
      | not (null (nfiBlockingVms impact)) ->
          pure $
            RespNodeInUse $
              "Networks owned by this node: "
                <> names (nfiOwnedNetworks impact)
                <> ". Still used by surviving VMs: "
                <> names (nfiBlockingVms impact)
      | otherwise -> do
          connected <- nodeConnected
          if connected
            then pure (RespError "An agent session is still connected to this node")
            else do
              failures <- concat <$> mapM teardownPeers (nfiOwnedNetworks impact)
              if null failures
                then commitForget impact
                else pure (RespError (T.intercalate "\n" failures))
  where
    state = acState ctx
    key = toSqlKey ident
    pool = ssDbPool state
    names = T.intercalate ", " . map nrName

    nodeConnected = do
      conns <- Map.lookup key <$> readTVarIO (ssAgents state)
      pure $ maybe False (\c -> Data.Maybe.isJust (ncNodeAgent c) || Data.Maybe.isJust (ncNetAgent c)) conns

    teardownPeers ref = do
      peers <- runSqlPool (selectList [M.NetworkPeerNetworkId ==. toSqlKey (nrId ref), M.NetworkPeerNodeId !=. key] []) pool
      fmap concat $ Control.Monad.forM peers $ \(Entity _ peer) -> do
        throwIfCancelled ctx
        let peerId = M.networkPeerNodeId peer
        result <- timeout 10000000 $ withNetAgent state peerId $ \agent ->
          Net.deleteNetwork agent (corvusBridgeName (nrId ref))
        case result of
          Just (Right (Right ())) -> pure []
          _ -> do
            label <- runSqlPool (get peerId) pool
            pure ["Failed to tear down network " <> nrName ref <> " on peer " <> maybe (T.pack (show (fromSqlKey peerId))) nodeName label]

    commitForget impact = do
      committed <-
        runSqlPool
          ( do
              checkTaskCommit state
              current <- loadForgetImpact key
              connected <- liftIO nodeConnected
              case current of
                Right fresh | fresh == impact && not connected -> do
                  mapM_ (deleteVm . nrId) (nfiVms impact)
                  deleteWhere [M.DiskImageNodeNodeId ==. key]
                  deleteWhere [M.NetworkPeerNodeId ==. key]
                  Control.Monad.forM_ (nfiOwnedNetworks impact) $ \ref -> do
                    let networkKey = toSqlKey (nrId ref)
                    deleteWhere [M.NetworkPeerNetworkId ==. networkKey]
                    delete networkKey
                  delete key
                  pure True
                _ -> pure False
          )
          pool
      if committed
        then do
          cancelNodeSupervisor state key
          warnings <- concat <$> mapM refreshNetwork (nfiPeerNetworks impact)
          pure (RespNodeForgotten impact warnings)
        else pure (RespError "Node connectivity or dependencies changed during forget. Review and retry")

    refreshNetwork ref = do
      let networkKey = toSqlKey (nrId ref)
      network <- runSqlPool (get networkKey) pool
      if maybe False M.networkRunning network
        then do
          warnings <- newIORef []
          let recordWarning err = modifyIORef' warnings (("Failed to refresh network " <> nrName ref <> ": " <> err) :)
          result <- timeout 10000000 (pushNetworkToAllMembersWithWarnings state recordWarning networkKey)
          peerWarnings <- reverse <$> readIORef warnings
          pure $
            peerWarnings <> case result of
              Just (Right ()) -> []
              Just (Left err) -> ["Failed to refresh network " <> nrName ref <> ": " <> err]
              Nothing -> ["Timed out refreshing network " <> nrName ref]
        else pure []
