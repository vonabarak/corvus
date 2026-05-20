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
  , NodeEdit (..)
  , NodeDrain (..)

    -- * Handlers
  , handleNodeAdd
  , handleNodeDelete
  , handleNodeEdit
  , handleNodeDrain
  , handleNodeList
  , handleNodeShow
  )
where

import Control.Concurrent.Async (cancel)
import Control.Concurrent.STM (readTVarIO)
import qualified Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN)
import Corvus.Action
import Corvus.Handlers.Resolve (validateName)
import Corvus.Model (Node (..))
import qualified Corvus.Model as M
import Corvus.NodeSupervisor (spawnNodeSupervisor)
import Corvus.Protocol
import Corvus.Types
  ( NodeConns (..)
  , ServerState (..)
  , removeNodeConns
  , runServerLogging
  )
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (fromSqlKey, toSqlKey)

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
  -> IO Response
handleNodeAdd state name host nodeAgentPort netAgentPort basePath mDesc adminState =
  case validateName "Node" name of
    Left err -> pure $ RespError err
    Right () -> runServerLogging state $ do
      logInfoN $ "Adding node: " <> name <> " (" <> host <> ":" <> T.pack (show nodeAgentPort) <> ")"
      now <- liftIO getCurrentTime
      let node =
            Node
              { nodeName = name
              , nodeHost = host
              , nodeNodeAgentPort = nodeAgentPort
              , nodeNetAgentPort = netAgentPort
              , nodeBasePath = basePath
              , nodeDescription = mDesc
              , nodeAdminState = adminState
              , nodeCreatedAt = now
              , nodeCpuCount = Nothing
              , nodeRamMbTotal = Nothing
              , nodeRamMbFree = Nothing
              , nodeStorageBytesTotal = Nothing
              , nodeStorageBytesFree = Nothing
              , nodeLoadAvg1 = Nothing
              , nodeLoadAvg5 = Nothing
              , nodeLoadAvg15 = Nothing
              , nodeKernelRelease = Nothing
              , nodeAgentVersion = Nothing
              , nodeNodeAgentHealthcheck = Nothing
              , nodeNetAgentHealthcheck = Nothing
              }
      result <- liftIO $ runSqlPool (insertUnique node) (ssDbPool state)
      case result of
        Just key -> do
          -- Fork the per-node reconnect supervisor right away so
          -- the operator doesn't need to restart the daemon
          -- before the new node starts accepting workloads.
          _ <- liftIO $ spawnNodeSupervisor state (Entity key node)
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
      let total = vmCount + netCount + diskCount
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
                  <> " disk placement(s). Delete them first."
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
  -> Maybe M.NodeAdminState
  -> IO Response
handleNodeEdit state nodeId mName mHost mNodeAgentPort mNetAgentPort mBasePath mDesc mAdminState = runServerLogging state $ do
  let key = toSqlKey nodeId :: M.NodeId
      pool = ssDbPool state
  mNode <- liftIO $ runSqlPool (get key) pool
  case mNode of
    Nothing -> pure RespNodeNotFound
    Just _ -> do
      let updates =
            maybe [] (\v -> [M.NodeName =. v]) mName
              ++ maybe [] (\v -> [M.NodeHost =. v]) mHost
              ++ maybe [] (\v -> [M.NodeNodeAgentPort =. v]) mNodeAgentPort
              ++ maybe [] (\v -> [M.NodeNetAgentPort =. v]) mNetAgentPort
              ++ maybe [] (\v -> [M.NodeBasePath =. v]) mBasePath
              ++ maybe [] (\v -> [M.NodeDescription =. v]) mDesc
              ++ maybe [] (\v -> [M.NodeAdminState =. v]) mAdminState
      case updates of
        [] -> pure RespNodeEdited
        us -> do
          liftIO $ runSqlPool (update key us) pool
          -- A host or port change moves the agent endpoint, so
          -- the existing supervisor's dial loop targets the
          -- old address. Cancel it and respawn against the
          -- fresh row so the daemon picks up the new endpoint
          -- immediately. Other fields (name, description,
          -- admin-state, basePath) leave the connection alone.
          let connectionMoved =
                Data.Maybe.isJust mHost
                  || Data.Maybe.isJust mNodeAgentPort
                  || Data.Maybe.isJust mNetAgentPort
          Control.Monad.when connectionMoved $ do
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
-- handleNodeDrain
--------------------------------------------------------------------------------

-- | Operator shortcut for marking a node as draining; subsequent
-- scheduler picks (Phase 2) skip it.
handleNodeDrain :: ServerState -> Int64 -> IO Response
handleNodeDrain state nodeId =
  handleNodeEdit
    state
    nodeId
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    (Just M.NodeDraining)

--------------------------------------------------------------------------------
-- handleNodeList
--------------------------------------------------------------------------------

handleNodeList :: ServerState -> IO Response
handleNodeList state = do
  nodes <- runSqlPool (selectList [] [Asc M.NodeName]) (ssDbPool state)
  pure $ RespNodeList $ map toNodeInfo nodes

handleNodeShow :: ServerState -> Int64 -> IO Response
handleNodeShow state nodeId = do
  let key = toSqlKey nodeId :: M.NodeId
  mNode <- runSqlPool (get key) (ssDbPool state)
  case mNode of
    Nothing -> pure RespNodeNotFound
    Just node -> pure $ RespNodeDetails $ toNodeDetails nodeId node

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
    Just nc -> cancel (ncSupervisor nc)
    Nothing -> pure ()
  removeNodeConns state nid

--------------------------------------------------------------------------------
-- DTO converters
--------------------------------------------------------------------------------

toNodeInfo :: Entity Node -> NodeInfo
toNodeInfo (Entity key node) =
  NodeInfo
    { noiId = fromSqlKey key
    , noiName = nodeName node
    , noiHost = nodeHost node
    , noiNodeAgentPort = nodeNodeAgentPort node
    , noiNetAgentPort = nodeNetAgentPort node
    , noiAdminState = nodeAdminState node
    , noiCreatedAt = nodeCreatedAt node
    , noiCpuCount = nodeCpuCount node
    , noiRamMbTotal = nodeRamMbTotal node
    , noiRamMbFree = nodeRamMbFree node
    , noiStorageBytesTotal = nodeStorageBytesTotal node
    , noiStorageBytesFree = nodeStorageBytesFree node
    , noiLoadAvg1 = nodeLoadAvg1 node
    , noiLastNodeAgentPushAt = nodeNodeAgentHealthcheck node
    , noiLastNetAgentPushAt = nodeNetAgentHealthcheck node
    }

toNodeDetails :: Int64 -> Node -> NodeDetails
toNodeDetails nodeId node =
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
    , nodRamMbTotal = nodeRamMbTotal node
    , nodRamMbFree = nodeRamMbFree node
    , nodStorageBytesTotal = nodeStorageBytesTotal node
    , nodStorageBytesFree = nodeStorageBytesFree node
    , nodLoadAvg1 = nodeLoadAvg1 node
    , nodLoadAvg5 = nodeLoadAvg5 node
    , nodLoadAvg15 = nodeLoadAvg15 node
    , nodKernelRelease = nodeKernelRelease node
    , nodAgentVersion = nodeAgentVersion node
    , nodLastNodeAgentPushAt = nodeNodeAgentHealthcheck node
    , nodLastNetAgentPushAt = nodeNetAgentHealthcheck node
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
  }

instance Action NodeAdd where
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
  , nedAdminState :: !(Maybe M.NodeAdminState)
  }

instance Action NodeEdit where
  actionSubsystem _ = M.SubNode
  actionCommand _ = "edit"
  actionEntityId = Just . fromIntegral . nedNodeId
  actionExecute ctx a =
    handleNodeEdit
      (acState ctx)
      (nedNodeId a)
      (nedName a)
      (nedHost a)
      (nedNodeAgentPort a)
      (nedNetAgentPort a)
      (nedBasePath a)
      (nedDescription a)
      (nedAdminState a)

newtype NodeDrain = NodeDrain {ndrNodeId :: Int64}

instance Action NodeDrain where
  actionSubsystem _ = M.SubNode
  actionCommand _ = "drain"
  actionEntityId = Just . fromIntegral . ndrNodeId
  actionExecute ctx a = handleNodeDrain (acState ctx) (ndrNodeId a)

-- 'fromMaybe' is re-exported here to keep the import set tight
-- if future edits drop the local use.
_unused :: a -> Maybe a -> a
_unused = fromMaybe
