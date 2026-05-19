{-# LANGUAGE OverloadedStrings #-}

-- | DB-backed helpers for resolving the 'NodeId' a given VM /
-- network / disk should dispatch its agent calls through, and
-- thin wrappers that combine the resolution with the matching
-- 'withNodeAgent' / 'withNetAgent' lookups.
--
-- The functions here are the slice-1c bridge between handlers
-- (which know "I want to drive VM @eid@" / "I want to delete
-- network @nid@") and the per-node registry in 'Corvus.Types'.
-- Phase 3 will extend the disk routing with proper
-- 'DiskImageNode' lookups.
module Corvus.NodeRouting
  ( -- * Resolving NodeIds from related rows
    vmNodeIdById
  , networkNodeIdById

    -- * Dispatch wrappers
  , withVmNodeAgent
  , withVmNetAgent
  , withNetworkNetAgent
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Corvus.Model as M
import qualified Corvus.NetAgentClient as NA
import qualified Corvus.NodeAgentClient as NOA
import Corvus.Types
  ( ServerState
  , ssDbPool
  , withNetAgent
  , withNodeAgent
  )
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist (get)
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (toSqlKey)

-- | Look up a VM's 'NodeId'. The eid is the 'Int64' carried on
-- the wire; we convert it via 'toSqlKey'.
vmNodeIdById :: (MonadIO m) => ServerState -> Int64 -> m (Either Text M.NodeId)
vmNodeIdById state eid = do
  mVm <- liftIO $ runSqlPool (get (toSqlKey eid :: M.VmId)) (ssDbPool state)
  pure $ case mVm of
    Nothing -> Left "VM not found"
    Just vm -> Right (M.vmNodeId vm)

-- | Look up a Network's 'NodeId'.
networkNodeIdById :: (MonadIO m) => ServerState -> Int64 -> m (Either Text M.NodeId)
networkNodeIdById state nid = do
  mNw <- liftIO $ runSqlPool (get (toSqlKey nid :: M.NetworkId)) (ssDbPool state)
  pure $ case mNw of
    Nothing -> Left "Network not found"
    Just nw -> Right (M.networkNodeId nw)

-- | Resolve VM's node and run an action through its nodeagent.
withVmNodeAgent
  :: ServerState
  -> Int64
  -- ^ VM entity id
  -> (NOA.NodeAgentClient -> IO a)
  -> IO (Either Text a)
withVmNodeAgent state eid action = do
  r <- vmNodeIdById state eid
  case r of
    Left e -> pure (Left e)
    Right nid -> withNodeAgent state nid action

-- | Resolve VM's node and run an action through its netd cap.
withVmNetAgent
  :: ServerState
  -> Int64
  -> (NA.NetAgentClient -> IO a)
  -> IO (Either Text a)
withVmNetAgent state eid action = do
  r <- vmNodeIdById state eid
  case r of
    Left e -> pure (Left e)
    Right nid -> withNetAgent state nid action

-- | Resolve Network's node and run an action through its netd cap.
withNetworkNetAgent
  :: ServerState
  -> Int64
  -> (NA.NetAgentClient -> IO a)
  -> IO (Either Text a)
withNetworkNetAgent state nid action = do
  r <- networkNodeIdById state nid
  case r of
    Left e -> pure (Left e)
    Right node -> withNetAgent state node action
