{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | NodeManager + Node cap implementations.
module Corvus.Rpc.Node
  ( NodeManagerCap (..)
  , NodeCap (..)
  , newNodeManagerCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Node as CGNode
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer)
import Corvus.Action (runAction)
import Corvus.Handlers.Node
  ( NodeAdd (..)
  , NodeDelete (..)
  , NodeDrain (..)
  , NodeEdit (..)
  , handleNodeList
  , handleNodeShow
  )
import qualified Corvus.Handlers.Resolve as Resolve
import Corvus.Protocol (Response (..))
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft, handleParsed)
import Corvus.Types (ServerState (..))
import Corvus.Wire.Enums (fromCapnpNodeAdminState)
import Corvus.Wire.Errors (WireError)
import Corvus.Wire.Node (toCapnpNodeDetails, toCapnpNodeInfo)
import Data.Int (Int64)
import qualified Data.Text as T
import Supervisors (Supervisor)

data NodeManagerCap = NodeManagerCap
  { nmState :: !ServerState
  , nmSup :: !Supervisor
  }

newNodeManagerCap :: ServerState -> Supervisor -> IO NodeManagerCap
newNodeManagerCap st sup = pure (NodeManagerCap st sup)

instance SomeServer NodeManagerCap

instance CGNode.NodeManager'server_ NodeManagerCap where
  nodeManager'list (NodeManagerCap st _) = handleParsed $ \_ -> do
    resp <- handleNodeList st
    case resp of
      RespNodeList nodes ->
        pure CGNode.NodeManager'list'results {CGNode.nodes = map toCapnpNodeInfo nodes}
      RespError msg -> throwFailed msg
      _ -> throwFailed "nodeManager'list: unexpected response"

  nodeManager'get (NodeManagerCap st sup) =
    handleParsed $ \CGNode.NodeManager'get'params {..} -> do
      ref' <- capnpRefToRef ref
      eid <- failOnLeft =<< Resolve.resolveNode ref' (ssDbPool st)
      client <- export @CGNode.Node sup (NodeCap st eid)
      pure CGNode.NodeManager'get'results {CGNode.node = client}

  nodeManager'create (NodeManagerCap st sup) =
    handleParsed $ \CGNode.NodeManager'create'params {params = CGNode.NodeAddParams {..}} -> do
      adminSt <- failOnEnum (fromCapnpNodeAdminState adminState)
      let act =
            NodeAdd
              { naName = name
              , naHost = host
              , naNodeAgentPort = fromIntegral nodeAgentPort
              , naNetAgentPort = fromIntegral netAgentPort
              , naBasePath = basePath
              , naDescription = if T.null description then Nothing else Just description
              , naAdminState = adminSt
              }
      resp <- runAction st act
      case resp of
        RespNodeCreated nid -> do
          client <- export @CGNode.Node sup (NodeCap st nid)
          pure CGNode.NodeManager'create'results {CGNode.node = client}
        RespError msg -> throwFailed msg
        _ -> throwFailed (T.pack ("nodeManager'create: unexpected response: " <> show resp))

data NodeCap = NodeCap
  { ncState :: !ServerState
  , ncId :: !Int64
  }

instance SomeServer NodeCap

instance CGNode.Node'server_ NodeCap where
  node'show (NodeCap st eid) = handleParsed $ \_ -> do
    resp <- handleNodeShow st eid
    case resp of
      RespNodeDetails det ->
        pure CGNode.Node'show'results {CGNode.details = toCapnpNodeDetails det}
      RespNodeNotFound -> throwFailed "Node not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "node'show: unexpected response"

  node'edit (NodeCap st eid) =
    handleParsed $ \CGNode.Node'edit'params {params = CGNode.NodeEditParams {..}} -> do
      mAdminSt <-
        if hasAdminState
          then Just <$> failOnEnum (fromCapnpNodeAdminState adminState)
          else pure Nothing
      let descUpdate
            | not hasDescription = Nothing
            | T.null description = Just Nothing
            | otherwise = Just (Just description)
          act =
            NodeEdit
              { nedNodeId = eid
              , nedName = if hasName then Just name else Nothing
              , nedHost = if hasHost then Just host else Nothing
              , nedNodeAgentPort = if hasNodeAgentPort then Just (fromIntegral nodeAgentPort) else Nothing
              , nedNetAgentPort = if hasNetAgentPort then Just (fromIntegral netAgentPort) else Nothing
              , nedBasePath = if hasBasePath then Just basePath else Nothing
              , nedDescription = descUpdate
              , nedAdminState = mAdminSt
              }
      resp <- runAction st act
      case resp of
        RespNodeEdited -> pure CGNode.Node'edit'results
        RespNodeNotFound -> throwFailed "Node not found"
        RespError msg -> throwFailed msg
        _ -> throwFailed "node'edit: unexpected response"

  node'drain (NodeCap st eid) = handleParsed $ \_ -> do
    resp <- runAction st (NodeDrain eid)
    case resp of
      RespNodeEdited -> pure CGNode.Node'drain'results
      RespNodeNotFound -> throwFailed "Node not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "node'drain: unexpected response"

  node'delete (NodeCap st eid) = handleParsed $ \_ -> do
    resp <- runAction st (NodeDelete eid)
    case resp of
      RespNodeDeleted -> pure CGNode.Node'delete'results
      RespNodeNotFound -> throwFailed "Node not found"
      RespNodeInUse msg -> throwFailed msg
      RespError msg -> throwFailed msg
      _ -> throwFailed "node'delete: unexpected response"

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

failOnEnum :: Either WireError a -> IO a
failOnEnum (Right x) = pure x
failOnEnum (Left e) = throwFailed (T.pack (show e))
