{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Node-related RPC wrappers extracted from "Corvus.Client.Capnp.Rpc".
module Corvus.Client.Capnp.Rpc.Node
  ( -- * Node read methods
    rpcNodeList
  , rpcNodeShow

    -- * Node lifecycle
  , rpcNodeAdd
  , rpcNodeEdit
  , rpcNodeDrain
  , rpcNodeDelete

    -- * Helpers
  , getNodeClient
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Node as CGNode
import Corvus.Client.Capnp.Connection (CapnpConnection (..))
import Corvus.Model (NodeAdminState (..))
import qualified Corvus.Protocol.Node as PNode
import Corvus.Wire.Common (EntityRef, entityRefFromText, toCapnpEntityRef)
import Corvus.Wire.Enums (toCapnpNodeAdminState)
import Corvus.Wire.Errors (WireError, showWireError)
import qualified Corvus.Wire.Node as WNode
import Data.Function ((&))
import Data.Int (Int32, Int64)
import qualified Data.Maybe
import Data.Text (Text)

-- | Call a method on a cap and return its parsed results struct.
callOn
  :: ( C.IsCap iface
     , C.IsStruct params
     , C.IsStruct results
     , C.Parse params (C.Parsed params)
     , C.Parse results (C.Parsed results)
     )
  => C.Method iface params results
  -> C.Parsed params
  -> C.Client iface
  -> IO (C.Parsed results)
callOn method p client = do
  raw <- (client & C.callP method p) >>= C.waitPipeline
  C.evalLimitT C.defaultLimit (C.parse raw)

failOnWire :: Either WireError a -> IO a
failOnWire (Right a) = pure a
failOnWire (Left e) = fail ("wire decode error: " <> show (showWireError e))

-- ---------------------------------------------------------------------
-- Node read methods
-- ---------------------------------------------------------------------

rpcNodeList :: CapnpConnection -> IO [PNode.NodeInfo]
rpcNodeList conn = do
  CGCorvus.Daemon'nodes'results {CGCorvus.mgr = mgr} <-
    callOn #nodes CGCorvus.Daemon'nodes'params (ccDaemon conn)
  CGNode.NodeManager'list'results {CGNode.nodes = ns} <-
    callOn #list CGNode.NodeManager'list'params mgr
  traverse (failOnWire . WNode.fromCapnpNodeInfo) ns

rpcNodeShow :: CapnpConnection -> EntityRef -> IO PNode.NodeDetails
rpcNodeShow conn ref = do
  nClient <- getNodeClient conn ref
  CGNode.Node'show'results {CGNode.details = det} <-
    callOn #show CGNode.Node'show'params nClient
  failOnWire (WNode.fromCapnpNodeDetails det)

-- ---------------------------------------------------------------------
-- Node lifecycle
-- ---------------------------------------------------------------------

rpcNodeAdd
  :: CapnpConnection
  -> Text
  -> Text
  -> Int
  -> Int
  -> Text
  -> Maybe Text
  -> NodeAdminState
  -> Bool
  -> IO Int64
rpcNodeAdd conn name host nodeAgentPort netAgentPort basePath mDesc adminSt netdDisabled = do
  CGCorvus.Daemon'nodes'results {CGCorvus.mgr = mgr} <-
    callOn #nodes CGCorvus.Daemon'nodes'params (ccDaemon conn)
  let inner =
        CGNode.NodeAddParams
          { CGNode.name = name
          , CGNode.host = host
          , CGNode.nodeAgentPort = fromIntegral nodeAgentPort
          , CGNode.netAgentPort = fromIntegral netAgentPort
          , CGNode.basePath = basePath
          , CGNode.description = Data.Maybe.fromMaybe "" mDesc
          , CGNode.adminState = toCapnpNodeAdminState adminSt
          , CGNode.netdDisabled = netdDisabled
          }
  CGNode.NodeManager'create'results {CGNode.node = nClient} <-
    callOn #create CGNode.NodeManager'create'params {CGNode.params = inner} mgr
  CGNode.Node'show'results {CGNode.details = det} <-
    callOn #show CGNode.Node'show'params nClient
  case det of CGNode.NodeDetails {CGNode.id = nid} -> pure nid

rpcNodeEdit
  :: CapnpConnection
  -> EntityRef
  -> Maybe Text
  -> Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> Maybe (Maybe Text)
  -> Maybe NodeAdminState
  -> Maybe Bool
  -> IO ()
rpcNodeEdit conn ref mName mHost mNodeAgentPort mNetAgentPort mBasePath mDesc mAdminSt mNetdDisabled = do
  nClient <- getNodeClient conn ref
  let (hasDesc, descText) = case mDesc of
        Nothing -> (False, "")
        Just Nothing -> (True, "")
        Just (Just t) -> (True, t)
      p =
        CGNode.NodeEditParams
          { CGNode.hasName = Data.Maybe.isJust mName
          , CGNode.name = Data.Maybe.fromMaybe "" mName
          , CGNode.hasHost = Data.Maybe.isJust mHost
          , CGNode.host = Data.Maybe.fromMaybe "" mHost
          , CGNode.hasNodeAgentPort = Data.Maybe.isJust mNodeAgentPort
          , CGNode.nodeAgentPort = maybe 0 fromIntegral mNodeAgentPort
          , CGNode.hasNetAgentPort = Data.Maybe.isJust mNetAgentPort
          , CGNode.netAgentPort = maybe 0 fromIntegral mNetAgentPort
          , CGNode.hasBasePath = Data.Maybe.isJust mBasePath
          , CGNode.basePath = Data.Maybe.fromMaybe "" mBasePath
          , CGNode.hasDescription = hasDesc
          , CGNode.description = descText
          , CGNode.hasAdminState = Data.Maybe.isJust mAdminSt
          , CGNode.adminState = maybe (toCapnpNodeAdminState NodeOnline) toCapnpNodeAdminState mAdminSt
          , CGNode.hasNetdDisabled = Data.Maybe.isJust mNetdDisabled
          , CGNode.netdDisabled = Data.Maybe.fromMaybe False mNetdDisabled
          }
  _ <- callOn #edit CGNode.Node'edit'params {CGNode.params = p} nClient
  pure ()

rpcNodeDrain :: CapnpConnection -> EntityRef -> IO ()
rpcNodeDrain conn ref = do
  nClient <- getNodeClient conn ref
  _ <- callOn #drain CGNode.Node'drain'params nClient
  pure ()

rpcNodeDelete :: CapnpConnection -> EntityRef -> IO ()
rpcNodeDelete conn ref = do
  nClient <- getNodeClient conn ref
  _ <- callOn #delete CGNode.Node'delete'params nClient
  pure ()

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

getNodeClient :: CapnpConnection -> EntityRef -> IO (C.Client CGNode.Node)
getNodeClient conn ref = do
  CGCorvus.Daemon'nodes'results {CGCorvus.mgr = mgr} <-
    callOn #nodes CGCorvus.Daemon'nodes'params (ccDaemon conn)
  CGNode.NodeManager'get'results {CGNode.node = nClient} <-
    callOn #get CGNode.NodeManager'get'params {CGNode.ref = toCapnpEntityRef ref} mgr
  pure nClient
