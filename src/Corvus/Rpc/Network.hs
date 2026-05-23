{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | NetworkManager + Network cap implementations.
module Corvus.Rpc.Network
  ( NetworkManagerCap (..)
  , NetworkCap (..)
  , newNetworkManagerCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Network as CGNet
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer)
import Corvus.Action (runAction)
import Corvus.Handlers.Network
  ( NetworkAttachNode (..)
  , NetworkCreate (..)
  , NetworkDelete (..)
  , NetworkDetachNode (..)
  , NetworkEdit (..)
  , NetworkStart (..)
  , NetworkStop (..)
  , handleNetworkList
  , handleNetworkShow
  )
import Corvus.Handlers.Resolve (resolveNetwork)
import Corvus.Protocol (Response (..))
import qualified Corvus.Protocol as P
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft, handleParsed)
import Corvus.Types (ServerState (..))
import Corvus.Wire.Network (toCapnpNetworkInfo)
import Data.Int (Int64)
import qualified Data.Text as T
import Supervisors (Supervisor)

data NetworkManagerCap = NetworkManagerCap
  { nmState :: !ServerState
  , nmSup :: !Supervisor
  }

newNetworkManagerCap :: ServerState -> Supervisor -> IO NetworkManagerCap
newNetworkManagerCap st sup = pure (NetworkManagerCap st sup)

instance SomeServer NetworkManagerCap

instance CGNet.NetworkManager'server_ NetworkManagerCap where
  networkManager'list (NetworkManagerCap st _) = handleParsed $ \_ -> do
    resp <- handleNetworkList st
    case resp of
      RespNetworkList nets ->
        pure CGNet.NetworkManager'list'results {CGNet.networks = map toCapnpNetworkInfo nets}
      RespError msg -> throwFailed msg
      _ -> throwFailed "networkManager'list: unexpected response"

  networkManager'get (NetworkManagerCap st sup) =
    handleParsed $ \CGNet.NetworkManager'get'params {..} -> do
      ref' <- capnpRefToRef ref
      eid <- failOnLeft =<< resolveNetwork ref' (ssDbPool st)
      client <- export @CGNet.Network sup (NetworkCap st eid)
      pure CGNet.NetworkManager'get'results {CGNet.network = client}

  networkManager'create (NetworkManagerCap st sup) =
    handleParsed $ \CGNet.NetworkManager'create'params {params = CGNet.NetworkCreateParams {..}} -> do
      nodeRef' <- capnpRefToRef node
      let act =
            NetworkCreate
              { ncrName = name
              , ncrNodeRef = P.unRef nodeRef'
              , ncrSubnet = subnet
              , ncrDhcp = dhcp
              , ncrNat = nat
              , ncrAutostart = autostart
              }
      resp <- runAction st act
      case resp of
        RespNetworkCreated nid -> do
          client <- export @CGNet.Network sup (NetworkCap st nid)
          pure CGNet.NetworkManager'create'results {CGNet.network = client}
        RespError msg -> throwFailed msg
        _ -> throwFailed (T.pack ("networkManager'create: unexpected response: " <> show resp))

data NetworkCap = NetworkCap
  { nwState :: !ServerState
  , nwId :: !Int64
  }

instance SomeServer NetworkCap

instance CGNet.Network'server_ NetworkCap where
  network'show (NetworkCap st eid) = handleParsed $ \_ -> do
    resp <- handleNetworkShow st eid
    case resp of
      RespNetworkDetails info ->
        pure CGNet.Network'show'results {CGNet.info = toCapnpNetworkInfo info}
      RespNetworkNotFound -> throwFailed "Network not found"
      RespError msg -> throwFailed msg
      _ -> throwFailed "network'show: unexpected response"

  network'start (NetworkCap st eid) = handleParsed $ \_ -> do
    resp <- runAction st (NetworkStart eid)
    case resp of
      RespNetworkStarted -> pure CGNet.Network'start'results
      RespNetworkNotFound -> throwFailed "Network not found"
      RespNetworkAlreadyRunning -> throwFailed "Network already running"
      RespNetworkError msg -> throwFailed msg
      RespError msg -> throwFailed msg
      _ -> throwFailed "network'start: unexpected response"

  network'stop (NetworkCap st eid) = handleParsed $ \CGNet.Network'stop'params {..} -> do
    resp <- runAction st (NetworkStop {nstopNetworkId = eid, nstopForce = force})
    case resp of
      RespNetworkStopped -> pure CGNet.Network'stop'results
      RespNetworkNotFound -> throwFailed "Network not found"
      RespNetworkNotRunning -> throwFailed "Network not running"
      RespNetworkInUse -> throwFailed "Network in use"
      RespNetworkError msg -> throwFailed msg
      RespError msg -> throwFailed msg
      _ -> throwFailed "network'stop: unexpected response"

  network'edit (NetworkCap st eid) =
    handleParsed $ \CGNet.Network'edit'params {params = CGNet.NetworkEditParams {..}} -> do
      let act =
            NetworkEdit
              { nedNetworkId = eid
              , nedSubnet = if hasSubnet then Just subnet else Nothing
              , nedDhcp = if hasDhcp then Just dhcp else Nothing
              , nedNat = if hasNat then Just nat else Nothing
              , nedAutostart = if hasAutostart then Just autostart else Nothing
              }
      resp <- runAction st act
      case resp of
        RespNetworkEdited -> pure CGNet.Network'edit'results
        RespNetworkNotFound -> throwFailed "Network not found"
        RespError msg -> throwFailed msg
        _ -> throwFailed "network'edit: unexpected response"

  network'delete (NetworkCap st eid) = handleParsed $ \_ -> do
    resp <- runAction st (NetworkDelete eid)
    case resp of
      RespNetworkDeleted -> pure CGNet.Network'delete'results
      RespNetworkNotFound -> throwFailed "Network not found"
      RespNetworkInUse -> throwFailed "Network in use"
      RespError msg -> throwFailed msg
      _ -> throwFailed "network'delete: unexpected response"

  network'attachNode (NetworkCap st eid) =
    handleParsed $ \CGNet.Network'attachNode'params {params = CGNet.NetworkPeerParams {..}} -> do
      nodeRef' <- capnpRefToRef node
      resp <-
        runAction st $
          NetworkAttachNode {nanNetworkId = eid, nanNodeRef = P.unRef nodeRef'}
      case resp of
        RespNetworkPeerAttached -> pure CGNet.Network'attachNode'results
        RespNetworkNotFound -> throwFailed "Network not found"
        RespNetworkError msg -> throwFailed msg
        RespError msg -> throwFailed msg
        _ -> throwFailed "network'attachNode: unexpected response"

  network'detachNode (NetworkCap st eid) =
    handleParsed $ \CGNet.Network'detachNode'params {params = CGNet.NetworkPeerParams {..}} -> do
      nodeRef' <- capnpRefToRef node
      resp <-
        runAction st $
          NetworkDetachNode {ndnNetworkId = eid, ndnNodeRef = P.unRef nodeRef'}
      case resp of
        RespNetworkPeerDetached -> pure CGNet.Network'detachNode'results
        RespNetworkNotFound -> throwFailed "Network not found"
        RespNetworkError msg -> throwFailed msg
        RespError msg -> throwFailed msg
        _ -> throwFailed "network'detachNode: unexpected response"
