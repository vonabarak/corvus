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
import Capnp.Rpc.Server (SomeServer, handleParsed, methodUnimplemented)
import Corvus.Handlers.Network (handleNetworkList, handleNetworkShow)
import Corvus.Handlers.Resolve (resolveNetwork)
import Corvus.Protocol (Response (..))
import Corvus.Rpc.Common (capnpRefToRef, failOnLeft)
import Corvus.Types (ServerState (..))
import Corvus.Wire.Network (toCapnpNetworkInfo)
import Data.Int (Int64)
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

  networkManager'get (NetworkManagerCap st sup) = handleParsed $ \CGNet.NetworkManager'get'params {..} -> do
    ref' <- capnpRefToRef ref
    eid <- failOnLeft =<< resolveNetwork ref' (ssDbPool st)
    client <- export @CGNet.Network sup (NetworkCap st eid)
    pure CGNet.NetworkManager'get'results {CGNet.network = client}

  networkManager'create _ = methodUnimplemented

data NetworkCap = NetworkCap
  { _nwState :: !ServerState
  , _nwId :: !Int64
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

  network'start _ = methodUnimplemented
  network'stop _ = methodUnimplemented
  network'edit _ = methodUnimplemented
  network'delete _ = methodUnimplemented
