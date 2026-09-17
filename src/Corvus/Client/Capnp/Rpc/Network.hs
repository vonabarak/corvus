{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Network-related RPC wrappers extracted from "Corvus.Client.Capnp.Rpc".
module Corvus.Client.Capnp.Rpc.Network
  ( -- * Network read methods
    rpcNetworkList
  , rpcNetworkShow

    -- * Network lifecycle
  , rpcNetworkCreate
  , rpcNetworkStart
  , rpcNetworkStop
  , rpcNetworkDelete
  , rpcNetworkAttachNode
  , rpcNetworkDetachNode

    -- * Network edit
  , rpcNetworkEdit

    -- * Helpers
  , getNetworkClient
  )
where

import qualified Capnp as C
import qualified Capnp.Gen.Corvus as CGCorvus
import qualified Capnp.Gen.Network as CGNet
import Corvus.Client.Capnp.Connection (CapnpConnection (..))
import qualified Corvus.Protocol.Network as PN
import Corvus.Wire.Common (EntityRef (..), entityRefFromText, toCapnpEntityRef)
import Corvus.Wire.Enums (toCapnpNodeAdminState)
import Corvus.Wire.Errors (WireError, showWireError)
import qualified Corvus.Wire.Network as WNet
import Data.Function ((&))
import Data.Int (Int64)
import qualified Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

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
-- Network read methods
-- ---------------------------------------------------------------------

rpcNetworkList :: CapnpConnection -> IO [PN.NetworkInfo]
rpcNetworkList conn = do
  CGCorvus.Daemon'networks'results {CGCorvus.mgr = mgr} <-
    callOn #networks CGCorvus.Daemon'networks'params (ccDaemon conn)
  CGNet.NetworkManager'list'results {CGNet.networks = ns} <-
    callOn #list CGNet.NetworkManager'list'params mgr
  pure (map WNet.fromCapnpNetworkInfo ns)

rpcNetworkShow :: CapnpConnection -> EntityRef -> IO PN.NetworkInfo
rpcNetworkShow conn refIn = do
  CGCorvus.Daemon'networks'results {CGCorvus.mgr = mgr} <-
    callOn #networks CGCorvus.Daemon'networks'params (ccDaemon conn)
  CGNet.NetworkManager'get'results {CGNet.network = nClient} <-
    callOn #get CGNet.NetworkManager'get'params {CGNet.ref = toCapnpEntityRef refIn} mgr
  CGNet.Network'show'results {CGNet.info = info} <-
    callOn #show CGNet.Network'show'params nClient
  pure (WNet.fromCapnpNetworkInfo info)

-- ---------------------------------------------------------------------
-- Network lifecycle
-- ---------------------------------------------------------------------

rpcNetworkCreate
  :: CapnpConnection
  -> Text
  -> Text
  -> Text
  -> Bool
  -> Bool
  -> Bool
  -> [Text]
  -> Text
  -> Bool
  -> IO Int64
rpcNetworkCreate conn name nodeRef subnet dhcp nat autostart dnsServers domain hostDns = do
  CGCorvus.Daemon'networks'results {CGCorvus.mgr = mgr} <-
    callOn #networks CGCorvus.Daemon'networks'params (ccDaemon conn)
  let inner =
        CGNet.NetworkCreateParams
          { CGNet.name = name
          , CGNet.node = toCapnpEntityRef (entityRefFromText nodeRef)
          , CGNet.subnet = subnet
          , CGNet.dhcp = dhcp
          , CGNet.nat = nat
          , CGNet.autostart = autostart
          , CGNet.dnsServers = dnsServers
          , CGNet.domain = domain
          , CGNet.hostDns = hostDns
          }
  CGNet.NetworkManager'create'results {CGNet.network = nClient} <-
    callOn #create CGNet.NetworkManager'create'params {CGNet.params = inner} mgr
  CGNet.Network'show'results {CGNet.info = info} <-
    callOn #show CGNet.Network'show'params nClient
  case info of CGNet.NetworkInfo {CGNet.id = nid} -> pure nid

rpcNetworkStart :: CapnpConnection -> EntityRef -> IO ()
rpcNetworkStart conn ref = do
  nClient <- getNetworkClient conn ref
  _ <- callOn #start CGNet.Network'start'params nClient
  pure ()

rpcNetworkStop :: CapnpConnection -> EntityRef -> Bool -> IO ()
rpcNetworkStop conn ref force = do
  nClient <- getNetworkClient conn ref
  _ <- callOn #stop CGNet.Network'stop'params {CGNet.force = force} nClient
  pure ()

rpcNetworkDelete :: CapnpConnection -> EntityRef -> IO ()
rpcNetworkDelete conn ref = do
  nClient <- getNetworkClient conn ref
  _ <- callOn #delete CGNet.Network'delete'params nClient
  pure ()

rpcNetworkAttachNode :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcNetworkAttachNode conn nwRef nodeRef = do
  nClient <- getNetworkClient conn nwRef
  let params =
        CGNet.NetworkPeerParams {CGNet.node = toCapnpEntityRef nodeRef}
  _ <-
    callOn
      #attachNode
      CGNet.Network'attachNode'params {CGNet.params = params}
      nClient
  pure ()

rpcNetworkDetachNode :: CapnpConnection -> EntityRef -> EntityRef -> IO ()
rpcNetworkDetachNode conn nwRef nodeRef = do
  nClient <- getNetworkClient conn nwRef
  let params =
        CGNet.NetworkPeerParams {CGNet.node = toCapnpEntityRef nodeRef}
  _ <-
    callOn
      #detachNode
      CGNet.Network'detachNode'params {CGNet.params = params}
      nClient
  pure ()

-- ---------------------------------------------------------------------
-- Network edit
-- ---------------------------------------------------------------------

rpcNetworkEdit
  :: CapnpConnection
  -> EntityRef
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe Bool
  -> Maybe [Text]
  -> Maybe Text
  -> Maybe Bool
  -> IO ()
rpcNetworkEdit conn ref mSubnet mDhcp mNat mAs mDns mDomain mHostDns = do
  nClient <- getNetworkClient conn ref
  let p =
        CGNet.NetworkEditParams
          { CGNet.hasName = False
          , CGNet.name = ""
          , CGNet.hasSubnet = Data.Maybe.isJust mSubnet
          , CGNet.subnet = Data.Maybe.fromMaybe "" mSubnet
          , CGNet.hasDhcp = Data.Maybe.isJust mDhcp
          , CGNet.dhcp = Data.Maybe.fromMaybe False mDhcp
          , CGNet.hasNat = Data.Maybe.isJust mNat
          , CGNet.nat = Data.Maybe.fromMaybe False mNat
          , CGNet.hasAutostart = Data.Maybe.isJust mAs
          , CGNet.autostart = Data.Maybe.fromMaybe False mAs
          , CGNet.hasDnsServers = Data.Maybe.isJust mDns
          , CGNet.dnsServers = Data.Maybe.fromMaybe [] mDns
          , CGNet.hasDomain = Data.Maybe.isJust mDomain
          , CGNet.domain = Data.Maybe.fromMaybe "" mDomain
          , CGNet.hasHostDns = Data.Maybe.isJust mHostDns
          , CGNet.hostDns = Data.Maybe.fromMaybe False mHostDns
          }
  _ <- callOn #edit CGNet.Network'edit'params {CGNet.params = p} nClient
  pure ()

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

getNetworkClient :: CapnpConnection -> EntityRef -> IO (C.Client CGNet.Network)
getNetworkClient conn ref = do
  CGCorvus.Daemon'networks'results {CGCorvus.mgr = mgr} <-
    callOn #networks CGCorvus.Daemon'networks'params (ccDaemon conn)
  CGNet.NetworkManager'get'results {CGNet.network = nClient} <-
    callOn #get CGNet.NetworkManager'get'params {CGNet.ref = toCapnpEntityRef ref} mgr
  pure nClient
