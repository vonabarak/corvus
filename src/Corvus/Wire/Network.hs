{-# LANGUAGE RecordWildCards #-}

-- | Cap'n Proto conversion for virtual networks.
module Corvus.Wire.Network
  ( toCapnpNetworkInfo
  , fromCapnpNetworkInfo
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Network as CGNet
import qualified Corvus.Protocol.Network as P
import Corvus.Wire.Time (nanosToUtcTime, utcTimeToNanos)

toCapnpNetworkInfo :: P.NetworkInfo -> C.Parsed CGNet.NetworkInfo
toCapnpNetworkInfo P.NetworkInfo {..} =
  CGNet.NetworkInfo
    { CGNet.id = nwiId
    , CGNet.name = nwiName
    , CGNet.subnet = nwiSubnet
    , CGNet.dhcp = nwiDhcp
    , CGNet.nat = nwiNat
    , CGNet.running = nwiRunning
    , CGNet.dnsmasqPid = maybe 0 fromIntegral nwiDnsmasqPid
    , CGNet.createdAt = utcTimeToNanos nwiCreatedAt
    , CGNet.autostart = nwiAutostart
    , CGNet.vni = maybe 0 fromIntegral nwiVni
    , CGNet.peerNodeIds = nwiPeerNodeIds
    }

fromCapnpNetworkInfo :: C.Parsed CGNet.NetworkInfo -> P.NetworkInfo
fromCapnpNetworkInfo CGNet.NetworkInfo {..} =
  P.NetworkInfo
    { P.nwiId = id
    , P.nwiName = name
    , P.nwiSubnet = subnet
    , P.nwiDhcp = dhcp
    , P.nwiNat = nat
    , P.nwiRunning = running
    , P.nwiDnsmasqPid = if dnsmasqPid == 0 then Nothing else Just (fromIntegral dnsmasqPid)
    , P.nwiCreatedAt = nanosToUtcTime createdAt
    , P.nwiAutostart = autostart
    , P.nwiVni = if vni == 0 then Nothing else Just (fromIntegral vni)
    , P.nwiPeerNodeIds = peerNodeIds
    }
