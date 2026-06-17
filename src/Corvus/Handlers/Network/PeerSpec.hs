{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Build the per-node 'NetworkSpec' a multi-node overlay pushes
-- to each participating netd.
--
-- The owner node runs the full network — bridge with CIDR, NAT,
-- dnsmasq + lease reservations. Peer nodes get an L2-only bridge
-- (no CIDR, no NAT, no dnsmasq) plus a VXLAN VTEP joined to the
-- mesh, so the BUM flood path carries broadcasts back to the
-- owner's dnsmasq.
--
-- Both flavours share name (the bridge ifname must be identical on
-- every node), MTU (1450 once VXLAN's 50-byte header is accounted
-- for), and overlay (same VNI + the rest of the mesh as flood
-- destinations).
module Corvus.Handlers.Network.PeerSpec
  ( PeerRole (..)
  , buildPeerSpec
  , collectNetworkMembers
  , collectHostReservations
  )
where

import qualified Corvus.Model as M
import qualified Corvus.NetAgentClient as NA
import Corvus.NetAgentClient.Spec (corvusBridgeName)
import qualified Corvus.NetAgentClient.Spec as Spec
import qualified Corvus.Utils.Network as N
import Data.Int (Int64)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (Entity (..), get, selectList, (==.))
import Database.Persist.Postgresql (SqlPersistT)
import Database.Persist.Sql (fromSqlKey)

-- | Which side of the overlay a particular spec is built for.
data PeerRole
  = -- | Network.nodeId — runs CIDR + NAT + dnsmasq.
    RoleOwner
  | -- | Listed in NetworkPeer — L2-only bridge + VXLAN VTEP, no IP.
    RolePeer
  deriving (Eq, Show)

-- | Build the spec the netd on @forNode@ should reconcile to. The
-- @members@ argument is every node that participates in the network
-- (owner + every peer) — used to compute that node's peer-IPs list
-- (everyone except itself).
buildPeerSpec
  :: M.Network
  -> Int64
  -- ^ network entity id (for the bridge ifname)
  -> PeerRole
  -> (M.NodeId, M.Node)
  -- ^ (forNode, its row); the row's @nodeHost@ is the underlay
  -- IP we hand to @ip link add … local <ip>@.
  -> [(M.NodeId, M.Node)]
  -- ^ all members including @forNode@.
  -> [NA.DhcpHostReservation]
  -- ^ reservations to emit on the OWNER spec; ignored for peers.
  -> Either Text NA.NetworkSpec
buildPeerSpec network networkId role (forNode, forNodeRow) members reservations = do
  let subnet = M.networkSubnet network
      bridgeName = corvusBridgeName networkId
      overlay = case M.networkVni network of
        Nothing -> NA.OverlayNone
        Just vni ->
          NA.OverlayVxlan
            NA.VxlanSpec
              { NA.vsVni = fromIntegral vni
              , NA.vsLocalIp = M.nodeHost forNodeRow
              , NA.vsPeerIps =
                  L.sort
                    [ M.nodeHost row
                    | (nid, row) <- members
                    , nid /= forNode
                    ]
              }
      -- Guests must subtract the VXLAN overhead (50 bytes) from their
      -- MTU. When the overlay is OverlayNone the bridge keeps the
      -- default 1500.
      mtu = case overlay of
        NA.OverlayNone -> 1500
        NA.OverlayVxlan _ -> 1450
  case role of
    RoleOwner -> do
      cidr <-
        if T.null subnet
          then Right ""
          else do
            gw <- N.gatewayAddress subnet
            prefix <- N.prefixLength subnet
            Right (gw <> "/" <> prefix)
      dhcp <- buildOwnerDhcp subnet (M.networkDhcp network) reservations dnsServers domain hostDns
      Right
        NA.NetworkSpec
          { NA.nsName = bridgeName
          , NA.nsCidr = cidr
          , NA.nsMtu = mtu
          , NA.nsNat =
              NA.NatSpec
                { NA.natEnabled = M.networkNat network
                , NA.natUplinkIf = ""
                }
          , NA.nsDhcp = dhcp
          , NA.nsOverlay = overlay
          }
    RolePeer ->
      Right
        NA.NetworkSpec
          { NA.nsName = bridgeName
          , -- L2-only bridge: no CIDR, no NAT, no dnsmasq. Frames
            -- destined for the owner traverse the VXLAN.
            NA.nsCidr = ""
          , NA.nsMtu = mtu
          , NA.nsNat = NA.NatSpec {NA.natEnabled = False, NA.natUplinkIf = ""}
          , NA.nsDhcp = disabledDhcp dnsServers
          , NA.nsOverlay = overlay
          }
  where
    dnsServers =
      Spec.decodeDnsServers (M.networkDnsServers network)
    domain =
      Spec.effectiveDomain (M.networkName network) (M.networkDomain network)
    hostDns = M.networkHostDns network

-- | Resolve the network's owner row + every peer row from the DB.
-- Returns @(owner, peers)@ where @peers@ does NOT include the owner.
-- A peer whose Node row has gone missing is silently skipped — the
-- supervisor loop will re-push to live members on its next pass.
collectNetworkMembers
  :: M.Network
  -> M.NetworkId
  -> SqlPersistT IO (Either Text ((M.NodeId, M.Node), [(M.NodeId, M.Node)]))
collectNetworkMembers network networkKey = do
  let ownerId = M.networkNodeId network
  mOwner <- get ownerId
  case mOwner of
    Nothing ->
      pure $
        Left $
          "network's owner node " <> T.pack (show (fromSqlKey ownerId)) <> " has vanished"
    Just owner -> do
      peerRows <- selectList [M.NetworkPeerNetworkId ==. networkKey] []
      let peerNodeIds = map (M.networkPeerNodeId . entityVal) peerRows
      peers <- catMaybes <$> mapM lookupPeer peerNodeIds
      pure $ Right ((ownerId, owner), peers)
  where
    lookupPeer nid = do
      mNode <- get nid
      pure ((nid,) <$> mNode)

-- | Collect every (MAC, IP) pair allocated to a NIC attached to
-- the given network. Used to build the owner-side dnsmasq
-- reservation list.
collectHostReservations
  :: M.NetworkId -> SqlPersistT IO [NA.DhcpHostReservation]
collectHostReservations networkKey = do
  nics <-
    selectList [M.NetworkInterfaceNetworkId ==. Just networkKey] []
  pure
    [ NA.DhcpHostReservation
      { NA.dhrMac = M.networkInterfaceMacAddress nic
      , NA.dhrIp = ip
      }
    | Entity _ nic <- nics
    , Just ip <- [M.networkInterfaceIpAddress nic]
    ]

-- ---------------------------------------------------------------------------
-- internals

buildOwnerDhcp
  :: Text
  -> Bool
  -> [NA.DhcpHostReservation]
  -> [Text]
  -> Text
  -- ^ effective DNS suffix (already defaulted to the network name)
  -> Bool
  -- ^ hostDns: install resolved drop-in
  -> Either Text NA.DhcpSpec
buildOwnerDhcp _subnet False _ dnsServers _ _ = Right (disabledDhcp dnsServers)
buildOwnerDhcp subnet True reservations dnsServers domain hostDns
  | T.null subnet = Left "DHCP enabled but subnet is empty"
  | otherwise = do
      start <- N.dhcpRangeStart subnet
      end <- N.dhcpRangeEnd subnet
      Right
        NA.DhcpSpec
          { NA.dhcpEnabled = True
          , NA.dhcpRangeStart = start
          , NA.dhcpRangeEnd = end
          , NA.dhcpLeaseTime = "12h"
          , NA.dhcpDomain = domain
          , NA.dhcpExtraArgs = []
          , NA.dhcpHostReservations = reservations
          , NA.dhcpDnsServers = dnsServers
          , NA.dhcpHostDns = hostDns
          }

disabledDhcp :: [Text] -> NA.DhcpSpec
disabledDhcp dnsServers =
  NA.DhcpSpec
    { NA.dhcpEnabled = False
    , NA.dhcpRangeStart = ""
    , NA.dhcpRangeEnd = ""
    , NA.dhcpLeaseTime = "12h"
    , NA.dhcpDomain = ""
    , NA.dhcpExtraArgs = []
    , NA.dhcpHostReservations = []
    , NA.dhcpDnsServers = dnsServers
    , NA.dhcpHostDns = False
    }
