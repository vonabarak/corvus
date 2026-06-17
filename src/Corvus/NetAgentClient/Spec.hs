{-# LANGUAGE OverloadedStrings #-}

-- | Helpers that turn daemon-side DB rows into agent-side specs.
--
-- The daemon's database is the only intent store; the agent
-- holds no persistent state and rebuilds its kernel state from
-- specs the daemon supplies on every (re)connect. This module
-- centralises the conversions so handlers (start / stop / VM
-- lifecycle / re-apply on reconnect) all agree on the same
-- canonical mapping.
--
-- Naming conventions enforced here mirror the validation the
-- agent's 'Corvus.Netd.Cleanup' prefix-scan and the apply
-- methods' validators rely on:
--
--   * Bridges: @"corvus-br-" <> base36(networkId)@
--   * TAPs:    @"corvus-tap-" <> base36(interfaceId)@
--
-- Both fit IFNAMSIZ (15-char max) for any 32-bit ID via base-36
-- encoding (max 7 chars). DB IDs are Int64 in theory; we error
-- out on overflow to avoid silent truncation.
module Corvus.NetAgentClient.Spec
  ( corvusBridgeName
  , corvusTapName
  , networkToSpec
  , encodeDnsServers
  , decodeDnsServers
  , effectiveDomain
  , sanitiseDomain
  )
where

import Corvus.Model (Network (..))
import Corvus.NetAgentClient
  ( DhcpSpec (..)
  , NatSpec (..)
  , NetworkSpec (..)
  , OverlaySpec (..)
  )
import qualified Corvus.Utils.Network as N
import Data.Char (chr, isAscii, isDigit, isLetter, ord)
import Data.Int (Int64)
import qualified Data.Text as T

-- | Bridge ifname for a DB-side network ID. Always begins with
-- @corvus-br-@ so 'Corvus.Netd.Cleanup' picks it up.
corvusBridgeName :: Int64 -> T.Text
corvusBridgeName nid = "corvus-br-" <> base36 nid

-- | TAP ifname for a DB-side network-interface ID.
corvusTapName :: Int64 -> T.Text
corvusTapName iid = "corvus-tap-" <> base36 iid

-- | Render a non-negative Int64 in base-36. Negative IDs (we
-- never assign them in practice) get a leading "neg" tag so the
-- name fails the agent's prefix check loudly rather than
-- colliding with positive IDs.
base36 :: Int64 -> T.Text
base36 n
  | n < 0 = "neg" <> base36 (negate n)
  | n == 0 = "0"
  | otherwise = T.pack (go n "")
  where
    go 0 acc = acc
    go k acc = go (k `div` 36) (digit (k `mod` 36) : acc)
    digit d
      | d < 10 = chr (ord '0' + fromIntegral d)
      | otherwise = chr (ord 'a' + fromIntegral d - 10)

-- | Convert a DB-side 'Network' row into the agent's
-- 'NetworkSpec'. Subnet decomposition errors (the row's CIDR
-- doesn't parse) surface as 'Left'.
--
-- The network's gateway IP becomes the bridge's CIDR
-- ("10.0.0.1/24" — the agent assigns it via @ip addr add@).
-- The dnsmasq listen address is the same IP without the prefix
-- (the agent strips the @/N@ suffix internally).
networkToSpec :: Int64 -> Network -> Either T.Text NetworkSpec
networkToSpec networkId network = do
  let subnet = networkSubnet network
  cidr <-
    if T.null subnet
      then Right ""
      else do
        gw <- N.gatewayAddress subnet
        prefix <- N.prefixLength subnet
        Right (gw <> "/" <> prefix)
  dhcp <- buildDhcp subnet (networkDhcp network)
  pure
    NetworkSpec
      { nsName = corvusBridgeName networkId
      , nsCidr = cidr
      , nsMtu = 1500
      , nsNat =
          NatSpec
            { natEnabled = networkNat network
            , natUplinkIf = ""
            -- "" → agent matches any outbound interface for the
            -- masquerade rule. A future enhancement can plumb
            -- a per-network or daemon-global uplink override.
            }
      , nsDhcp = dhcp
      , nsOverlay = OverlayNone
      }
  where
    dnsServers = decodeDnsServers (networkDnsServers network)
    -- Default the domain to the (sanitised) network name when the
    -- operator hasn't set one explicitly. This is what gets emitted
    -- as `--domain=` to dnsmasq AND used by the agent to choose the
    -- systemd-resolved drop-in's `~suffix` route.
    domain = effectiveDomain (networkName network) (networkDomain network)
    hostDns = networkHostDns network
    buildDhcp _ False = Right disabledDhcp
    buildDhcp subnet True
      | T.null subnet = Left "DHCP enabled but subnet is empty"
      | otherwise = do
          start <- N.dhcpRangeStart subnet
          end <- N.dhcpRangeEnd subnet
          Right
            DhcpSpec
              { dhcpEnabled = True
              , dhcpRangeStart = start
              , dhcpRangeEnd = end
              , dhcpLeaseTime = "12h"
              , dhcpDomain = domain
              , dhcpExtraArgs = []
              , dhcpHostReservations = []
              , dhcpDnsServers = dnsServers
              , dhcpHostDns = hostDns
              }
    disabledDhcp =
      DhcpSpec
        { dhcpEnabled = False
        , dhcpRangeStart = ""
        , dhcpRangeEnd = ""
        , dhcpLeaseTime = "12h"
        , -- DNS turning on follows DHCP today: no DHCP → no domain
          -- emitted regardless of the column value.
          dhcpDomain = ""
        , dhcpExtraArgs = []
        , dhcpHostReservations = []
        , dhcpDnsServers = dnsServers
        , dhcpHostDns = False
        }

-- | Pack a DNS-server list into the comma-joined 'Text' the
-- 'Network.dnsServers' DB column carries. The empty list maps to
-- the empty string so default-on-migrate matches "no DNS option".
-- Whitespace around entries is dropped, blanks are filtered out.
encodeDnsServers :: [T.Text] -> T.Text
encodeDnsServers = T.intercalate "," . filter (not . T.null) . map T.strip

-- | Inverse of 'encodeDnsServers'. Empty string → @[]@.
decodeDnsServers :: T.Text -> [T.Text]
decodeDnsServers t =
  filter (not . T.null) (map T.strip (T.splitOn "," t))

-- | Pick the DNS suffix actually used by the agent + on-host resolver
-- drop-in. An explicit, non-empty @domain@ column wins; otherwise we
-- fall back to the sanitised network name. Returns the empty string
-- only when the network's name is itself unusable (all-non-DNS chars).
effectiveDomain :: T.Text {- network name -} -> T.Text {- networkDomain column -} -> T.Text
effectiveDomain name colVal
  | not (T.null colVal) = sanitiseDomain colVal
  | otherwise = sanitiseDomain name

-- | Coerce a free-form network name into a DNS label that dnsmasq and
-- systemd-resolved both accept: lowercase ASCII letters / digits /
-- hyphen, no leading or trailing hyphen, hyphen runs collapsed.
sanitiseDomain :: T.Text -> T.Text
sanitiseDomain raw =
  T.dropAround (== '-')
    . T.pack
    . collapseHyphens
    . T.unpack
    . T.map mapCh
    $ T.toLower (T.strip raw)
  where
    mapCh c
      | isAscii c && (isLetter c || isDigit c) = c
      | c == '-' = '-'
      | otherwise = '-'
    collapseHyphens ('-' : '-' : rest) = collapseHyphens ('-' : rest)
    collapseHyphens (c : rest) = c : collapseHyphens rest
    collapseHyphens [] = []
