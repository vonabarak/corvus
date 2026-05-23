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
import Data.Char (chr, ord)
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
    buildDhcp subnet False = Right disabledDhcp
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
              , dhcpDomain = ""
              , dhcpExtraArgs = []
              , dhcpHostReservations = []
              }
    disabledDhcp =
      DhcpSpec
        { dhcpEnabled = False
        , dhcpRangeStart = ""
        , dhcpRangeEnd = ""
        , dhcpLeaseTime = "12h"
        , dhcpDomain = ""
        , dhcpExtraArgs = []
        , dhcpHostReservations = []
        }
