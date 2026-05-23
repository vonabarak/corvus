{-# LANGUAGE OverloadedStrings #-}

-- | Pure IPAM + VNI helpers for the network handler.
--
-- These functions don't touch the database — callers pass in the
-- already-allocated state (list of used IPs, list of used VNIs) and
-- receive a fresh allocation back. That keeps the policy here unit
-- testable without persistence, and lets the handler decide where the
-- allocation happens transactionally.
module Corvus.Handlers.Network.Ipam
  ( allocateIp
  , vniFromPool
  , defaultVniBase
  )
where

import Corvus.Utils.Network (ipToText, parseCidr, parseIpv4, prefixToMask)
import Data.Bits (complement, (.&.))
import qualified Data.Set as Set
import Data.Text (Text)

-- | Allocate the next free host address in a network's CIDR, skipping
-- the network address, the gateway (@.1@), and the broadcast address.
-- @usedIps@ is the set of v4 addresses already handed out (any
-- unparseable entries are ignored, since they can't conflict).
allocateIp :: Text -> [Text] -> Either Text Text
allocateIp cidr usedIps = do
  (addr, prefix) <- parseCidr cidr
  let mask = prefixToMask prefix
      network = addr .&. mask
      broadcast = network + complement mask
      gateway = network + 1
      used = Set.fromList [w | u <- usedIps, Right w <- [parseIpv4 u]]
      reserved = Set.fromList [network, gateway, broadcast]
      pool = [network + 2 .. broadcast - 1]
      free = [ip | ip <- pool, not (Set.member ip used), not (Set.member ip reserved)]
  case free of
    [] -> Left $ "no free IP addresses in " <> cidr
    (ip : _) -> Right (ipToText ip)

-- | First VNI handed out when a network gains a peer. Picked above
-- the IANA-reserved range (1–4095 overlap with VLAN IDs; we leave a
-- generous gap to make tcpdump output unambiguous).
defaultVniBase :: Int
defaultVniBase = 10000

-- | Pick the next free VNI given the set of in-use ones, starting at
-- 'defaultVniBase'.
vniFromPool :: [Int] -> Int
vniFromPool used =
  let s = Set.fromList used
   in head [v | v <- [defaultVniBase ..], not (Set.member v s)]
