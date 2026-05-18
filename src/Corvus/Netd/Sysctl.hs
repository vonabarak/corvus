{-# LANGUAGE OverloadedStrings #-}

-- | Typed sysctl shims for the network agent.
--
-- The Cap'n Proto schema deliberately does NOT expose a generic
-- @setSysctl(key, value)@ method — that would let a compromised
-- daemon flip arbitrary kernel knobs (e.g.
-- `kernel.unprivileged_userns_clone`, `kernel.modules_disabled`)
-- to weaken the host. Every kernel param the daemon legitimately
-- needs gets its own typed wrapper here.
--
-- Phase 2 ships just 'setIpForwarding'. New knobs (e.g.
-- @net.bridge.bridge-nf-call-iptables=0@, IPv6-equivalents) land
-- as additional named functions as the agent grows.
module Corvus.Netd.Sysctl
  ( NetFamily (..)
  , setIpForwarding
  )
where

-- | IP family selector. Mirrors the schema's @NetFamily@ enum so
-- the cap impl can pass it through without an extra conversion.
data NetFamily = V4 | V6
  deriving (Eq, Show)

-- | Toggle IPv4 or IPv6 forwarding. Writes directly to
-- @/proc/sys/net/(ipv4|ipv6/conf/all)/(ip_)?forward@ — equivalent
-- to @sysctl -w net.ipv4.ip_forward=1@ but avoids a fork+exec.
--
-- Throws 'IOError' on failure (e.g. /proc not writable, agent
-- not running as root). The cap impl catches and converts to
-- the Cap'n Proto error envelope.
setIpForwarding :: Bool -> NetFamily -> IO ()
setIpForwarding enabled family = do
  let path = procPath family
      value = if enabled then "1" else "0"
  writeFile path value
  where
    procPath V4 = "/proc/sys/net/ipv4/ip_forward"
    procPath V6 = "/proc/sys/net/ipv6/conf/all/forwarding"
