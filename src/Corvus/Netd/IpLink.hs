{-# LANGUAGE OverloadedStrings #-}

-- | Thin wrapper around the iproute2 `ip` command for the network
-- agent's kernel-state ops.
--
-- The agent runs as root (system systemd service) so plain `ip
-- link add` works directly — there's no nsenter, no user-ns
-- gymnastics. Errors are surfaced as 'IpLinkError' with the
-- captured stderr so the cap impl can attach it to the Cap'n
-- Proto failure response.
--
-- Phase 2 keeps to a shell-out model rather than netlink-via-FFI
-- for two reasons:
--
--   1. The kernel mutations here are infrequent (bridge create /
--      destroy / link up — once per network creation, not per
--      packet). The `fork+exec ip` overhead is dwarfed by every
--      other cost.
--   2. `ip` already does the right thing for ENOTUNIQ / EEXIST
--      etc. — we get useful error text without re-implementing
--      netlink error decoding.
--
-- A future rewrite to direct netlink (via @netlink@ Hackage
-- package, or @rtnetlink@) is on the table for Phase 2.5 if
-- the rtnetlink watcher pulls in the deps anyway.
module Corvus.Netd.IpLink
  ( IpLinkError (..)
  , bridgeAdd
  , bridgeDel
  , addrAdd
  , linkSetUp
  , linkExists
  , tapAdd
  , tapDel
  , linkSetMaster
  , linkSetNoMaster
  )
where

import qualified Control.Exception as E
import qualified Data.Text as T
import Data.Word (Word32)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Captured failure from an @ip@ invocation. Holds the argv
-- vector and the stderr text so the cap impl can build a useful
-- Cap'n Proto error message without losing context.
data IpLinkError = IpLinkError
  { ileArgs :: ![T.Text]
  , ileExitCode :: !Int
  , ileStderr :: !T.Text
  }
  deriving (Show)

instance E.Exception IpLinkError

-- | @ip link add <name> type bridge@. Idempotency is the caller's
-- problem — call 'linkExists' first if you want to be permissive.
bridgeAdd :: T.Text -> IO (Either IpLinkError ())
bridgeAdd name =
  runIp ["link", "add", T.unpack name, "type", "bridge"]

-- | @ip link del <name>@. Fails if the link doesn't exist; safe
-- to call against a stale name as long as you're prepared to
-- ignore the error.
bridgeDel :: T.Text -> IO (Either IpLinkError ())
bridgeDel name =
  runIp ["link", "del", T.unpack name]

-- | @ip addr add <cidr> dev <iface>@. The CIDR string is passed
-- through unchanged; the kernel validates it.
addrAdd :: T.Text -> T.Text -> IO (Either IpLinkError ())
addrAdd cidr iface =
  runIp ["addr", "add", T.unpack cidr, "dev", T.unpack iface]

-- | @ip link set <iface> up@.
linkSetUp :: T.Text -> IO (Either IpLinkError ())
linkSetUp iface =
  runIp ["link", "set", T.unpack iface, "up"]

-- | True iff a link with this name exists on the host. Lightweight
-- check via @ip link show@ that just asks for one interface.
linkExists :: T.Text -> IO Bool
linkExists iface = do
  (code, _out, _err) <-
    readProcessWithExitCode "ip" ["link", "show", "dev", T.unpack iface] ""
  pure (code == ExitSuccess)

-- | @ip tuntap add dev <name> mode tap user <uid> group <gid>@.
-- Creates a persistent TAP whose @/dev/net/tun@ reopen rights are
-- granted to the specified uid/gid via @TUNSETOWNER@ — so QEMU
-- (running as that uid) can open it without @CAP_NET_ADMIN@.
tapAdd :: T.Text -> Word32 -> Word32 -> IO (Either IpLinkError ())
tapAdd name uid gid =
  runIp
    [ "tuntap"
    , "add"
    , "dev"
    , T.unpack name
    , "mode"
    , "tap"
    , "user"
    , show uid
    , "group"
    , show gid
    ]

-- | @ip tuntap del dev <name> mode tap@. The @mode tap@ must match
-- the create-time mode or the kernel rejects the request.
tapDel :: T.Text -> IO (Either IpLinkError ())
tapDel name =
  runIp ["tuntap", "del", "dev", T.unpack name, "mode", "tap"]

-- | @ip link set <iface> master <bridge>@.
linkSetMaster :: T.Text -> T.Text -> IO (Either IpLinkError ())
linkSetMaster iface master =
  runIp ["link", "set", T.unpack iface, "master", T.unpack master]

-- | @ip link set <iface> nomaster@. Idempotent — detaching an
-- already-detached interface is a no-op at the kernel level (but
-- iproute2 may still return success without doing anything).
linkSetNoMaster :: T.Text -> IO (Either IpLinkError ())
linkSetNoMaster iface =
  runIp ["link", "set", T.unpack iface, "nomaster"]

-- ---------------------------------------------------------------------------
-- Internal

runIp :: [String] -> IO (Either IpLinkError ())
runIp args = do
  (code, _stdout, stderr) <- readProcessWithExitCode "ip" args ""
  case code of
    ExitSuccess -> pure (Right ())
    ExitFailure n ->
      pure $
        Left
          IpLinkError
            { ileArgs = map T.pack ("ip" : args)
            , ileExitCode = n
            , ileStderr = T.strip (T.pack stderr)
            }
