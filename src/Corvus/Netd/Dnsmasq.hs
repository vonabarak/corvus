{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | dnsmasq supervisor for `corvus-netd`.
--
-- One dnsmasq instance per Corvus network. We spawn it in
-- foreground mode (@--keep-in-foreground@) so we keep direct
-- process control: the returned 'DnsmasqProcess' carries the
-- 'ProcessHandle' for clean termination, and a 'getProcessExitCode'
-- check can detect crashes without `SIGCHLD` plumbing.
--
-- Phase 2 first cut: no child-subreaper, no automatic restart, no
-- EventSink notifications on exit. A follow-up slice can plumb in
-- 'EventSink.onDnsmasqExited' via a watcher thread that calls
-- 'waitForProcess' and forwards the event to the daemon.
module Corvus.Netd.Dnsmasq
  ( DnsmasqProcess (..)
  , DnsmasqStartParams (..)
  , DnsmasqError (..)
  , buildDnsmasqArgs
  , startDnsmasq
  , stopDnsmasq
  )
where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.IO (Handle, IOMode (..), hClose, openFile)
import System.Posix.Types (ProcessID)
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , StdStream (..)
  , createProcess
  , getPid
  , getProcessExitCode
  , proc
  , terminateProcess
  , waitForProcess
  )

-- | Live dnsmasq child. The handle is how Haskell talks to the
-- process; the pid is duplicated for the Cap'n Proto wire response.
data DnsmasqProcess = DnsmasqProcess
  { dpHandle :: !ProcessHandle
  , dpPid :: !ProcessID
  , dpLogHandle :: !Handle
  -- ^ File handle for the log so 'stopDnsmasq' can close it;
  -- dnsmasq inherits it as stdout/stderr.
  }

-- | Inputs the caller passes to 'startDnsmasq'. Mirrors the
-- Cap'n Proto 'DnsmasqParams' struct minus the bridge cap (the
-- session resolves the cap to the bridge ifname and passes that
-- as Text).
data DnsmasqStartParams = DnsmasqStartParams
  { dspBridge :: !T.Text
  , dspListenAddr :: !T.Text
  , dspDhcpRange :: !T.Text
  , dspDomain :: !T.Text
  , dspExtraArgs :: ![T.Text]
  , dspHostReservations :: ![(T.Text, T.Text)]
  -- ^ MAC-keyed reservations. Each entry becomes one
  -- @--dhcp-host=<mac>,<ip>@ flag, so the daemon's IPAM allocation
  -- sticks to the same VM across DHCP renews and node migrations.
  , dspDnsServers :: ![T.Text]
  -- ^ DNS servers to advertise via DHCP option 6. The non-empty
  -- list becomes a single
  -- @--dhcp-option=option:dns-server,IP1,IP2,...@ flag; an empty
  -- list emits no flag (no DNS supplied to leases).
  }
  deriving (Show)

-- | Failures observed during startup. Either we couldn't spawn
-- the binary, or it crashed within the readiness window. The
-- crash variant carries a tail of the captured log so a daemon
-- sees the actual dnsmasq error message ("address already in
-- use", "interface not found", …) rather than a bare exit code.
data DnsmasqError
  = DnsmasqSpawnError !T.Text
  | DnsmasqDiedDuringStartup !Int !T.Text
  deriving (Show)

instance E.Exception DnsmasqError

-- | Pure assembly of dnsmasq's argv from a 'DnsmasqStartParams'.
-- Extracted from 'startDnsmasq' for testability and reused by the
-- spawn path. Order matters only in that 'dspExtraArgs' lands
-- last, letting an operator override any earlier flag.
buildDnsmasqArgs :: DnsmasqStartParams -> [String]
buildDnsmasqArgs p =
  baseArgs <> rangeArgs <> domainArgs <> dnsArgs <> reservationArgs <> extraArgs
  where
    bridge = T.unpack (dspBridge p)
    baseArgs =
      [ "--keep-in-foreground"
      , -- Ignore the system dnsmasq.conf entirely. Distro-shipped
        -- configs often set --bind-dynamic, --conf-dir, port=53
        -- etc.; we want a clean, deterministic command line.
        -- /dev/null is read as an empty config; --conf-file with
        -- an empty value isn't enough on Gentoo (the parser
        -- still walks /etc/dnsmasq.d).
        "--conf-file=/dev/null"
      , "--conf-dir="
      , "--bind-interfaces"
      , "--interface=" <> bridge
      , "--except-interface=lo"
      , "--listen-address=" <> T.unpack (dspListenAddr p)
      , -- --port=0 disables the DNS listener entirely; our role
        -- here is purely DHCP for VM networks. Keeps us off port
        -- 53, which is usually claimed by systemd-resolved on
        -- Gentoo systemd hosts.
        "--port=0"
      , "--no-resolv"
      , "--no-hosts"
      , "--no-poll"
      ]
    rangeArgs
      | T.null (dspDhcpRange p) = []
      | otherwise = ["--dhcp-range=" <> T.unpack (dspDhcpRange p)]
    domainArgs
      | T.null (dspDomain p) = []
      | otherwise = ["--domain=" <> T.unpack (dspDomain p)]
    reservationArgs =
      [ "--dhcp-host=" <> T.unpack mac <> "," <> T.unpack ip
      | (mac, ip) <- dspHostReservations p
      ]
    dnsArgs
      | null (dspDnsServers p) = []
      | otherwise =
          [ "--dhcp-option=option:dns-server,"
              <> T.unpack (T.intercalate "," (dspDnsServers p))
          ]
    extraArgs = T.unpack <$> dspExtraArgs p

-- | Spawn dnsmasq with the given params. Waits ~200ms after fork
-- to confirm the process didn't crash on startup (bad flags,
-- interface doesn't exist, listen-address not on the bridge),
-- then returns the handle.
--
-- Log output is appended to
-- @/var/log/corvus-netd-dnsmasq-<bridge>.log@. If that path is
-- unwritable (test environments etc.) we fall back to
-- @/dev/null@; the supervisor's task is process lifecycle, not
-- log retention.
startDnsmasq :: DnsmasqStartParams -> IO (Either DnsmasqError DnsmasqProcess)
startDnsmasq p = do
  let bridge = T.unpack (dspBridge p)
      logPath = "/var/log/corvus-netd-dnsmasq-" <> bridge <> ".log"
  -- Truncate any prior log so 'tailLog' on failure shows THIS
  -- run's output, not the previous one's. WriteMode opens with
  -- O_TRUNC.
  E.handle (\(_ :: IOError) -> pure ()) $ do
    h <- openFile logPath WriteMode
    hClose h
  logHandle <- openLogOr logPath
  let allArgs = buildDnsmasqArgs p
  spawnResult <-
    E.try @E.SomeException $
      createProcess
        (proc "dnsmasq" allArgs)
          { std_in = NoStream
          , std_out = UseHandle logHandle
          , std_err = UseHandle logHandle
          }
  case spawnResult of
    Left e -> pure (Left (DnsmasqSpawnError (T.pack (show e))))
    Right (_, _, _, hdl) -> do
      threadDelay 200000
      exited <- getProcessExitCode hdl
      case exited of
        Just code -> do
          -- Close our copy of the log so the captured tail
          -- includes everything dnsmasq wrote before exiting.
          E.handle (\(_ :: IOError) -> pure ()) (hClose logHandle)
          tail_ <- tailLog logPath
          pure (Left (DnsmasqDiedDuringStartup (exitInt code) tail_))
        Nothing -> do
          mPid <- getPid hdl
          case mPid of
            Nothing ->
              pure (Left (DnsmasqSpawnError "dnsmasq pid unavailable"))
            Just pid ->
              pure $
                Right
                  DnsmasqProcess
                    { dpHandle = hdl
                    , dpPid = pid
                    , dpLogHandle = logHandle
                    }

-- | Send SIGTERM and wait for exit, then close the inherited log
-- handle. 'terminateProcess'+'waitForProcess' handles the
-- timeout-then-SIGKILL case via process-1.6's own behaviour;
-- we don't add an extra grace timer here.
stopDnsmasq :: DnsmasqProcess -> IO ()
stopDnsmasq dp = do
  terminateProcess (dpHandle dp)
  _ <- waitForProcess (dpHandle dp)
  E.handle (\(_ :: IOError) -> pure ()) (hClose (dpLogHandle dp))

-- ---------------------------------------------------------------------------
-- internal helpers

openLogOr :: FilePath -> IO Handle
openLogOr path =
  E.handle (\(_ :: IOError) -> openFile "/dev/null" WriteMode) $
    openFile path AppendMode

-- | Best-effort tail of the captured log for surfacing in a
-- 'DnsmasqDiedDuringStartup' error. Limited to the last 2 KiB so
-- the Cap'n Proto error envelope stays bounded.
tailLog :: FilePath -> IO T.Text
tailLog path =
  E.handle (\(_ :: IOError) -> pure "") $ do
    contents <- readFile path
    let trimmed = drop (max 0 (length contents - 2048)) contents
    pure (T.pack trimmed)

exitInt :: ExitCode -> Int
exitInt ExitSuccess = 0
exitInt (ExitFailure n) = n
