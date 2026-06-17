{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | systemd-resolved per-link configurator for `corvus-netd`.
--
-- For every managed network that serves DNS (DHCP on + a non-empty
-- domain) and has @hostDns@ enabled, the agent binds three pieces
-- of per-interface resolver state to the bridge via @resolvectl@:
--
-- @
-- resolvectl dns    \<bridge\> \<bridge-ip\>
-- resolvectl domain \<bridge\> ~\<suffix\>
-- resolvectl dnssec \<bridge\> no
-- @
--
-- The @~\<suffix\>@ routing-only domain tells resolved to consult
-- the bridge's DNS server ONLY for queries matching the suffix;
-- the rest of the host's DNS continues to flow through whatever
-- upstream the host already uses. The per-link @dnssec no@ scopes
-- the DNSSEC opt-out to queries routed through this interface,
-- which is necessary because dnsmasq doesn't sign its
-- DHCP-lease-derived answers and the @<suffix>.@ zone is private
-- (resolved can't fetch DS records for it from the public root,
-- so any global DNSSEC validation would @failed-auxiliary@ and
-- reject the otherwise-valid answer).
--
-- We deliberately use the runtime @resolvectl@ surface rather than
-- a @resolved.conf.d@ drop-in: drop-in @DNSSEC=@ is a GLOBAL
-- setting that would disable DNSSEC for the whole host, while
-- per-link @resolvectl dnssec@ scopes it correctly. Persistence
-- across reboots is provided by the daemon's
-- @reapplyRunningNetworks@ pass, which re-issues @applyNetwork@
-- for every running network on startup and so reaches this code
-- path again.
--
-- 'Corvus.Netd.Cleanup.cleanupCorvusKernelState' calls
-- 'revertAllResolvedLinks' after dropping all @corvus-br*@
-- interfaces so resolved doesn't carry stale per-link state across
-- agent restarts.
module Corvus.Netd.HostDns
  ( installResolvedRouting
  , revertResolvedRouting
  , revertAllResolvedLinks
  )
where

import qualified Control.Exception as E
import Control.Monad (unless, when)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

-- | Install per-link resolved routing for @bridge@ so the host
-- forwards @\<anything\>.\<suffix\>@ queries to @bridgeIp@. No-op
-- + warn when @/run/systemd/resolve/@ is missing (the host isn't
-- running systemd-resolved) or @resolvectl@ isn't on PATH.
-- Idempotent: re-running with the same inputs is safe.
installResolvedRouting
  :: T.Text
  -- ^ bridge name (e.g. @corvus-br-1@)
  -> T.Text
  -- ^ bridge IP, bare (e.g. @192.168.72.1@)
  -> T.Text
  -- ^ DNS suffix to route (e.g. @corvus@)
  -> IO ()
installResolvedRouting bridge bridgeIp suffix
  | T.null suffix = pure ()
  | T.null bridgeIp = pure ()
  | otherwise = do
      ok <- resolvedAvailable
      unless ok $
        warn ("systemd-resolved not detected; skipping host DNS for " <> bridge)
      if not ok
        then pure ()
        else do
          let b = T.unpack bridge
          -- Order: DNS server first (creates the per-link entry),
          -- then routing domain (binds the suffix), then DNSSEC
          -- opt-out (must come after the link has a DNS so resolved
          -- has something to apply the setting to). All three
          -- complete in <100ms total.
          _ <- runResolvectl ["dns", b, T.unpack bridgeIp] bridge
          _ <- runResolvectl ["domain", b, "~" <> T.unpack suffix] bridge
          _ <- runResolvectl ["dnssec", b, "no"] bridge
          pure ()

-- | Drop the per-link resolved state for @bridge@. Best-effort:
-- @revert@ on an interface with no per-link config is a no-op in
-- modern resolved versions, so this is also safe to call on a
-- bridge that never had host-DNS enabled.
revertResolvedRouting :: T.Text -> IO ()
revertResolvedRouting bridge = do
  ok <- resolvedAvailable
  when ok $ do
    _ <- runResolvectl ["revert", T.unpack bridge] bridge
    pure ()

-- | Revert per-link resolved state for every interface matching
-- @corvus-br*@. Called from the agent's startup-cleanup pass so a
-- restarted netd doesn't leave behind routing for vanished
-- bridges.
revertAllResolvedLinks :: [T.Text] -> IO ()
revertAllResolvedLinks bridges = do
  ok <- resolvedAvailable
  when ok $
    mapM_ revertResolvedRouting bridges

-- ---------------------------------------------------------------------------
-- internals

-- | systemd-resolved present on the host. We probe @/run/systemd/resolve/@
-- since that's the runtime-state marker; any system with this directory
-- has @resolvectl@ on PATH too (it's part of the systemd package).
resolvedAvailable :: IO Bool
resolvedAvailable = doesDirectoryExist "/run/systemd/resolve"

-- | Run @resolvectl args@; log + ignore failures.
runResolvectl :: [String] -> T.Text -> IO ExitCode
runResolvectl args bridge =
  E.handle onErr $ do
    (code, _, err) <- readProcessWithExitCode "resolvectl" args ""
    case code of
      ExitSuccess -> pure ExitSuccess
      ExitFailure _ -> do
        warn
          ( "resolvectl "
              <> T.pack (unwords args)
              <> " failed for "
              <> bridge
              <> ": "
              <> T.strip (T.pack err)
          )
        pure code
  where
    onErr :: E.SomeException -> IO ExitCode
    onErr e = do
      warn (T.pack ("resolvectl spawn failed: " <> show e))
      pure (ExitFailure 127)

warn :: T.Text -> IO ()
warn msg =
  unless (T.null msg) $
    hPutStrLn stderr ("[netd] HostDns: " <> T.unpack msg)
