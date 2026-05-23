{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Startup + shutdown cleanup for `corvus-netd`.
--
-- The agent is stateless: nothing persists to disk. Every fresh
-- process starts with a guaranteed-empty world by deleting every
-- kernel resource whose name matches one of the agent's
-- conventional prefixes:
--
--   * @ip link@      — @corvus-br*@ and @corvus-tap*@
--   * @nftables@     — table @inet corvus_fw@
--   * @dnsmasq@      — processes whose argv contains
--                       @--interface=corvus-br@
--
-- 'cleanupCorvusKernelState' runs once during
-- 'Corvus.Netd.Server.runNetdServer' before the listener binds,
-- and once from the @SIGTERM@ / @SIGINT@ handler in
-- @app/netd/Main.hs@ before the process exits. Symmetric so
-- restarts and explicit stops both terminate with no agent state
-- left on the host.
module Corvus.Netd.Cleanup
  ( cleanupCorvusKernelState
  , corvusBridgePrefix
  , corvusTapPrefix
  , corvusVxlanPrefix
  )
where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, logWarnN, runStderrLoggingT)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.Posix.Signals (sigKILL, sigTERM, signalProcess)
import System.Posix.Types (CPid (..), ProcessID)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

-- | Naming prefixes the agent reserves. The startup / shutdown
-- pass treats anything matching as fair game; anything else is
-- left alone.
corvusBridgePrefix, corvusTapPrefix, corvusVxlanPrefix :: T.Text
corvusBridgePrefix = "corvus-br"
corvusTapPrefix = "corvus-tap"
corvusVxlanPrefix = "corvus-vx"

-- | Best-effort full cleanup. Each step logs + swallows failures
-- so a half-broken host still gets as much cleaned up as
-- possible. Order matters: dnsmasq before nftables (rules may
-- reference DHCP), TAPs before bridges (kernel refuses to delete
-- a bridge with slaves).
cleanupCorvusKernelState :: IO ()
cleanupCorvusKernelState = runStderrLoggingT $ do
  -- 1. dnsmasq children matching our argv pattern.
  pids <- liftIO findCorvusDnsmasq
  unless (null pids) $
    logInfoN ("[netd] cleanup: killing " <> tshow (length pids) <> " dnsmasq pid(s)")
  mapM_ (liftIO . signalAndReap) pids

  -- 2. nftables table.
  warnOn "nft delete table inet corvus_fw" =<< liftIO nftDropCorvusTable

  -- 3. TAPs.
  taps <- liftIO (ipLinksByPrefix corvusTapPrefix)
  mapM_ (delAndWarn "tap") taps

  -- 4. VXLAN VTEPs (must precede bridge delete: a bridge with a
  -- slaved vxlan refuses to go away). Distinct prefix so the prefix
  -- scan doesn't accidentally pick up bridges starting with
  -- @corvus-vx*@ (none exist by convention).
  vxlans <- liftIO (ipLinksByPrefix corvusVxlanPrefix)
  mapM_ (delAndWarn "vxlan") vxlans

  -- 5. Bridges last (after their slaves are gone).
  bridges <- liftIO (ipLinksByPrefix corvusBridgePrefix)
  mapM_ (delAndWarn "bridge") bridges
  where
    delAndWarn kind name = do
      result <- liftIO (ipLinkDel name)
      warnOn ("ip link del " <> kind <> " " <> name) result

warnOn :: T.Text -> Either T.Text () -> LoggingT IO ()
warnOn _ (Right ()) = pure ()
warnOn label (Left msg) =
  logWarnN ("[netd] cleanup: " <> label <> ": " <> msg)

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

-- ---------------------------------------------------------------------------
-- Process control

-- | SIGTERM, poll for exit up to ~1s, SIGKILL if still alive.
-- We don't waitpid — these processes were not forked by us in
-- this process instance (the agent was restarted; PID 1 reaps).
signalAndReap :: ProcessID -> IO ()
signalAndReap pid = do
  ignoreErr (signalProcess sigTERM pid)
  pollGone 10
  where
    pollGone :: Int -> IO ()
    pollGone 0 = ignoreErr (signalProcess sigKILL pid)
    pollGone n = do
      alive <- isAlive pid
      when alive (threadDelay 100000 >> pollGone (n - 1))
    ignoreErr :: IO () -> IO ()
    ignoreErr = E.handle (\(_ :: E.SomeException) -> pure ())

isAlive :: ProcessID -> IO Bool
isAlive (CPid pid) = doesDirectoryExist ("/proc/" <> show pid)

-- | Find every dnsmasq process whose argv contains
-- @--interface=corvus-br@. Uses @pgrep -f -a@ for the search.
findCorvusDnsmasq :: IO [ProcessID]
findCorvusDnsmasq = do
  (code, out, _) <-
    readProcessWithExitCode
      "pgrep"
      ["-f", "-a", "dnsmasq.*--interface=corvus-br"]
      ""
  case code of
    ExitSuccess -> pure (parsePgrep out)
    -- pgrep exits 1 when there are no matches; that's the common case.
    ExitFailure _ -> pure []
  where
    parsePgrep raw =
      [ CPid pidN
      | line <- lines raw
      , (pidS : _) <- [words line]
      , Just pidN <- [readMaybe pidS]
      ]

-- ---------------------------------------------------------------------------
-- iproute2 + nftables shell-outs

-- | @nft delete table inet corvus_fw@. Right () if the table was
-- removed or already absent.
nftDropCorvusTable :: IO (Either T.Text ())
nftDropCorvusTable = do
  (code, _, err) <-
    readProcessWithExitCode "nft" ["delete", "table", "inet", "corvus_fw"] ""
  pure $ case code of
    ExitSuccess -> Right ()
    ExitFailure _
      | absent err -> Right ()
      | otherwise -> Left (T.strip (T.pack err))
  where
    absent err =
      "No such file" `T.isInfixOf` T.pack err
        || "does not exist" `T.isInfixOf` T.pack err

-- | List link names by prefix via @ip -o link show@. Tolerant
-- of `ip` failure (returns @[]@).
--
-- `ip -o link show` format (one per line):
--
-- @
-- 7: corvus-br0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 …
-- 8: corvus-tap0@if7: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 …
-- @
--
-- We split on ": " to grab the name field, then strip the
-- optional @\\@if\<idx\>@ suffix that's present on bridged TAPs.
ipLinksByPrefix :: T.Text -> IO [T.Text]
ipLinksByPrefix prefix = do
  (code, out, _) <- readProcessWithExitCode "ip" ["-o", "link", "show"] ""
  pure $ case code of
    ExitFailure _ -> []
    ExitSuccess ->
      [ name
      | line <- T.lines (T.pack out)
      , Just name <- [parseLinkName line]
      , prefix `T.isPrefixOf` name
      ]
  where
    parseLinkName line = case T.splitOn ": " line of
      (_ : nameField : _) -> Just (T.takeWhile (/= '@') nameField)
      _ -> Nothing

-- | @ip link del <name>@.
ipLinkDel :: T.Text -> IO (Either T.Text ())
ipLinkDel name = do
  (code, _, err) <-
    readProcessWithExitCode "ip" ["link", "del", T.unpack name] ""
  pure $ case code of
    ExitSuccess -> Right ()
    ExitFailure _ -> Left (T.strip (T.pack err))
