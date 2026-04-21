{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Allocate TCP ports for QEMU SPICE listeners.
--
-- Iterates the configured @qcSpicePortMin..qcSpicePortMax@ range, skips
-- any port already recorded against a VM in the database, and
-- test-binds each candidate to the configured SPICE bind address. The
-- first port that binds cleanly wins. A small TOCTOU window remains
-- between the test bind and QEMU opening the listener — QEMU fails
-- loudly on collision and 'handleVmStart' treats that as a start
-- error.
module Corvus.Qemu.SpicePort
  ( allocateSpicePort
  )
where

import Control.Exception (SomeException, bracket, try)
import Data.Maybe (mapMaybe)
import Data.Pool (Pool)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (Entity (..), selectList)
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend)

import qualified Corvus.Model as M
import Corvus.Qemu.Config (QemuConfig (..))
import Corvus.Types (ServerState (..))

import qualified Network.Socket as NS

-- | Allocate a free TCP port for SPICE. Returns 'Left' with an
-- operator-facing message when the configured range is exhausted, or
-- when no candidate port can be bound (for instance because the bind
-- address is bogus).
allocateSpicePort :: ServerState -> IO (Either Text Int)
allocateSpicePort state = do
  let cfg = ssQemuConfig state
      lo = qcSpicePortMin cfg
      hi = qcSpicePortMax cfg
      bindAddr = T.unpack (qcSpiceBindAddress cfg)
  reserved <- dbAllocatedPorts (ssDbPool state)
  let candidates = filter (\p -> not (Set.member p reserved)) [lo .. hi]
  tryPorts bindAddr candidates
  where
    tryPorts _ [] =
      pure $ Left "no free SPICE port in configured range"
    tryPorts bindAddr (p : ps) = do
      bindable <- canBind bindAddr p
      if bindable then pure (Right p) else tryPorts bindAddr ps

-- | Collect ports already assigned to other VMs. The daemon clears
-- @spicePort@ when a VM stops, so anything we read here is held by a
-- running VM (or a VM whose PID the startup-cleanup pass has not yet
-- reaped — either way, do not reuse).
dbAllocatedPorts :: Pool SqlBackend -> IO (Set.Set Int)
dbAllocatedPorts pool = do
  rows <- runSqlPool (selectList [] []) pool
  pure $ Set.fromList $ mapMaybe (M.vmSpicePort . entityVal) (rows :: [Entity M.Vm])

-- | Test-bind a TCP socket to @(addr, port)@ and close it again.
-- Returns 'True' when the bind succeeds, 'False' otherwise. Address
-- lookup failures are treated as non-bindable (rather than raising) so
-- the caller can just move on to the next candidate.
canBind :: String -> Int -> IO Bool
canBind addr port = do
  result <- try attempt :: IO (Either SomeException ())
  pure $ case result of
    Right () -> True
    Left _ -> False
  where
    attempt = do
      addrInfo <- resolve addr port
      bracket (NS.socket (NS.addrFamily addrInfo) NS.Stream NS.defaultProtocol) NS.close $ \sock -> do
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.bind sock (NS.addrAddress addrInfo)

    resolve host p = do
      let hints = NS.defaultHints {NS.addrFlags = [NS.AI_NUMERICHOST, NS.AI_NUMERICSERV], NS.addrSocketType = NS.Stream}
      infos <- NS.getAddrInfo (Just hints) (Just host) (Just (show p))
      case infos of
        (a : _) -> pure a
        [] -> ioError (userError "SPICE bind address did not resolve")
