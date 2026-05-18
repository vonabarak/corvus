-- | Per-process resource ledgers for `corvus-netd`.
--
-- The agent is stateless across processes (Phase 2.5): nothing
-- persists to disk. These ledgers are the *in-memory* records
-- of what the agent's current process instance has applied.
-- Discarded on shutdown; rebuilt by the daemon's on-(re)connect
-- re-apply loop.
--
-- Two independent ledger types — one for networks, one for
-- TAPs. The agent's overall state holds an instance of each.
-- Polymorphic over the spec / live-state representations so
-- 'Corvus.Netd.Network' and 'Corvus.Netd.Tap' can pick their
-- own concrete types without dragging this module into their
-- import graphs.
module Corvus.Netd.Ledger
  ( -- * Network ledger
    NetworkLedger
  , newNetworkLedger
  , readNetworks
  , insertNetwork
  , removeNetwork

    -- * TAP ledger
  , TapLedger
  , newTapLedger
  , readTaps
  , insertTap
  , removeTap
  )
where

import Control.Concurrent.STM (STM, TVar, modifyTVar', newTVarIO, readTVar)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Networks

-- | Key = network name (e.g. @corvus-br0@). Value = the spec
-- last applied + whatever live kernel/process handles the
-- state machine allocated for it.
newtype NetworkLedger spec live = NetworkLedger
  { networkVar :: TVar (Map.Map T.Text (spec, live))
  }

newNetworkLedger :: IO (NetworkLedger spec live)
newNetworkLedger = NetworkLedger <$> newTVarIO Map.empty

readNetworks :: NetworkLedger spec live -> STM (Map.Map T.Text (spec, live))
readNetworks = readTVar . networkVar

insertNetwork
  :: NetworkLedger spec live -> T.Text -> spec -> live -> STM ()
insertNetwork l name spec live =
  modifyTVar' (networkVar l) (Map.insert name (spec, live))

removeNetwork :: NetworkLedger spec live -> T.Text -> STM ()
removeNetwork l name = modifyTVar' (networkVar l) (Map.delete name)

-- ---------------------------------------------------------------------------
-- TAPs

-- | Key = TAP name (e.g. @corvus-tap-vm42-eth0@). Value =
-- spec only — TAPs have no live handles beyond their kernel
-- link, which is identifiable by name.
newtype TapLedger spec = TapLedger
  { tapVar :: TVar (Map.Map T.Text spec)
  }

newTapLedger :: IO (TapLedger spec)
newTapLedger = TapLedger <$> newTVarIO Map.empty

readTaps :: TapLedger spec -> STM (Map.Map T.Text spec)
readTaps = readTVar . tapVar

insertTap :: TapLedger spec -> T.Text -> spec -> STM ()
insertTap l name spec = modifyTVar' (tapVar l) (Map.insert name spec)

removeTap :: TapLedger spec -> T.Text -> STM ()
removeTap l name = modifyTVar' (tapVar l) (Map.delete name)
