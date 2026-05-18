-- | Per-owner resource ledger for `corvus-netd`.
--
-- The agent owns one 'Ledger' shared across all connections. Every
-- resource the agent creates on behalf of a client — bridge, TAP,
-- NAT rule, dnsmasq supervisor — is recorded here with a teardown
-- action that the agent uses to free the kernel state when the
-- resource is destroyed.
--
-- Why a shared map and not per-connection state? Two reasons:
--
--   * `claimBridge` / `claimTap` re-adopt resources after a daemon
--     restart. The new connection hasn't created the resource —
--     it's looking it up by name across the agent's whole view.
--   * `listBridges` returns every bridge the calling owner owns,
--     not just the ones the current connection holds caps for.
--
-- Lookup keys are @(owner, kind, name)@. Resources from different
-- owners with the same name are independent (different keys), so
-- two unprivileged users can each have a `default` bridge without
-- colliding. Within an owner, names are unique per kind.
--
-- The ledger doesn't do persistence yet. Phase 2.5 / Phase 3 may
-- add a `state.json` cache + rtnetlink-driven reconciliation;
-- until then, an agent restart loses the in-memory view and
-- existing kernel-state resources become unmanaged (a `claim` call
-- would fail because the entry's gone).
module Corvus.Netd.Ledger
  ( Ledger
  , Kind (..)
  , Resource (..)
  , newLedger

    -- * Operations
  , record
  , forget
  , Corvus.Netd.Ledger.lookup
  , list
  , listAll
  )
where

import Control.Concurrent.STM (STM, TVar, modifyTVar', newTVarIO, readTVar)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime)

-- | Resource kinds the ledger distinguishes. Stored as the second
-- component of the lookup key, so a bridge named "default" doesn't
-- collide with a TAP named "default".
data Kind
  = KBridge
  | KTap
  | KNat
  | KDnsmasq
  deriving (Eq, Ord, Show)

-- | One resource entry.
--
-- 'rTeardown' is the side-effecting action the agent invokes to
-- free the underlying kernel state (e.g. @ip link del br0@,
-- @kill dnsmasq@). It MUST be idempotent — both the cap's explicit
-- @destroy@ method and 'Capnp.Rpc.Server.SomeServer.shutdown'
-- (which fires on cap drop) can race to call it, and a future
-- rtnetlink-driven cleanup path adds a third caller.
data Resource = Resource
  { rOwner :: !T.Text
  , rKind :: !Kind
  , rName :: !T.Text
  , rCreated :: !UTCTime
  , rTeardown :: !(IO ())
  }

-- | Ledger handle. The internal Map is keyed by @(owner, kind, name)@.
newtype Ledger = Ledger (TVar (Map.Map LedgerKey Resource))

type LedgerKey = (T.Text, Kind, T.Text)

newLedger :: IO Ledger
newLedger = Ledger <$> newTVarIO Map.empty

-- | Insert a resource. Caller is responsible for ensuring the
-- key isn't already taken — the agent enforces "name unique per
-- (owner, kind)" higher up by calling 'Corvus.Netd.Ledger.lookup'
-- before issuing the kernel mutation.
record :: Ledger -> Resource -> STM ()
record (Ledger var) r =
  modifyTVar' var (Map.insert (rOwner r, rKind r, rName r) r)

-- | Remove a resource; returns the prior entry (if any) so the
-- caller can run the teardown action.
forget :: Ledger -> T.Text -> Kind -> T.Text -> STM (Maybe Resource)
forget (Ledger var) owner kind name = do
  m <- readTVar var
  let key = (owner, kind, name)
  case Map.lookup key m of
    Nothing -> pure Nothing
    Just r -> do
      modifyTVar' var (Map.delete key)
      pure (Just r)

-- | Look up a resource without removing it.
lookup :: Ledger -> T.Text -> Kind -> T.Text -> STM (Maybe Resource)
lookup (Ledger var) owner kind name =
  Map.lookup (owner, kind, name) <$> readTVar var

-- | List every resource of a given kind owned by `owner`.
list :: Ledger -> T.Text -> Kind -> STM [Resource]
list (Ledger var) owner kind = do
  m <- readTVar var
  pure
    [ r
    | ((o, k, _), r) <- Map.toList m
    , o == owner
    , k == kind
    ]

-- | List every resource in the ledger, ignoring owner/kind. Used
-- by agent-side housekeeping (drift reconciliation) — never by a
-- session-scoped method.
listAll :: Ledger -> STM [Resource]
listAll (Ledger var) = Map.elems <$> readTVar var
