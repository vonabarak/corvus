{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Per-owner session capability for `corvus-netd`.
--
-- A 'SessionCap' is returned by @NetAgent.session@ and scopes every
-- subsequent privileged operation to a single owner tag. Phase 1:
-- the cap exists, every privileged method throws @methodUnimplemented@
-- (the generated bindings' default), so we override no methods on
-- purpose — the goal here is end-to-end wire shape, not behavior.
-- Phase 2 will fill in the real implementations.
module Corvus.Netd.Caps.Session
  ( SessionCap (..)
  , newSessionCap
  )
where

import qualified Capnp.Gen.Netagent as CGN
import Capnp.Rpc.Server (SomeServer)
import qualified Data.Text as T
import Supervisors (Supervisor)

-- | Session state: just the owner tag and the supervisor used to
-- export sub-caps. The kernel-state-touching fields (ledger,
-- watchdog handle, etc.) land here in Phase 2.
data SessionCap = SessionCap
  { scOwner :: !T.Text
  , scSup :: !Supervisor
  }

newSessionCap :: T.Text -> Supervisor -> IO SessionCap
newSessionCap owner sup = pure (SessionCap owner sup)

instance SomeServer SessionCap

-- | Default methods come from the generated class: every override
-- uses @methodUnimplemented@. That's fine for Phase 1 — a client
-- that calls @createBridge@ on the stub will get a clean RPC error
-- back, which is exactly what the smoke test asserts against.
instance CGN.Session'server_ SessionCap
