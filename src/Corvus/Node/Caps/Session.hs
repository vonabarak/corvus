{-# LANGUAGE ScopedTypeVariables #-}

-- | Per-owner session capability for `corvus-nodeagent`.
--
-- Phase 1 ships liveness-only: a single `ping` so the daemon's
-- reconnect loop can prove the end-to-end path
-- (daemon → agent → Session) before declaring the connection
-- healthy. Subsequent phases extend this with disk / VM / console
-- / status-subscribe methods.
module Corvus.Node.Caps.Session
  ( SessionCap (..)
  , newSessionCap
  )
where

import qualified Capnp.Gen.Nodeagent as CGNA
import Capnp.Rpc.Server (SomeServer)
import Corvus.Rpc.Common (handleParsed)
import qualified Data.Text as T

-- | Session state. The owner tag is kept for logs; later phases
-- may extend this struct with ledger references when disk and VM
-- operations land.
newtype SessionCap = SessionCap
  { scOwner :: T.Text
  }

newSessionCap :: T.Text -> IO SessionCap
newSessionCap owner = pure SessionCap {scOwner = owner}

instance SomeServer SessionCap

instance CGNA.Session'server_ SessionCap where
  session'ping _ =
    handleParsed $ \_ ->
      pure CGNA.Session'ping'results
