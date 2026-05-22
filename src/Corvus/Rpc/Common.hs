{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Shared helpers for the Cap'n Proto cap implementations:
-- 'EntityRef' resolution, a small "throw if Left" combinator, and a
-- connection-safe replacement for 'Capnp.Rpc.Server.handleParsed'.
module Corvus.Rpc.Common
  ( -- * EntityRef → Ref bridge
    capnpRefToRef

    -- * Failures
  , failOnLeft

    -- * Method-handler wrappers (connection-safe)
  , handleParsed
  , handleParsedAsync
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Common as CGCommon
import qualified Capnp.Repr as R
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (MethodHandler)
import qualified Capnp.Rpc.Server as CapnpServer
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import qualified Corvus.Protocol as P
import Corvus.Wire.Common (EntityRef (..), fromCapnpEntityRef)
import Corvus.Wire.Errors (WireError, showWireError)
import Data.Text (Text)
import qualified Data.Text as T

-- | Translate the wire-level 'EntityRef' union into the existing
-- text-based 'Corvus.Protocol.Ref' that 'Corvus.Handlers.Resolve.*'
-- understands. Numeric ids become @show id@; names pass through.
capnpRefToRef :: C.Parsed CGCommon.EntityRef -> IO P.Ref
capnpRefToRef raw = case fromCapnpEntityRef raw of
  Right (RefById n) -> pure $ P.Ref (T.pack (show n))
  Right (RefByName t) -> pure $ P.Ref t
  Left e -> throwFailed (showWireError e)

failOnLeft :: Either Text a -> IO a
failOnLeft (Right a) = pure a
failOnLeft (Left e) = throwFailed e

-- | Connection-safe replacement for 'Capnp.Rpc.Server.handleParsed'.
--
-- The upstream helper wraps the user handler in 'propagateExceptions',
-- which calls 'breakPromise' on exception (so the client gets a
-- structured error) **and then re-raises**. The re-raised exception
-- kills the RPC server's run loop; @runConn@'s @concurrently_@
-- promptly tears down the connection's other loops, and the client
-- sees the next RPC as @Peer disconnected@. The net effect:
-- @throwFailed "<some user-facing error>"@ from any handler kills
-- the entire connection.
--
-- This wrapper delegates parsing + the @breakPromise@ to the upstream
-- 'CapnpServer.handleParsed' (so the client still gets the error)
-- and then swallows the re-raise so the RPC loop keeps running.
-- Asynchronous exceptions are caught too — this is intentional: an
-- async cancel of an in-flight RPC should not also abort the
-- connection. Whatever cancels us also drives the connection
-- teardown through other channels.
handleParsed
  :: ( C.Parse p pp
     , R.IsStruct p
     , C.Parse r pr
     , R.IsStruct r
     )
  => (pp -> IO pr)
  -> MethodHandler p r
handleParsed handler param fulfiller =
  CapnpServer.handleParsed handler param fulfiller
    `catch` \(_ :: SomeException) -> pure ()

-- | Asynchronous variant of 'handleParsed': forks the handler in
-- a fresh 'forkIO' so the calling 'Capnp.Rpc.Server.runServer'
-- loop returns to the dispatch queue immediately and the next
-- RPC on the same exported cap can start running.
--
-- 'runServer' processes one 'CallInfo' to completion before
-- pulling the next one — fine for fast handlers, but a hard
-- single-threaded bottleneck for anything that blocks (a
-- multi-minute @vmGuestExec@ during a build, for example): every
-- subsequent call on the agent's session sits in the queue until
-- the slow one returns. Forking the handler restores the
-- expected "calls proceed independently" semantics.
--
-- 'Capnp.Rpc.Promise.Fulfiller' is STM-backed and thread-safe,
-- so calling 'fulfill' / 'breakPromise' from the forked thread
-- is well-defined. Exceptions inside the handler still flow
-- through upstream 'propagateExceptions' → the promise is
-- broken with a structured RPC error before the catch swallows
-- the re-raise (same rationale as 'handleParsed' — keep the
-- connection alive).
--
-- Use this sparingly: only for handlers that legitimately block
-- on something outside the agent's control (guest-side processes,
-- long file I/O, etc.). Forking every handler would multiply the
-- thread count and complicate per-VM serialization invariants
-- that some handlers rely on.
handleParsedAsync
  :: ( C.Parse p pp
     , R.IsStruct p
     , C.Parse r pr
     , R.IsStruct r
     )
  => (pp -> IO pr)
  -> MethodHandler p r
handleParsedAsync handler param fulfiller =
  void $
    forkIO $
      CapnpServer.handleParsed handler param fulfiller
        `catch` \(_ :: SomeException) -> pure ()
