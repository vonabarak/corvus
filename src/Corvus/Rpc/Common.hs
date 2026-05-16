{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Shared helpers for the Cap'n Proto cap implementations:
-- 'EntityRef' resolution, a small "throw if Left" combinator, and a
-- connection-safe replacement for 'Capnp.Rpc.Server.handleParsed'.
module Corvus.Rpc.Common
  ( -- * EntityRef → Ref bridge
    capnpRefToRef

    -- * Failures
  , failOnLeft
  , failNotFound
  , throwWireError

    -- * Method-handler wrapper (connection-safe)
  , handleParsed
  )
where

import qualified Capnp.Classes as C
import qualified Capnp.Gen.Common as CGCommon
import qualified Capnp.Repr as R
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (MethodHandler)
import qualified Capnp.Rpc.Server as CapnpServer
import Control.Exception (SomeException, catch)
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

failNotFound :: Text -> IO a
failNotFound what = throwFailed (what <> " not found")

throwWireError :: WireError -> IO a
throwWireError = throwFailed . showWireError

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
