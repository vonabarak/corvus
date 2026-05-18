{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Bridge capability for `corvus-netd`.
--
-- One 'BridgeCap' instance represents one Linux bridge in the host
-- network namespace. Lifecycle:
--
--   1. 'Corvus.Netd.Caps.Session.createBridge' creates the kernel
--      object (@ip link add ...@), records a ledger entry whose
--      'rTeardown' is the corresponding @ip link del@, builds this
--      cap, and exports it via the supervisor.
--   2. The Cap'n Proto layer hands the exported client to the
--      daemon. The daemon can call 'info', 'destroy'.
--   3. When the daemon drops the cap (or the connection closes),
--      'Capnp.Rpc.Server.SomeServer.shutdown' fires, which runs the
--      same teardown the explicit 'destroy' method runs.
--   4. Teardown is idempotent: the ledger entry is removed
--      transactionally, so the second caller's `forget` returns
--      'Nothing' and the @ip link del@ is skipped.
module Corvus.Netd.Caps.Bridge
  ( BridgeCap (..)
  , newBridgeCap
  , destroyBridgeCap
  , bridgeInfo
  )
where

import qualified Capnp.Gen.Netagent as CGN
import Capnp.Rpc.Server (SomeServer (..))
import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN, logWarnN, runStderrLoggingT)
import Corvus.Netd.IpLink (IpLinkError (..))
import qualified Corvus.Netd.Ledger as L
import Corvus.Rpc.Common (handleParsed)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Typeable (Typeable, cast)
import Data.Word (Word32)

-- | Server-side state for one bridge resource.
--
-- The teardown action lives in the ledger entry under
-- @(bcOwner, KBridge, bcName)@. Removing the ledger entry IS what
-- makes destroy idempotent.
data BridgeCap = BridgeCap
  { bcOwner :: !T.Text
  , bcName :: !T.Text
  , bcCidr :: !T.Text
  , bcMtu :: !Word32
  , bcCreated :: !UTCTime
  , bcLedger :: !L.Ledger
  }
  deriving (Typeable)

newBridgeCap
  :: T.Text
  -- ^ owner tag
  -> T.Text
  -- ^ bridge name (must already match the kernel link)
  -> T.Text
  -- ^ CIDR, or empty for L2-only
  -> Word32
  -- ^ MTU
  -> UTCTime
  -- ^ creation time
  -> L.Ledger
  -> IO BridgeCap
newBridgeCap owner name cidr mtu created ledger =
  pure
    BridgeCap
      { bcOwner = owner
      , bcName = name
      , bcCidr = cidr
      , bcMtu = mtu
      , bcCreated = created
      , bcLedger = ledger
      }

-- | Tear down the bridge (kernel + ledger) if it's still recorded.
-- Idempotent: a second call after success is a no-op. Logs and
-- swallows 'IpLinkError' so the cap shutdown path can't crash the
-- agent on a stale @ip link del@.
destroyBridgeCap :: BridgeCap -> IO ()
destroyBridgeCap bc = do
  mResource <-
    atomically $
      L.forget (bcLedger bc) (bcOwner bc) L.KBridge (bcName bc)
  case mResource of
    Nothing -> pure ()
    Just r -> do
      result <- E.try @IpLinkError (L.rTeardown r)
      case result of
        Right () -> pure ()
        Left e ->
          runStderrLoggingT $
            logWarnN
              ( "[netd] bridge teardown failed ("
                  <> bcName bc
                  <> "): "
                  <> ileStderr e
              )

-- | Canonical 'BridgeCap' → 'BridgeInfo' projection. Kept here
-- rather than in 'Corvus.Netd.Caps.Session' so a future
-- @claimBridge@ that reconstructs caps from ledger entries can
-- reuse it.
bridgeInfo :: BridgeCap -> CGN.Parsed CGN.BridgeInfo
bridgeInfo bc =
  CGN.BridgeInfo
    { CGN.name = bcName bc
    , CGN.cidr = bcCidr bc
    , CGN.mtu = bcMtu bc
    , CGN.upState = "up" -- Phase 2 always brings the bridge up at create time.
    , CGN.owner = bcOwner bc
    , CGN.tapCount = 0 -- Phase 2.x: count attached TAPs via ledger query.
    }

-- ---------------------------------------------------------------------------
-- SomeServer / cap-method bindings
-- ---------------------------------------------------------------------------

instance SomeServer BridgeCap where
  shutdown bc = do
    runStderrLoggingT $ logInfoN ("[netd] bridge shutdown: " <> bcName bc)
    destroyBridgeCap bc

  -- Make this cap recoverable via 'Capnp.Rpc.unwrapServer' so
  -- cross-cap references (e.g. TapParams.bridge :Bridge) can pull
  -- the underlying BridgeCap out of the wire cap.
  unwrap = cast

instance CGN.Bridge'server_ BridgeCap where
  bridge'info bc =
    handleParsed $ \_ ->
      pure CGN.Bridge'info'results {CGN.info = bridgeInfo bc}

  bridge'destroy bc =
    handleParsed $ \_ -> do
      destroyBridgeCap bc
      pure CGN.Bridge'destroy'results

-- attachTap / detachTap land in the next Phase 2 slice (TAP cap).
-- Until then they remain at the generated `methodUnimplemented`
-- default, which is sufficient for Phase 2's first integration
-- test.
