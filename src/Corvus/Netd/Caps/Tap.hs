{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | TAP capability for `corvus-netd`.
--
-- One 'TapCap' represents one persistent TAP device in the host
-- network namespace. The agent creates it with @ip tuntap add ...
-- user <uid> group <gid>@, which makes the device reopen-friendly
-- for the daemon's uid: QEMU later opens @/dev/net/tun@ + issues
-- @TUNSETIFF@ on the named device without needing @CAP_NET_ADMIN@,
-- because @tun_chr_check_perm@ in the kernel permits reopen if the
-- caller matches the TAP's owner uid/gid.
--
-- Lifecycle mirrors 'Corvus.Netd.Caps.Bridge.BridgeCap':
--
--   1. 'Corvus.Netd.Caps.Session.createTap' creates the kernel
--      device, attaches it to the bridge, brings it up, and
--      records the teardown action in the ledger.
--   2. When the daemon drops the cap (or the connection closes),
--      'shutdown' fires and runs the teardown — same idempotent
--      ledger-atomic-yank path the bridge uses.
--   3. Teardown order: @ip link set nomaster@ (defensive — kernel
--      handles this implicitly on delete, but explicit detach
--      surfaces errors earlier) then @ip tuntap del@.
module Corvus.Netd.Caps.Tap
  ( TapCap (..)
  , newTapCap
  , destroyTapCap
  , tapInfo
  )
where

import qualified Capnp.Gen.Netagent as CGN
import Capnp.Rpc.Server (SomeServer (..))
import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Monad.Logger (logInfoN, logWarnN, runStderrLoggingT)
import Corvus.Netd.IpLink (IpLinkError (..))
import qualified Corvus.Netd.Ledger as L
import Corvus.Rpc.Common (handleParsed)
import qualified Data.Text as T
import Data.Typeable (Typeable, cast)
import Data.Word (Word32)

-- | Server-side state for one TAP resource. The teardown action
-- lives in the ledger under @(tcOwner, KTap, tcName)@.
data TapCap = TapCap
  { tcOwner :: !T.Text
  , tcName :: !T.Text
  , tcUid :: !Word32
  , tcGid :: !Word32
  , tcBridge :: !T.Text
  -- ^ Bridge this TAP was attached to at create time. Informational
  -- only — 'Bridge.attachTap' can move it later (Phase 2.x).
  , tcLedger :: !L.Ledger
  }
  deriving (Typeable)

newTapCap
  :: T.Text
  -- ^ owner tag
  -> T.Text
  -- ^ TAP ifname (already exists in the kernel)
  -> Word32
  -- ^ TUNSETOWNER uid
  -> Word32
  -- ^ TUNSETOWNER gid
  -> T.Text
  -- ^ bridge ifname
  -> L.Ledger
  -> IO TapCap
newTapCap owner name uid gid bridge ledger =
  pure
    TapCap
      { tcOwner = owner
      , tcName = name
      , tcUid = uid
      , tcGid = gid
      , tcBridge = bridge
      , tcLedger = ledger
      }

-- | Tear down the TAP (kernel + ledger). Idempotent via the
-- atomic ledger forget — losing the race makes this a no-op.
destroyTapCap :: TapCap -> IO ()
destroyTapCap tc = do
  mResource <-
    atomically $
      L.forget (tcLedger tc) (tcOwner tc) L.KTap (tcName tc)
  case mResource of
    Nothing -> pure ()
    Just r -> do
      result <- E.try @IpLinkError (L.rTeardown r)
      case result of
        Right () -> pure ()
        Left e ->
          runStderrLoggingT $
            logWarnN
              ( "[netd] tap teardown failed ("
                  <> tcName tc
                  <> "): "
                  <> ileStderr e
              )

-- | Canonical 'TapCap' → 'TapInfo' projection.
tapInfo :: TapCap -> CGN.Parsed CGN.TapInfo
tapInfo tc =
  CGN.TapInfo
    { CGN.name = tcName tc
    , CGN.bridge = tcBridge tc
    , CGN.uid = tcUid tc
    , CGN.gid = tcGid tc
    , CGN.owner = tcOwner tc
    }

-- ---------------------------------------------------------------------------
-- SomeServer / cap-method bindings
-- ---------------------------------------------------------------------------

instance SomeServer TapCap where
  shutdown tc = do
    runStderrLoggingT $ logInfoN ("[netd] tap shutdown: " <> tcName tc)
    destroyTapCap tc

  unwrap = cast

instance CGN.Tap'server_ TapCap where
  tap'info tc =
    handleParsed $ \_ ->
      pure CGN.Tap'info'results {CGN.info = tapInfo tc}

  tap'destroy tc =
    handleParsed $ \_ -> do
      destroyTapCap tc
      pure CGN.Tap'destroy'results
