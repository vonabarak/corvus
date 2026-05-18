{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | dnsmasq handle capability for `corvus-netd`.
--
-- One 'DnsmasqHandleCap' tracks one running dnsmasq child. The
-- ledger entry's @rTeardown@ does the SIGTERM-and-wait via
-- 'Corvus.Netd.Dnsmasq.stopDnsmasq'. Cap drop and explicit
-- `stop()` both feed through 'destroyDnsmasqHandleCap', whose
-- atomic ledger-yank guarantees a single teardown per child.
module Corvus.Netd.Caps.DnsmasqHandle
  ( DnsmasqHandleCap (..)
  , newDnsmasqHandleCap
  , destroyDnsmasqHandleCap
  , orphanDnsmasqHandleCap
  )
where

import qualified Capnp.Gen.Netagent as CGN
import Capnp.Rpc.Server (SomeServer (..))
import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Monad.Logger (logInfoN, logWarnN, runStderrLoggingT)
import qualified Corvus.Netd.Ledger as L
import Corvus.Rpc.Common (handleParsed)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Typeable (Typeable, cast)
import Data.Word (Word32)
import System.Posix.Types (CPid (..))

-- | Server-side state for one dnsmasq supervisor instance.
data DnsmasqHandleCap = DnsmasqHandleCap
  { dhcOwner :: !T.Text
  , dhcName :: !T.Text
  -- ^ Ledger key — usually @"dnsmasq-<bridge>"@. Opaque to
  -- callers; surfaced only in logs.
  , dhcPid :: !Word32
  , dhcLedger :: !L.Ledger
  }
  deriving (Typeable)

newDnsmasqHandleCap
  :: T.Text -> T.Text -> CPid -> L.Ledger -> IO DnsmasqHandleCap
newDnsmasqHandleCap owner name (CPid pid) ledger =
  pure
    DnsmasqHandleCap
      { dhcOwner = owner
      , dhcName = name
      , dhcPid = fromIntegral pid
      , dhcLedger = ledger
      }

-- | Tear down the dnsmasq supervisor. Idempotent via the atomic
-- ledger forget.
destroyDnsmasqHandleCap :: DnsmasqHandleCap -> IO ()
destroyDnsmasqHandleCap dhc = do
  mResource <-
    atomically $
      L.forget (dhcLedger dhc) (dhcOwner dhc) L.KDnsmasq (dhcName dhc)
  case mResource of
    Nothing -> pure ()
    Just r -> do
      result <- E.try @E.SomeException (L.rTeardown r)
      case result of
        Right () -> pure ()
        Left e ->
          runStderrLoggingT $
            logWarnN
              ( "[netd] dnsmasq teardown failed ("
                  <> dhcName dhc
                  <> "): "
                  <> T.pack (show e)
              )

instance SomeServer DnsmasqHandleCap where
  shutdown dhc = do
    runStderrLoggingT $
      logInfoN ("[netd] dnsmasq cap-drop → orphan: " <> dhcName dhc)
    orphanDnsmasqHandleCap dhc

  unwrap = cast

-- | Cap-drop path. Same orphan-window pattern as the other caps.
-- A reconnecting daemon can't re-claim dnsmasq (no claimDnsmasq
-- method in v1), but the orphan window still gives the daemon
-- the chance to call 'stop()' via a fresh session before the
-- sweeper reaps the process.
orphanDnsmasqHandleCap :: DnsmasqHandleCap -> IO ()
orphanDnsmasqHandleCap dhc = do
  now <- getCurrentTime
  _ <-
    atomically $
      L.markOrphan (dhcLedger dhc) (dhcOwner dhc) L.KDnsmasq (dhcName dhc) now
  pure ()

instance CGN.DnsmasqHandle'server_ DnsmasqHandleCap where
  dnsmasqHandle'pid dhc =
    handleParsed $ \_ ->
      pure CGN.DnsmasqHandle'pid'results {CGN.pid = dhcPid dhc}

  dnsmasqHandle'stop dhc =
    handleParsed $ \_ -> do
      destroyDnsmasqHandleCap dhc
      pure CGN.DnsmasqHandle'stop'results
