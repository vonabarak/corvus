{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | NatRule capability for `corvus-netd`.
--
-- One 'NatRuleCap' represents one masquerade rule in @inet
-- corvus_fw postrouting@. The rule's name in the ledger is the
-- string form of the kernel handle that nftables assigned at
-- insert time, so explicit destroy + cap-drop shutdown both find
-- the same entry and run the same idempotent @nft delete rule …
-- handle N@ on it.
--
-- The Cap'n Proto schema lets us drop the cap as the destroy
-- gesture; explicit 'destroy()' is provided too for daemons that
-- prefer the symmetric @install / destroy@ shape.
module Corvus.Netd.Caps.NatRule
  ( NatRuleCap (..)
  , newNatRuleCap
  , destroyNatRuleCap
  , orphanNatRuleCap
  )
where

import qualified Capnp.Gen.Netagent as CGN
import Capnp.Rpc.Server (SomeServer (..))
import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Monad.Logger (logInfoN, logWarnN, runStderrLoggingT)
import qualified Corvus.Netd.Ledger as L
import Corvus.Netd.Nftables (NftError (..))
import Corvus.Rpc.Common (handleParsed)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.Typeable (Typeable, cast)

-- | Server-side state for one NAT rule resource. The ledger entry
-- key uses the rule's kernel handle (as Text) so it's deterministic
-- and survives cap rebuilds across @claim@ paths.
data NatRuleCap = NatRuleCap
  { nrcOwner :: !T.Text
  , nrcRuleKey :: !T.Text
  -- ^ Ledger key for this rule — usually the kernel handle
  -- rendered as text. Opaque to callers.
  , nrcLedger :: !L.Ledger
  }
  deriving (Typeable)

newNatRuleCap :: T.Text -> T.Text -> L.Ledger -> IO NatRuleCap
newNatRuleCap owner ruleKey ledger =
  pure
    NatRuleCap
      { nrcOwner = owner
      , nrcRuleKey = ruleKey
      , nrcLedger = ledger
      }

-- | Tear down the rule (kernel + ledger). Idempotent via the
-- atomic ledger forget — losing the race makes this a no-op.
destroyNatRuleCap :: NatRuleCap -> IO ()
destroyNatRuleCap nrc = do
  mResource <-
    atomically $
      L.forget (nrcLedger nrc) (nrcOwner nrc) L.KNat (nrcRuleKey nrc)
  case mResource of
    Nothing -> pure ()
    Just r -> do
      result <- E.try @NftError (L.rTeardown r)
      case result of
        Right () -> pure ()
        Left e ->
          runStderrLoggingT $
            logWarnN
              ( "[netd] nat-rule teardown failed ("
                  <> nrcRuleKey nrc
                  <> "): "
                  <> neStderr e
              )

instance SomeServer NatRuleCap where
  shutdown nrc = do
    runStderrLoggingT $
      logInfoN ("[netd] nat-rule cap-drop → orphan: " <> nrcRuleKey nrc)
    orphanNatRuleCap nrc

  unwrap = cast

-- | Cap-drop path. Same orphan-window pattern as bridge/TAP.
orphanNatRuleCap :: NatRuleCap -> IO ()
orphanNatRuleCap nrc = do
  now <- getCurrentTime
  _ <-
    atomically $
      L.markOrphan
        (nrcLedger nrc)
        (nrcOwner nrc)
        L.KNat
        (nrcRuleKey nrc)
        now
  pure ()

instance CGN.NatRule'server_ NatRuleCap where
  natRule'destroy nrc =
    handleParsed $ \_ -> do
      destroyNatRuleCap nrc
      pure CGN.NatRule'destroy'results
