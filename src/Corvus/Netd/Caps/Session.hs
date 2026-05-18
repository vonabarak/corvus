{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Per-owner session capability for `corvus-netd`.
--
-- A 'SessionCap' is returned by @NetAgent.session@ and scopes every
-- subsequent privileged operation to a single owner tag. Phase 2
-- implements:
--
--   * 'createBridge' — @ip link add@ / @ip addr add@ / @ip link set
--     up@ chain, plus a ledger entry whose teardown deletes the
--     bridge. Returns a real 'BridgeCap'.
--   * 'listBridges'  — ledger query, scoped to this session's owner.
--   * 'claimBridge'  — re-adopt an existing bridge by name.
--   * 'setIpForwarding' — typed sysctl, root-only.
--
-- The rest (TAP, NAT, dnsmasq, event subscription) remain as
-- generated `methodUnimplemented` defaults until the next slices.
module Corvus.Netd.Caps.Session
  ( SessionCap (..)
  , newSessionCap
  )
where

import Capnp (export)
import qualified Capnp.Gen.Netagent as CGN
import Capnp.Rpc (throwFailed, unwrapServer)
import Capnp.Rpc.Server (SomeServer)
import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Monad (unless, void, when)
import Corvus.Netd.Caps.Bridge (BridgeCap (..), newBridgeCap)
import Corvus.Netd.Caps.DnsmasqHandle (newDnsmasqHandleCap)
import Corvus.Netd.Caps.NatRule (newNatRuleCap)
import Corvus.Netd.Caps.Tap (newTapCap)
import qualified Corvus.Netd.Dnsmasq as Dn
import Corvus.Netd.IpLink
  ( IpLinkError (..)
  , addrAdd
  , bridgeAdd
  , bridgeDel
  , linkSetMaster
  , linkSetUp
  , tapAdd
  , tapDel
  )
import qualified Corvus.Netd.Ledger as L
import qualified Corvus.Netd.Nftables as Nft
import qualified Corvus.Netd.Sysctl as Sys
import Corvus.Rpc.Common (handleParsed)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Supervisors (Supervisor)

-- | Session state.  The supervisor exports sub-caps (Bridge, Tap,
-- …) and is per-connection — see 'Corvus.Netd.Server' — so dropping
-- the connection cleanly drops every cap the session handed out.
data SessionCap = SessionCap
  { scOwner :: !T.Text
  , scSup :: !Supervisor
  , scLedger :: !L.Ledger
  }

newSessionCap :: T.Text -> Supervisor -> L.Ledger -> IO SessionCap
newSessionCap owner sup ledger =
  pure
    SessionCap
      { scOwner = owner
      , scSup = sup
      , scLedger = ledger
      }

instance SomeServer SessionCap

instance CGN.Session'server_ SessionCap where
  -- ----- createBridge -----------------------------------------------------
  session'createBridge sess =
    handleParsed $ \CGN.Session'createBridge'params {CGN.params = p} -> do
      let CGN.BridgeParams {CGN.name = name, CGN.cidr = cidr, CGN.mtu = mtu} = p
      when (T.null name) $ throwFailed "createBridge: name must be non-empty"
      -- Reject duplicate within this owner.
      existing <-
        atomically $
          L.lookup (scLedger sess) (scOwner sess) L.KBridge name
      case existing of
        Just _ ->
          throwFailed $
            "createBridge: bridge already exists for owner: " <> name
        Nothing -> pure ()
      created <- getCurrentTime
      -- Kernel chain. If addrAdd or linkSetUp fails, roll back the
      -- bridge link so we don't leak a half-configured interface.
      bridgeAdd name >>= failOnIpErr "createBridge:link-add"
      let rollback = void (bridgeDel name)
      let runOrRollback step label =
            step >>= \case
              Right () -> pure ()
              Left e -> do
                rollback
                throwFailed (label <> ": " <> ileStderr e)
      unless (T.null cidr) $
        runOrRollback (addrAdd cidr name) "createBridge:addr-add"
      runOrRollback (linkSetUp name) "createBridge:link-up"
      -- Record in the ledger. Teardown removes the bridge link.
      let teardown = do
            res <- bridgeDel name
            case res of
              Right () -> pure ()
              Left e -> E.throwIO e
      atomically $
        L.record
          (scLedger sess)
          L.Resource
            { L.rOwner = scOwner sess
            , L.rKind = L.KBridge
            , L.rName = name
            , L.rCreated = created
            , L.rTeardown = teardown
            }
      cap <-
        newBridgeCap
          (scOwner sess)
          name
          cidr
          mtu
          created
          (scLedger sess)
      client <- export @CGN.Bridge (scSup sess) cap
      pure CGN.Session'createBridge'results {CGN.bridge = client}

  -- ----- listBridges ------------------------------------------------------
  session'listBridges sess =
    handleParsed $ \_ -> do
      rs <- atomically $ L.list (scLedger sess) (scOwner sess) L.KBridge
      -- We don't have the BridgeCaps in the ledger entries (the
      -- entries only carry teardown closures). For listBridges we
      -- only need BridgeInfo, and we have enough metadata in the
      -- Resource (name, owner, created) plus reading cidr/mtu from
      -- /sys would be needed for full fidelity. Phase 2 punts: we
      -- return name+owner and leave the rest blank, which is what
      -- the daemon needs for its `crv network list` rendering.
      let toInfo r =
            CGN.BridgeInfo
              { CGN.name = L.rName r
              , CGN.cidr = ""
              , CGN.mtu = 0
              , CGN.upState = "up"
              , CGN.owner = L.rOwner r
              , CGN.tapCount = 0
              }
      pure CGN.Session'listBridges'results {CGN.bridges = map toInfo rs}

  -- ----- claimBridge ------------------------------------------------------
  session'claimBridge sess =
    handleParsed $ \CGN.Session'claimBridge'params {CGN.name = name} -> do
      mExisting <-
        atomically $
          L.lookup (scLedger sess) (scOwner sess) L.KBridge name
      case mExisting of
        Nothing ->
          throwFailed $ "claimBridge: no bridge named " <> name <> " for this owner"
        Just r -> do
          -- Rebuild a fresh BridgeCap pointing at the same ledger
          -- entry. CIDR/MTU lost to the reconstruction (the ledger
          -- doesn't preserve them today); a Phase 2.5 widening of
          -- Resource carries them along.
          cap <-
            newBridgeCap
              (scOwner sess)
              name
              ""
              0
              (L.rCreated r)
              (scLedger sess)
          client <- export @CGN.Bridge (scSup sess) cap
          pure CGN.Session'claimBridge'results {CGN.bridge = client}

  -- ----- createTap --------------------------------------------------------
  session'createTap sess =
    handleParsed $ \CGN.Session'createTap'params {CGN.params = p} -> do
      let CGN.TapParams
            { CGN.name = name
            , CGN.bridge = bridgeClient
            , CGN.uid = uid
            , CGN.gid = gid
            } = p
      when (T.null name) $ throwFailed "createTap: name must be non-empty"
      -- Recover the local BridgeCap from the wire cap. A remote
      -- cap or a wrong-agent cap returns Nothing and we refuse —
      -- TAP creation requires the bridge to live on THIS agent.
      bc <- case unwrapServer bridgeClient :: Maybe BridgeCap of
        Just bc -> pure bc
        Nothing ->
          throwFailed "createTap: bridge cap is not local to this agent"
      -- Reject duplicate within this owner.
      existing <-
        atomically $
          L.lookup (scLedger sess) (scOwner sess) L.KTap name
      case existing of
        Just _ ->
          throwFailed $ "createTap: tap already exists for owner: " <> name
        Nothing -> pure ()
      created <- getCurrentTime
      -- Kernel chain: create TAP, attach to bridge, bring up.
      tapAdd name uid gid >>= failOnIpErr "createTap:tuntap-add"
      let rollbackTap = void (tapDel name)
      let runOrRollback step label =
            step >>= \case
              Right () -> pure ()
              Left e -> do
                rollbackTap
                throwFailed (label <> ": " <> ileStderr e)
      runOrRollback
        (linkSetMaster name (bcName bc))
        "createTap:link-set-master"
      runOrRollback (linkSetUp name) "createTap:link-up"
      let teardown = do
            res <- tapDel name
            case res of
              Right () -> pure ()
              Left e -> E.throwIO e
      atomically $
        L.record
          (scLedger sess)
          L.Resource
            { L.rOwner = scOwner sess
            , L.rKind = L.KTap
            , L.rName = name
            , L.rCreated = created
            , L.rTeardown = teardown
            }
      cap <-
        newTapCap
          (scOwner sess)
          name
          uid
          gid
          (bcName bc)
          (scLedger sess)
      client <- export @CGN.Tap (scSup sess) cap
      pure CGN.Session'createTap'results {CGN.tap = client}

  -- ----- claimTap ---------------------------------------------------------
  session'claimTap sess =
    handleParsed $ \CGN.Session'claimTap'params {CGN.name = name} -> do
      mExisting <-
        atomically $
          L.lookup (scLedger sess) (scOwner sess) L.KTap name
      case mExisting of
        Nothing ->
          throwFailed $ "claimTap: no tap named " <> name <> " for this owner"
        Just _ -> do
          -- Phase 2's ledger doesn't preserve uid/gid/bridge so we
          -- return a cap with zeros; a Phase 2.5 widening of
          -- Resource carries the metadata along (mirrors the
          -- equivalent gap in claimBridge).
          cap <- newTapCap (scOwner sess) name 0 0 "" (scLedger sess)
          client <- export @CGN.Tap (scSup sess) cap
          pure CGN.Session'claimTap'results {CGN.tap = client}

  -- ----- installNat -------------------------------------------------------
  session'installNat sess =
    handleParsed $ \CGN.Session'installNat'params {CGN.params = p} -> do
      let CGN.NatParams
            { CGN.bridge = _bridgeClient
            , CGN.uplinkIf = uplinkIf
            , CGN.subnet = subnet
            } = p
      -- Phase 2 doesn't actually need the bridge cap for NAT — the
      -- masquerade rule keys off subnet + uplink only. We accept
      -- the cap argument (per the schema) but ignore it. A later
      -- slice that wires bridge-aware FORWARD policy will use it.
      when (T.null subnet) $ throwFailed "installNat: subnet must be non-empty"
      -- Idempotent base-table install. Cheap if it already exists.
      Nft.ensureBaseTable >>= \case
        Right () -> pure ()
        Left e -> throwFailed ("installNat:ensure-table: " <> Nft.neStderr e)
      created <- getCurrentTime
      handle <-
        Nft.addMasquerade subnet uplinkIf >>= \case
          Right h -> pure h
          Left e -> throwFailed ("installNat:add-rule: " <> Nft.neStderr e)
      let ruleKey = T.pack (show handle)
          teardown = do
            res <- Nft.deleteRule handle
            case res of
              Right () -> pure ()
              Left e -> E.throwIO e
      atomically $
        L.record
          (scLedger sess)
          L.Resource
            { L.rOwner = scOwner sess
            , L.rKind = L.KNat
            , L.rName = ruleKey
            , L.rCreated = created
            , L.rTeardown = teardown
            }
      cap <- newNatRuleCap (scOwner sess) ruleKey (scLedger sess)
      client <- export @CGN.NatRule (scSup sess) cap
      pure CGN.Session'installNat'results {CGN.nat = client}

  -- ----- startDnsmasq -----------------------------------------------------
  session'startDnsmasq sess =
    handleParsed $ \CGN.Session'startDnsmasq'params {CGN.params = p} -> do
      let CGN.DnsmasqParams
            { CGN.bridge = bridgeClient
            , CGN.listenAddr = listenAddr
            , CGN.dhcpRange = dhcpRange
            , CGN.domain = domain
            , CGN.extraArgs = extraArgs
            } = p
      bc <- case unwrapServer bridgeClient :: Maybe BridgeCap of
        Just bc -> pure bc
        Nothing ->
          throwFailed "startDnsmasq: bridge cap is not local to this agent"
      when (T.null listenAddr) $
        throwFailed "startDnsmasq: listenAddr must be non-empty"
      let bridgeName = bcName bc
          ledgerName = "dnsmasq-" <> bridgeName
      existing <-
        atomically $
          L.lookup (scLedger sess) (scOwner sess) L.KDnsmasq ledgerName
      case existing of
        Just _ ->
          throwFailed $
            "startDnsmasq: dnsmasq already running for bridge: "
              <> bridgeName
        Nothing -> pure ()
      created <- getCurrentTime
      let startParams =
            Dn.DnsmasqStartParams
              { Dn.dspBridge = bridgeName
              , Dn.dspListenAddr = listenAddr
              , Dn.dspDhcpRange = dhcpRange
              , Dn.dspDomain = domain
              , Dn.dspExtraArgs = extraArgs
              }
      dn <-
        Dn.startDnsmasq startParams >>= \case
          Right dn -> pure dn
          Left e ->
            throwFailed ("startDnsmasq: " <> T.pack (show e))
      let teardown = Dn.stopDnsmasq dn
      atomically $
        L.record
          (scLedger sess)
          L.Resource
            { L.rOwner = scOwner sess
            , L.rKind = L.KDnsmasq
            , L.rName = ledgerName
            , L.rCreated = created
            , L.rTeardown = teardown
            }
      cap <-
        newDnsmasqHandleCap
          (scOwner sess)
          ledgerName
          (Dn.dpPid dn)
          (scLedger sess)
      client <- export @CGN.DnsmasqHandle (scSup sess) cap
      pure CGN.Session'startDnsmasq'results {CGN.server = client}

  -- ----- setIpForwarding --------------------------------------------------
  session'setIpForwarding _ =
    handleParsed $ \CGN.Session'setIpForwarding'params {CGN.enabled = enabled, CGN.family_ = family_} -> do
      let fam = case family_ of
            CGN.NetFamily'v4 -> Sys.V4
            CGN.NetFamily'v6 -> Sys.V6
            CGN.NetFamily'unknown' _ -> Sys.V4
      result <- E.try @IOError (Sys.setIpForwarding enabled fam)
      case result of
        Right () -> pure CGN.Session'setIpForwarding'results
        Left e -> throwFailed $ "setIpForwarding: " <> T.pack (show e)

-- ---------------------------------------------------------------------------
-- Internal

failOnIpErr :: T.Text -> Either IpLinkError () -> IO ()
failOnIpErr label = \case
  Right () -> pure ()
  Left e -> throwFailed (label <> ": " <> ileStderr e)
