{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Network state machine for `corvus-netd`.
--
-- A "network" is the bundle of kernel state the agent owns on
-- behalf of one logical corvus network:
--
--   * a bridge link (`ip link add … type bridge`)
--   * an optional CIDR on that bridge
--   * an optional masquerade rule in `inet corvus_fw postrouting`
--   * an optional dnsmasq child process
--
-- Three operations:
--
--   * 'applyNetwork' — idempotent: create-if-missing,
--     update-if-changed, no-op-if-same.
--   * 'deleteNetwork' — remove the network's entire kernel
--     state and its ledger entry.
--   * 'listNetworks' — enumerate the agent's ledger.
--
-- Reconciliation compares the incoming spec against the stored
-- one and walks deltas. Errors during a delta step roll back
-- whatever kernel state was already touched in that step; the
-- ledger entry is only updated after every step succeeds.
module Corvus.Netd.Network
  ( NetworkSpec (..)
  , NatSpec (..)
  , DhcpSpec (..)
  , DhcpHostReservation (..)
  , OverlaySpec (..)
  , VxlanSpec (..)
  , NetworkInfo (..)
  , NetworkLiveState
  , NetworkError (..)
  , Ledger
  , applyNetwork
  , deleteNetwork
  , listNetworks
  )
where

import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Monad (unless, void, when, (<=<))
import Corvus.Netd.Cleanup (corvusBridgePrefix)
import qualified Corvus.Netd.Dnsmasq as Dn
import qualified Corvus.Netd.HostDns as HD
import Corvus.Netd.IpLink
  ( IpLinkError (..)
  , addrAdd
  , addrDel
  , bridgeAdd
  , bridgeDel
  , fdbAppend
  , fdbDel
  , fdbList
  , linkSetMaster
  , linkSetMtu
  , linkSetUp
  , vxlanAdd
  )
import qualified Corvus.Netd.IpLink as IpLink
import qualified Corvus.Netd.Ledger as L
import Corvus.Netd.Nftables (NftError (..), RuleHandle)
import qualified Corvus.Netd.Nftables as Nft
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word32)

-- | The 'L.NetworkLedger' specialised for our spec/live types.
type Ledger = L.NetworkLedger NetworkSpec NetworkLiveState

-- | Shape of a network. Mirrors the Cap'n Proto @NetworkSpec@
-- struct.
data NetworkSpec = NetworkSpec
  { nsName :: !T.Text
  , nsCidr :: !T.Text
  , nsMtu :: !Word32
  , nsNat :: !NatSpec
  , nsDhcp :: !DhcpSpec
  , nsOverlay :: !OverlaySpec
  }
  deriving (Eq, Show)

data NatSpec = NatSpec
  { natEnabled :: !Bool
  , natUplinkIf :: !T.Text
  }
  deriving (Eq, Show)

data DhcpSpec = DhcpSpec
  { dhcpEnabled :: !Bool
  , dhcpRangeStart :: !T.Text
  , dhcpRangeEnd :: !T.Text
  , dhcpLeaseTime :: !T.Text
  , dhcpDomain :: !T.Text
  , dhcpExtraArgs :: ![T.Text]
  , dhcpHostReservations :: ![DhcpHostReservation]
  , dhcpDnsServers :: ![T.Text]
  , dhcpHostDns :: !Bool
  }
  deriving (Eq, Show)

data DhcpHostReservation = DhcpHostReservation
  { dhrMac :: !T.Text
  , dhrIp :: !T.Text
  }
  deriving (Eq, Show)

-- | Overlay configuration. 'OverlayNone' is single-node behavior;
-- 'OverlayVxlan' wires a VXLAN VTEP onto the bridge so the L2
-- segment spans multiple hosts.
data OverlaySpec
  = OverlayNone
  | OverlayVxlan !VxlanSpec
  deriving (Eq, Show)

data VxlanSpec = VxlanSpec
  { vsVni :: !Word32
  , vsLocalIp :: !T.Text
  , vsPeerIps :: ![T.Text]
  }
  deriving (Eq, Show)

-- | Live kernel/process handles the agent allocated for one
-- network. Stays in the ledger alongside the spec.
data NetworkLiveState = NetworkLiveState
  { lsNatHandle :: !(Maybe RuleHandle)
  , lsDnsmasq :: !(Maybe Dn.DnsmasqProcess)
  }

-- | What the agent reports back for one network.
data NetworkInfo = NetworkInfo
  { niSpec :: !NetworkSpec
  , niUpState :: !T.Text
  , niDnsmasqPid :: !Word32
  }
  deriving (Show)

-- | Errors surfaced to the Session cap layer.
data NetworkError
  = InvalidName !T.Text
  | KernelFailure !T.Text
  | NftFailure !T.Text
  | DnsmasqFailure !T.Text
  deriving (Show)

instance E.Exception NetworkError

-- ---------------------------------------------------------------------------
-- Public API

applyNetwork :: Ledger -> NetworkSpec -> IO (Either NetworkError NetworkInfo)
applyNetwork ledger spec = E.try @NetworkError $ do
  validateName (nsName spec)
  current <- atomically $ Map.lookup (nsName spec) <$> L.readNetworks ledger
  case current of
    Nothing -> createFromScratch ledger spec
    Just (oldSpec, oldState) -> reconcile ledger oldSpec oldState spec

deleteNetwork :: Ledger -> T.Text -> IO (Either NetworkError ())
deleteNetwork ledger name = E.try @NetworkError $ do
  mEntry <-
    atomically $ do
      m <- L.readNetworks ledger
      case Map.lookup name m of
        Nothing -> pure Nothing
        Just entry -> do
          L.removeNetwork ledger name
          pure (Just entry)
  case mEntry of
    Nothing -> pure ()
    Just (spec, state) -> teardown spec state

listNetworks :: Ledger -> IO [NetworkInfo]
listNetworks ledger = do
  m <- atomically (L.readNetworks ledger)
  pure (map (uncurry mkInfo) (Map.elems m))

-- ---------------------------------------------------------------------------
-- Validation

validateName :: T.Text -> IO ()
validateName name = do
  whenE (T.null name) (InvalidName "name must be non-empty")
  whenE (not (corvusBridgePrefix `T.isPrefixOf` name)) $
    InvalidName (name <> ": must start with \"" <> corvusBridgePrefix <> "\"")
  -- IFNAMSIZ is 16 (includes NUL); usable length is 15.
  whenE (T.length name > 15) $
    InvalidName (name <> ": exceeds IFNAMSIZ-1 (15 chars)")

whenE :: Bool -> NetworkError -> IO ()
whenE True err = E.throwIO err
whenE False _ = pure ()

-- ---------------------------------------------------------------------------
-- Fresh-create path

createFromScratch :: Ledger -> NetworkSpec -> IO NetworkInfo
createFromScratch ledger spec = do
  let name = nsName spec
  -- ip link add … type bridge
  unwrapIp =<< bridgeAdd name
  let rollback = do
        case nsOverlay spec of
          OverlayVxlan v -> void (IpLink.bridgeDel (vxlanIfaceName (vsVni v)))
          OverlayNone -> pure ()
        void (bridgeDel name)
  withRollback rollback $ do
    -- MTU (default is 1500 — only set if different)
    when (nsMtu spec /= 1500) $
      unwrapIp =<< linkSetMtu name (nsMtu spec)
    -- CIDR (optional)
    unless (T.null (nsCidr spec)) $
      unwrapIp =<< addrAdd (nsCidr spec) name
    -- VXLAN overlay (optional) — must be in place before linkSetUp
    -- on the bridge so the agent observes a consistent ledger state.
    applyOverlayCreate spec
    -- Bring up
    unwrapIp =<< linkSetUp name
  -- NAT (optional)
  natHandle <-
    if natEnabled (nsNat spec)
      then do
        unwrapNft =<< Nft.ensureBaseTable
        Just <$> (unwrapNft =<< Nft.addMasquerade (nsCidr spec) (natUplinkIf (nsNat spec)))
      else pure Nothing
  -- dnsmasq (optional)
  dn <-
    if dhcpEnabled (nsDhcp spec)
      then Just <$> startDnsmasqFor spec
      else pure Nothing
  let live =
        NetworkLiveState
          { lsNatHandle = natHandle
          , lsDnsmasq = dn
          }
  atomically $ L.insertNetwork ledger (nsName spec) spec live
  pure (mkInfo spec live)

-- ---------------------------------------------------------------------------
-- Reconcile path

reconcile
  :: Ledger
  -> NetworkSpec
  -> NetworkLiveState
  -> NetworkSpec
  -> IO NetworkInfo
reconcile ledger oldSpec oldState newSpec
  | oldSpec == newSpec = pure (mkInfo newSpec oldState)
  | otherwise = do
      let name = nsName newSpec
      -- CIDR change
      when (nsCidr oldSpec /= nsCidr newSpec) $ do
        unless (T.null (nsCidr oldSpec)) $
          ignoreIp =<< addrDel (nsCidr oldSpec) name
        unless (T.null (nsCidr newSpec)) $
          unwrapIp =<< addrAdd (nsCidr newSpec) name
      -- MTU change
      when (nsMtu oldSpec /= nsMtu newSpec) $
        unwrapIp =<< linkSetMtu name (nsMtu newSpec)
      -- Overlay delta (VXLAN device + flood FDB)
      reconcileOverlay oldSpec newSpec
      -- NAT delta
      newHandle <- reconcileNat oldSpec oldState newSpec
      -- DHCP delta
      newDn <- reconcileDhcp oldSpec oldState newSpec
      let live =
            NetworkLiveState
              { lsNatHandle = newHandle
              , lsDnsmasq = newDn
              }
      atomically $ L.insertNetwork ledger name newSpec live
      pure (mkInfo newSpec live)

reconcileNat
  :: NetworkSpec
  -> NetworkLiveState
  -> NetworkSpec
  -> IO (Maybe RuleHandle)
reconcileNat oldSpec oldState newSpec
  | nsNat oldSpec == nsNat newSpec && nsCidr oldSpec == nsCidr newSpec =
      pure (lsNatHandle oldState)
  | otherwise = do
      -- drop old handle if any
      case lsNatHandle oldState of
        Just h -> ignoreNft =<< Nft.deleteRule h
        Nothing -> pure ()
      if natEnabled (nsNat newSpec)
        then do
          unwrapNft =<< Nft.ensureBaseTable
          Just
            <$> (unwrapNft =<< Nft.addMasquerade (nsCidr newSpec) (natUplinkIf (nsNat newSpec)))
        else pure Nothing

-- For dnsmasq we always stop-and-maybe-restart on any change.
-- dnsmasq doesn't reload all flag categories via SIGHUP, so a
-- clean restart is the safest reconcile.
reconcileDhcp
  :: NetworkSpec
  -- ^ the spec we're reconciling FROM (for the old bridge name; the
  -- bridge can't actually rename today but we still pull from oldSpec
  -- for symmetry with the rest of the reconcile)
  -> NetworkLiveState
  -> NetworkSpec
  -> IO (Maybe Dn.DnsmasqProcess)
reconcileDhcp oldSpec oldState newSpec = do
  -- Drop the per-link resolved routing BEFORE stopping dnsmasq so
  -- a brief "dnsmasq up, host points at it, but it's about to be
  -- killed" window doesn't surface as a SERVFAIL spike to the
  -- host's resolver. The fresh apply below re-installs it if
  -- still desired.
  HD.revertResolvedRouting (nsName oldSpec)
  for_ (lsDnsmasq oldState) Dn.stopDnsmasq
  if dhcpEnabled (nsDhcp newSpec)
    then Just <$> startDnsmasqFor newSpec
    else pure Nothing

-- ---------------------------------------------------------------------------
-- dnsmasq supervision

startDnsmasqFor :: NetworkSpec -> IO Dn.DnsmasqProcess
startDnsmasqFor spec = do
  let dhcp = nsDhcp spec
      listenAddr = stripCidrSuffix (nsCidr spec)
      dhcpRange
        | T.null (dhcpRangeStart dhcp) || T.null (dhcpRangeEnd dhcp) = ""
        | otherwise =
            dhcpRangeStart dhcp
              <> ","
              <> dhcpRangeEnd dhcp
              <> ","
              <> dhcpLeaseTime dhcp
      params =
        Dn.DnsmasqStartParams
          { Dn.dspBridge = nsName spec
          , Dn.dspListenAddr = listenAddr
          , Dn.dspDhcpRange = dhcpRange
          , Dn.dspDomain = dhcpDomain dhcp
          , Dn.dspExtraArgs = dhcpExtraArgs dhcp
          , Dn.dspHostReservations =
              [(dhrMac r, dhrIp r) | r <- dhcpHostReservations dhcp]
          , Dn.dspDnsServers = dhcpDnsServers dhcp
          }
  dn <-
    Dn.startDnsmasq params >>= \case
      Right d -> pure d
      Left e -> E.throwIO (DnsmasqFailure (T.pack (show e)))
  -- Install (or refresh) per-link systemd-resolved routing so the
  -- host can resolve `*.<domain>` to the bridge IP. No-op if
  -- hostDns is off or the domain is empty. Idempotent: same
  -- per-link state, every apply.
  when (dhcpHostDns dhcp) $
    HD.installResolvedRouting (nsName spec) listenAddr (dhcpDomain dhcp)
  pure dn

-- | "10.0.0.1/24" → "10.0.0.1".  dnsmasq wants a bare IP for
-- --listen-address.
stripCidrSuffix :: T.Text -> T.Text
stripCidrSuffix cidr = case T.splitOn "/" cidr of
  (host : _) -> host
  [] -> cidr

-- ---------------------------------------------------------------------------
-- Teardown

teardown :: NetworkSpec -> NetworkLiveState -> IO ()
teardown spec state = do
  -- Drop the host-side route first so resolved stops pointing at
  -- the bridge before dnsmasq goes away.
  HD.revertResolvedRouting (nsName spec)
  for_ (lsDnsmasq state) Dn.stopDnsmasq
  for_ (lsNatHandle state) (ignoreNft <=< Nft.deleteRule)
  case nsOverlay spec of
    OverlayVxlan v -> void (IpLink.bridgeDel (vxlanIfaceName (vsVni v)))
    OverlayNone -> pure ()
  void (bridgeDel (nsName spec))

-- ---------------------------------------------------------------------------
-- Overlay (VXLAN) reconciliation

-- | Linux ifname for a VXLAN VTEP. Distinct prefix so 'Cleanup' can
-- pick it up on agent startup / shutdown. With VNI base 10000 in
-- base-10 we stay inside IFNAMSIZ-1 for any VNI up to 9 999 999.
vxlanIfaceName :: Word32 -> T.Text
vxlanIfaceName vni = "corvus-vx" <> T.pack (show vni)

-- | The "all zeros" MAC the kernel's bridge code interprets as a
-- BUM flood entry. One entry per peer VTEP gives head-end
-- replication.
floodMac :: T.Text
floodMac = "00:00:00:00:00:00"

-- | Create the VXLAN device + initial flood FDB. Called from the
-- fresh-create path only; reconcile uses 'reconcileOverlay'.
applyOverlayCreate :: NetworkSpec -> IO ()
applyOverlayCreate spec = case nsOverlay spec of
  OverlayNone -> pure ()
  OverlayVxlan v -> do
    let vxIface = vxlanIfaceName (vsVni v)
    unwrapIp =<< vxlanAdd vxIface (vsVni v) (vsLocalIp v)
    -- MTU on the vxlan device: the bridge picks up the smallest
    -- member MTU, so this ends up constraining what guests can
    -- send without fragmenting.
    when (nsMtu spec /= 1500) $
      unwrapIp =<< linkSetMtu vxIface (nsMtu spec)
    unwrapIp =<< linkSetMaster vxIface (nsName spec)
    unwrapIp =<< linkSetUp vxIface
    mapM_ (unwrapIp <=< fdbAppend floodMac vxIface) (vsPeerIps v)

-- | Reconcile VXLAN state when the spec changes. Four cases:
--
--   * none → none: nothing to do.
--   * none → vxlan: create + populate (same as fresh).
--   * vxlan → none: destroy.
--   * vxlan → vxlan: VNI / localIp changed → recreate; otherwise
--     just reconcile the flood FDB.
reconcileOverlay :: NetworkSpec -> NetworkSpec -> IO ()
reconcileOverlay oldSpec newSpec = case (nsOverlay oldSpec, nsOverlay newSpec) of
  (OverlayNone, OverlayNone) -> pure ()
  (OverlayNone, OverlayVxlan _) -> applyOverlayCreate newSpec
  (OverlayVxlan v, OverlayNone) ->
    void (IpLink.bridgeDel (vxlanIfaceName (vsVni v)))
  (OverlayVxlan vOld, OverlayVxlan vNew)
    | vsVni vOld /= vsVni vNew || vsLocalIp vOld /= vsLocalIp vNew -> do
        -- VNI or local-ip changed: blow it away and recreate.
        void (IpLink.bridgeDel (vxlanIfaceName (vsVni vOld)))
        applyOverlayCreate newSpec
    | otherwise -> reconcileFlood (vxlanIfaceName (vsVni vNew)) (vsPeerIps vNew)

-- | Make the flood FDB on @dev@ match @desired@ (the set of peer
-- VTEP IPs). Adds missing entries, deletes stale ones. We only
-- manage flood entries (MAC = '00:00:00:00:00:00') — unicast
-- entries get installed by the bridge's data-plane learning and
-- are not our business.
reconcileFlood :: T.Text -> [T.Text] -> IO ()
reconcileFlood dev desired = do
  entries <- unwrapIp =<< fdbList dev
  let currentFlood =
        Set.fromList [dst | (mac, dst) <- entries, mac == floodMac]
      target = Set.fromList desired
      toAdd = Set.difference target currentFlood
      toRemove = Set.difference currentFlood target
  mapM_ (unwrapIp <=< fdbAppend floodMac dev) (Set.toList toAdd)
  mapM_ (ignoreIp <=< fdbDel floodMac dev) (Set.toList toRemove)

-- ---------------------------------------------------------------------------
-- Helpers

mkInfo :: NetworkSpec -> NetworkLiveState -> NetworkInfo
mkInfo spec state =
  NetworkInfo
    { niSpec = spec
    , niUpState = "up"
    , niDnsmasqPid = maybe 0 (fromIntegral . Dn.dpPid) (lsDnsmasq state)
    }

-- | Run-or-throw for IpLink results.
unwrapIp :: Either IpLinkError a -> IO a
unwrapIp (Right a) = pure a
unwrapIp (Left e) = E.throwIO (KernelFailure (ileStderr e))

-- | Best-effort IpLink call; swallow errors. Used for delete
-- ops during reconcile where divergence (admin yanked state)
-- is recoverable.
ignoreIp :: Either IpLinkError () -> IO ()
ignoreIp = const (pure ())

-- | Run-or-throw for Nftables results.
unwrapNft :: Either NftError a -> IO a
unwrapNft (Right a) = pure a
unwrapNft (Left e) = E.throwIO (NftFailure (neStderr e))

-- | Best-effort nftables call.
ignoreNft :: Either NftError () -> IO ()
ignoreNft = const (pure ())

-- | Run @action@; on 'NetworkError', run @rollback@ and re-throw.
withRollback :: IO () -> IO a -> IO a
withRollback rollback =
  E.handle (\(e :: NetworkError) -> rollback >> E.throwIO e)
