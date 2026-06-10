{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Per-owner session capability for `corvus-netd`.
--
-- Phase 2.5: declarative model. The session exposes seven
-- idempotent methods that drive 'Corvus.Netd.Network' and
-- 'Corvus.Netd.Tap' state machines. No per-resource cap types.
module Corvus.Netd.Caps.Session
  ( SessionCap (..)
  , newSessionCap
  )
where

import qualified Capnp.Gen.Netagent as CGN
import Capnp.Rpc (throwFailed)
import Capnp.Rpc.Server (SomeServer)
import qualified Control.Exception as E
import qualified Corvus.Netd.Events as Ev
import qualified Corvus.Netd.Ledger as L
import qualified Corvus.Netd.Network as Net
import qualified Corvus.Netd.Sysctl as Sys
import qualified Corvus.Netd.Tap as Tap
import Corvus.Rpc.Common (handleParsed)
import qualified Data.Text as T
import Supervisors (Supervisor)

-- | Session state. The two ledgers are process-wide (shared
-- across all sessions); 'scSubs' is also process-wide.
-- 'scOwner' is the caller-supplied owner tag, kept for logs but
-- not currently used for any partitioning logic.
data SessionCap = SessionCap
  { scOwner :: !T.Text
  , scSup :: !Supervisor
  , scNetworkLedger :: !(L.NetworkLedger Net.NetworkSpec Net.NetworkLiveState)
  , scTapLedger :: !(L.TapLedger Tap.TapSpec)
  , scSubs :: !Ev.Subscribers
  }

newSessionCap
  :: T.Text
  -> Supervisor
  -> L.NetworkLedger Net.NetworkSpec Net.NetworkLiveState
  -> L.TapLedger Tap.TapSpec
  -> Ev.Subscribers
  -> IO SessionCap
newSessionCap owner sup nl tl subs =
  pure
    SessionCap
      { scOwner = owner
      , scSup = sup
      , scNetworkLedger = nl
      , scTapLedger = tl
      , scSubs = subs
      }

instance SomeServer SessionCap

instance CGN.Session'server_ SessionCap where
  -- -- Networks ---------------------------------------------------------------

  session'applyNetwork sess =
    handleParsed $ \CGN.Session'applyNetwork'params {CGN.spec = wireSpec} -> do
      spec <- decodeNetworkSpec wireSpec
      result <- Net.applyNetwork (scNetworkLedger sess) spec
      case result of
        Right info ->
          pure
            CGN.Session'applyNetwork'results
              { CGN.info = encodeNetworkInfo info
              }
        Left e ->
          throwFailed ("applyNetwork: " <> netErrText e)

  session'listNetworks sess =
    handleParsed $ \_ -> do
      infos <- Net.listNetworks (scNetworkLedger sess)
      pure
        CGN.Session'listNetworks'results
          { CGN.networks = map encodeNetworkInfo infos
          }

  session'deleteNetwork sess =
    handleParsed $ \CGN.Session'deleteNetwork'params {CGN.name = name} -> do
      result <- Net.deleteNetwork (scNetworkLedger sess) name
      case result of
        Right () -> pure CGN.Session'deleteNetwork'results
        Left e -> throwFailed ("deleteNetwork: " <> netErrText e)

  -- -- TAPs -------------------------------------------------------------------

  session'applyTap sess =
    handleParsed $ \CGN.Session'applyTap'params {CGN.spec = wireSpec} -> do
      let spec = decodeTapSpec wireSpec
      result <- Tap.applyTap (scTapLedger sess) spec
      case result of
        Right info ->
          pure
            CGN.Session'applyTap'results
              { CGN.info = encodeTapInfo info
              }
        Left e -> throwFailed ("applyTap: " <> tapErrText e)

  session'listTaps sess =
    handleParsed $ \_ -> do
      infos <- Tap.listTaps (scTapLedger sess)
      pure
        CGN.Session'listTaps'results
          { CGN.taps = map encodeTapInfo infos
          }

  session'deleteTap sess =
    handleParsed $ \CGN.Session'deleteTap'params {CGN.name = name} -> do
      result <- Tap.deleteTap (scTapLedger sess) name
      case result of
        Right () -> pure CGN.Session'deleteTap'results
        Left e -> throwFailed ("deleteTap: " <> tapErrText e)

  -- -- Kernel knobs ------------------------------------------------------------

  session'setIpForwarding _ =
    handleParsed $
      \CGN.Session'setIpForwarding'params
        { CGN.enabled = enabled
        , CGN.family_ = family_
        } -> do
          let fam = case family_ of
                CGN.NetFamily'v4 -> Sys.V4
                CGN.NetFamily'v6 -> Sys.V6
                CGN.NetFamily'unknown' _ -> Sys.V4
          result <- E.try @IOError (Sys.setIpForwarding enabled fam)
          case result of
            Right () -> pure CGN.Session'setIpForwarding'results
            Left e -> throwFailed ("setIpForwarding: " <> T.pack (show e))

  -- -- Event sink --------------------------------------------------------------

  session'subscribeEvents sess =
    handleParsed $ \CGN.Session'subscribeEvents'params {CGN.sink = sink} -> do
      Ev.addSink (scSubs sess) sink
      pure CGN.Session'subscribeEvents'results

-- ---------------------------------------------------------------------------
-- Wire ↔ internal decoders / encoders

decodeNetworkSpec :: CGN.Parsed CGN.NetworkSpec -> IO Net.NetworkSpec
decodeNetworkSpec
  CGN.NetworkSpec
    { CGN.name = name
    , CGN.cidr = cidr
    , CGN.mtu = mtu
    , CGN.nat = nat
    , CGN.dhcp = dhcp
    , CGN.overlay = overlay
    } =
    pure
      Net.NetworkSpec
        { Net.nsName = name
        , Net.nsCidr = cidr
        , Net.nsMtu = mtu
        , Net.nsNat = decodeNatSpec nat
        , Net.nsDhcp = decodeDhcpSpec dhcp
        , Net.nsOverlay = decodeOverlaySpec overlay
        }

decodeOverlaySpec :: CGN.Parsed CGN.OverlaySpec -> Net.OverlaySpec
decodeOverlaySpec CGN.OverlaySpec {CGN.union' = variant} = case variant of
  CGN.OverlaySpec'none -> Net.OverlayNone
  CGN.OverlaySpec'vxlan v -> Net.OverlayVxlan (decodeVxlanSpec v)
  CGN.OverlaySpec'unknown' _ -> Net.OverlayNone

decodeVxlanSpec :: CGN.Parsed CGN.VxlanSpec -> Net.VxlanSpec
decodeVxlanSpec
  CGN.VxlanSpec
    { CGN.vni = vni
    , CGN.localIp = localIp
    , CGN.peerIps = peerIps
    } =
    Net.VxlanSpec
      { Net.vsVni = vni
      , Net.vsLocalIp = localIp
      , Net.vsPeerIps = peerIps
      }

decodeDhcpHostReservation
  :: CGN.Parsed CGN.DhcpHostReservation -> Net.DhcpHostReservation
decodeDhcpHostReservation
  CGN.DhcpHostReservation {CGN.mac = mac, CGN.ip = ip} =
    Net.DhcpHostReservation {Net.dhrMac = mac, Net.dhrIp = ip}

decodeNatSpec :: CGN.Parsed CGN.NatSpec -> Net.NatSpec
decodeNatSpec
  CGN.NatSpec
    { CGN.enabled = enabled
    , CGN.uplinkIf = uplinkIf
    } =
    Net.NatSpec
      { Net.natEnabled = enabled
      , Net.natUplinkIf = uplinkIf
      }

decodeDhcpSpec :: CGN.Parsed CGN.DhcpSpec -> Net.DhcpSpec
decodeDhcpSpec
  CGN.DhcpSpec
    { CGN.enabled = enabled
    , CGN.rangeStart = rs
    , CGN.rangeEnd = re
    , CGN.leaseTime = lt
    , CGN.domain = dom
    , CGN.extraArgs = extra
    , CGN.hostReservations = res
    , CGN.dnsServers = dns
    } =
    Net.DhcpSpec
      { Net.dhcpEnabled = enabled
      , Net.dhcpRangeStart = rs
      , Net.dhcpRangeEnd = re
      , Net.dhcpLeaseTime = lt
      , Net.dhcpDomain = dom
      , Net.dhcpExtraArgs = extra
      , Net.dhcpHostReservations = map decodeDhcpHostReservation res
      , Net.dhcpDnsServers = dns
      }

decodeTapSpec :: CGN.Parsed CGN.TapSpec -> Tap.TapSpec
decodeTapSpec
  CGN.TapSpec
    { CGN.name = name
    , CGN.bridge = bridge
    , CGN.uid = uid
    , CGN.gid = gid
    } =
    Tap.TapSpec
      { Tap.tsName = name
      , Tap.tsBridge = bridge
      , Tap.tsUid = uid
      , Tap.tsGid = gid
      }

encodeNetworkInfo :: Net.NetworkInfo -> CGN.Parsed CGN.NetworkInfo
encodeNetworkInfo info =
  CGN.NetworkInfo
    { CGN.spec = encodeNetworkSpec (Net.niSpec info)
    , CGN.upState = Net.niUpState info
    , CGN.dnsmasqPid = Net.niDnsmasqPid info
    }

encodeNetworkSpec :: Net.NetworkSpec -> CGN.Parsed CGN.NetworkSpec
encodeNetworkSpec spec =
  CGN.NetworkSpec
    { CGN.name = Net.nsName spec
    , CGN.cidr = Net.nsCidr spec
    , CGN.mtu = Net.nsMtu spec
    , CGN.nat = encodeNatSpec (Net.nsNat spec)
    , CGN.dhcp = encodeDhcpSpec (Net.nsDhcp spec)
    , CGN.overlay = encodeOverlaySpec (Net.nsOverlay spec)
    }

encodeOverlaySpec :: Net.OverlaySpec -> CGN.Parsed CGN.OverlaySpec
encodeOverlaySpec o = CGN.OverlaySpec {CGN.union' = variant}
  where
    variant = case o of
      Net.OverlayNone -> CGN.OverlaySpec'none
      Net.OverlayVxlan v -> CGN.OverlaySpec'vxlan (encodeVxlanSpec v)

encodeVxlanSpec :: Net.VxlanSpec -> CGN.Parsed CGN.VxlanSpec
encodeVxlanSpec v =
  CGN.VxlanSpec
    { CGN.vni = Net.vsVni v
    , CGN.localIp = Net.vsLocalIp v
    , CGN.peerIps = Net.vsPeerIps v
    }

encodeDhcpHostReservation
  :: Net.DhcpHostReservation -> CGN.Parsed CGN.DhcpHostReservation
encodeDhcpHostReservation r =
  CGN.DhcpHostReservation {CGN.mac = Net.dhrMac r, CGN.ip = Net.dhrIp r}

encodeNatSpec :: Net.NatSpec -> CGN.Parsed CGN.NatSpec
encodeNatSpec n =
  CGN.NatSpec
    { CGN.enabled = Net.natEnabled n
    , CGN.uplinkIf = Net.natUplinkIf n
    }

encodeDhcpSpec :: Net.DhcpSpec -> CGN.Parsed CGN.DhcpSpec
encodeDhcpSpec d =
  CGN.DhcpSpec
    { CGN.enabled = Net.dhcpEnabled d
    , CGN.rangeStart = Net.dhcpRangeStart d
    , CGN.rangeEnd = Net.dhcpRangeEnd d
    , CGN.leaseTime = Net.dhcpLeaseTime d
    , CGN.domain = Net.dhcpDomain d
    , CGN.extraArgs = Net.dhcpExtraArgs d
    , CGN.hostReservations =
        map encodeDhcpHostReservation (Net.dhcpHostReservations d)
    , CGN.dnsServers = Net.dhcpDnsServers d
    }

encodeTapInfo :: Tap.TapInfo -> CGN.Parsed CGN.TapInfo
encodeTapInfo info =
  CGN.TapInfo
    { CGN.spec = encodeTapSpec (Tap.tiSpec info)
    , CGN.upState = Tap.tiUpState info
    }

encodeTapSpec :: Tap.TapSpec -> CGN.Parsed CGN.TapSpec
encodeTapSpec spec =
  CGN.TapSpec
    { CGN.name = Tap.tsName spec
    , CGN.bridge = Tap.tsBridge spec
    , CGN.uid = Tap.tsUid spec
    , CGN.gid = Tap.tsGid spec
    }

-- | Render a 'Net.NetworkError' for Cap'n Proto failure envelopes.
netErrText :: Net.NetworkError -> T.Text
netErrText (Net.InvalidName t) = t
netErrText (Net.KernelFailure t) = t
netErrText (Net.NftFailure t) = t
netErrText (Net.DnsmasqFailure t) = t

tapErrText :: Tap.TapError -> T.Text
tapErrText (Tap.InvalidTapName t) = t
tapErrText (Tap.KernelFailure t) = t
