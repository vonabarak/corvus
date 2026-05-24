@0xdc412029c77dfa4a;

# Cap'n Proto wire schema for `corvus-netd`, the privileged network
# agent. Phase 2.5: declarative model. The daemon expresses INTENT
# via `applyNetwork` / `applyTap`; the agent reconciles the kernel
# to match. No per-resource Cap'n Proto interfaces (Bridge,
# NatRule, DnsmasqHandle, Tap) and no orphan window — the agent is
# stateless and the daemon's database is the single source of intent.
#
# Naming convention: every agent-managed kernel resource has a
# fixed prefix the agent uses for startup / shutdown cleanup:
#
#   * Bridges:  `corvus-br*`
#   * TAPs:     `corvus-tap*`
#   * nftables: table `inet corvus_fw`
#   * dnsmasq:  identified by `--interface=corvus-br*` in argv
#
# Authentication: mTLS gates the listener. The agent binds to
# 0.0.0.0:9877 by default; override the bind address via the
# binary's --host flag. Caller-claimed `owner` strings partition
# the agent's ledger but are not a security boundary.

# ---------------------------------------------------------------------
# Bootstrap interface
# ---------------------------------------------------------------------

interface NetAgent {
  # Open a new session. `owner` is a free-form tag (typically the
  # caller's numeric uid as a string) used to partition resources
  # in the agent's ledger. Not authenticated.
  session @0 (owner :Text) -> (session :Session);

  # Liveness + capability probe.
  ping    @1 () -> ();
  version @2 () -> (info :AgentInfo);
}

struct AgentInfo {
  semver       @0 :Text;       # "0.2.0"
  capabilities @1 :List(Text); # "network", "tap", "ip-forwarding", "events"
}

# ---------------------------------------------------------------------
# Session — scoped to one owner tag.
#
# All operations are declarative + idempotent. The agent's in-memory
# ledger maps name → spec; the kernel is the side effect. Specs are
# expressed by-name, never by capability.
# ---------------------------------------------------------------------

interface Session {
  # Networks (= bridge + optional NAT rule + optional dnsmasq).
  # applyNetwork is idempotent: create-if-missing,
  # update-if-changed, no-op-if-same.
  applyNetwork  @0 (spec :NetworkSpec) -> (info :NetworkInfo);
  listNetworks  @1 ()                  -> (networks :List(NetworkInfo));
  deleteNetwork @2 (name :Text)        -> ();

  # TAPs. Same idempotent model. The bridge must already have been
  # applyNetwork'd (referenced by name).
  applyTap   @3 (spec :TapSpec) -> (info :TapInfo);
  listTaps   @4 ()              -> (taps :List(TapInfo));
  deleteTap  @5 (name :Text)    -> ();

  # Host knob — typed, not generic sysctl.
  setIpForwarding @6 (enabled :Bool, family :NetFamily) -> ();

  # Drift notifications: rtnetlink-driven, fired when the agent
  # observes a kernel state change it didn't initiate.
  subscribeEvents @7 (sink :EventSink) -> ();
}

# ---------------------------------------------------------------------
# Network spec / info
# ---------------------------------------------------------------------

struct NetworkSpec {
  # MUST begin with "corvus-br". The prefix is the agent's startup
  # and shutdown cleanup marker; resources without it are not
  # touched by the agent.
  name @0 :Text;

  # "10.42.0.1/24" or "" for an L2-only bridge (no IP). On peer
  # nodes of a VXLAN overlay this MUST be empty — only the owner
  # gets the gateway IP / NAT / DHCP.
  cidr @1 :Text;

  # Guest-facing MTU. Callers pre-subtract 50 bytes for VXLAN
  # encapsulation so the bridge member's MTU matches what the
  # guest will fragment / segment to.
  mtu @2 :UInt32 = 1500;

  # NAT and DHCP are optional sub-structs. Always present in the
  # wire encoding; the `enabled` field controls whether the agent
  # acts on the rest.
  nat  @3 :NatSpec;
  dhcp @4 :DhcpSpec;

  # Multi-node overlay. Defaults to 'none' (single-node bridge).
  overlay @5 :OverlaySpec;
}

struct NatSpec {
  enabled  @0 :Bool;
  # Uplink interface for the masquerade rule. "" matches any
  # outbound interface; common case: leave it empty.
  uplinkIf @1 :Text;
}

struct DhcpSpec {
  enabled    @0 :Bool;
  rangeStart @1 :Text;
  rangeEnd   @2 :Text;
  leaseTime  @3 :Text = "12h";
  # "" = no --domain flag passed to dnsmasq.
  domain     @4 :Text;
  # Pass-through args for advanced dnsmasq tuning. Most callers
  # leave this empty.
  extraArgs  @5 :List(Text);
  # MAC-keyed lease reservations. The daemon emits one entry per
  # NIC attached to a managed network, so the same VM always
  # receives the same IP — including when it migrates onto a peer
  # node. Empty on networks without an IPAM allocation yet.
  hostReservations @6 :List(DhcpHostReservation);
}

struct DhcpHostReservation {
  mac @0 :Text;   # "52:54:00:aa:bb:cc"
  ip  @1 :Text;   # "10.0.0.7"
}

# A network's overlay configuration. 'none' means today's
# single-node behavior (one bridge, no VXLAN). 'vxlan' adds a
# VXLAN VTEP attached to the bridge so the L2 segment spans
# multiple nodes.
struct OverlaySpec {
  union {
    none  @0 :Void;
    vxlan @1 :VxlanSpec;
  }
}

struct VxlanSpec {
  # 24-bit VXLAN Network Identifier. Same value on every node in
  # the mesh.
  vni @0 :UInt32;

  # This node's underlay IP — the 'local' kwarg on
  # `ip link add type vxlan`. Distinct from the bridge CIDR.
  localIp @1 :Text;

  # Every OTHER VTEP in the mesh. The agent installs one BUM flood
  # FDB entry per peer (head-end replication).
  peerIps @2 :List(Text);
}

struct NetworkInfo {
  spec       @0 :NetworkSpec;
  upState    @1 :Text;       # "up", "down", "unknown"
  dnsmasqPid @2 :UInt32;     # 0 if DHCP disabled
}

# ---------------------------------------------------------------------
# TAP spec / info
# ---------------------------------------------------------------------

struct TapSpec {
  # MUST begin with "corvus-tap". Same cleanup-marker rationale.
  name @0 :Text;

  # Name of the bridge to attach this TAP to. The bridge MUST have
  # been applied first; the agent rejects unknown bridge names.
  bridge @1 :Text;

  # TUNSETOWNER: the agent creates the persistent TAP owned by
  # this uid/gid, so QEMU (running as the daemon's uid) can
  # reopen it without CAP_NET_ADMIN.
  uid @2 :UInt32;
  gid @3 :UInt32;
}

struct TapInfo {
  spec    @0 :TapSpec;
  upState @1 :Text;
}

# ---------------------------------------------------------------------
# Events
# ---------------------------------------------------------------------

# Streaming sink the daemon implements and passes to the agent via
# `subscribeEvents`. The agent calls back when kernel state drifts.
interface EventSink {
  # kind = "network" | "tap" | "dnsmasq"
  onResourceVanished @0 (kind :Text, name :Text) -> ();
}

# ---------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------

enum NetFamily {
  v4 @0;
  v6 @1;
}
