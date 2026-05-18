@0xdc412029c77dfa4a;

# Cap'n Proto wire schema for `corvus-netd`, the privileged network
# agent. The daemon (unprivileged) connects to the agent (root) over
# TCP and requests every operation that needs CAP_NET_ADMIN — bridge
# create/destroy, TAP create/destroy, NAT rules, dnsmasq lifecycle,
# IP forwarding toggles.
#
# Phase 1: stubs only. Methods return placeholder values and log the
# call; no kernel mutation happens yet. Phase 2 fills in the real
# implementations.
#
# Authentication: out of scope for v1. The agent binds to
# 127.0.0.1:9877 and trusts every connection. Caller-claimed `owner`
# strings partition the agent's ledger but are not a security
# boundary.

# ---------------------------------------------------------------------
# Bootstrap interface
# ---------------------------------------------------------------------

interface NetAgent {
  # Open a new session. `owner` is a free-form tag (typically the
  # caller's numeric uid as a string) used to partition resources in
  # the agent's ledger. Not authenticated.
  session @0 (owner :Text) -> (session :Session);

  # Liveness + capability probe.
  ping    @1 () -> ();
  version @2 () -> (info :AgentInfo);
}

struct AgentInfo {
  semver       @0 :Text;       # "0.1.0"
  capabilities @1 :List(Text); # advertised feature flags, e.g. "bridge", "tap", "nat", "dnsmasq"
}

# ---------------------------------------------------------------------
# Session — scoped to one owner tag.
#
# Resources created through a Session are tagged with that owner and
# bound to the Session's lifetime: dropping the session capability
# moves owned resources to a 60-second orphan window during which the
# owner can re-adopt them via the matching `claim*` call. After the
# window, the agent reaps everything.
# ---------------------------------------------------------------------

interface Session {
  # Bridge management.
  createBridge  @0 (params :BridgeParams) -> (bridge :Bridge);
  listBridges   @1 () -> (bridges :List(BridgeInfo));
  claimBridge   @2 (name :Text) -> (bridge :Bridge);

  # TAP management. The agent creates TAPs with TUNSETOWNER set to the
  # session's owner uid (parsed from the owner tag if numeric); the
  # daemon then passes the ifname to QEMU, which opens it directly.
  createTap     @3 (params :TapParams) -> (tap :Tap);
  claimTap      @4 (name :Text) -> (tap :Tap);

  # Kernel knobs — typed, never a generic sysctl(key, value).
  setIpForwarding @5 (enabled :Bool, family :NetFamily) -> ();

  # NAT rules. Drop the returned cap to remove the rule.
  installNat    @6 (params :NatParams) -> (nat :NatRule);

  # dnsmasq lifecycle. Drop the handle to stop the process.
  startDnsmasq  @7 (params :DnsmasqParams) -> (server :DnsmasqHandle);

  # Drift events (rtnetlink-driven). The agent calls back into the
  # given sink when kernel state changes outside its control.
  subscribeEvents @8 (sink :EventSink) -> ();
}

# ---------------------------------------------------------------------
# Resource capabilities. Each one is connection-scoped: drop the cap
# and the agent tears the resource down (after the orphan grace).
# ---------------------------------------------------------------------

interface Bridge {
  info      @0 () -> (info :BridgeInfo);
  attachTap @1 (tap :Tap) -> ();
  detachTap @2 (tap :Tap) -> ();
  destroy   @3 () -> ();
}

interface Tap {
  info    @0 () -> (info :TapInfo);
  destroy @1 () -> ();
}

interface DnsmasqHandle {
  pid  @0 () -> (pid :UInt32);
  stop @1 () -> ();
}

interface NatRule {
  destroy @0 () -> ();
}

# ---------------------------------------------------------------------
# Streaming event sink. Same pattern as schema/streams.capnp — the
# daemon implements this server-side and passes the cap into
# Session.subscribeEvents.
# ---------------------------------------------------------------------

interface EventSink {
  onResourceVanished @0 (kind :Text, name :Text) -> ();
  onDnsmasqExited    @1 (name :Text, exitStatus :Int32) -> ();
}

# ---------------------------------------------------------------------
# Parameter structs
# ---------------------------------------------------------------------

struct BridgeParams {
  name @0 :Text;  # e.g. "corvus-br-7"
  cidr @1 :Text;  # "10.42.0.1/24"; "" for L2-only (no IP on bridge)
  mtu  @2 :UInt32 = 1500;
}

struct TapParams {
  name   @0 :Text;    # "corvus-tap-vm42-eth0"
  bridge @1 :Bridge;  # capability, not name
  # The TUNSETOWNER uid/gid the agent should set on the persistent
  # TAP. The daemon passes its own creds here so QEMU (running as the
  # daemon's uid) can reopen the device.
  uid    @2 :UInt32;
  gid    @3 :UInt32;
}

struct NatParams {
  bridge   @0 :Bridge;
  uplinkIf @1 :Text;  # "eth0" or "" for "any default route"
  subnet   @2 :Text;  # "10.42.0.0/24"
}

struct DnsmasqParams {
  bridge      @0 :Bridge;
  listenAddr  @1 :Text;  # gateway IP on the bridge
  dhcpRange   @2 :Text;  # "10.42.0.100,10.42.0.250,12h" or "" for no DHCP
  domain      @3 :Text;  # "" for default
  extraArgs   @4 :List(Text);  # passthrough flags for advanced use
}

# ---------------------------------------------------------------------
# Info structs returned by `info` / `list` methods
# ---------------------------------------------------------------------

struct BridgeInfo {
  name     @0 :Text;
  cidr     @1 :Text;
  mtu      @2 :UInt32;
  upState  @3 :Text;   # "up", "down", "unknown"
  owner    @4 :Text;
  tapCount @5 :UInt32;
}

struct TapInfo {
  name   @0 :Text;
  bridge @1 :Text;   # bridge name (informational)
  uid    @2 :UInt32;
  gid    @3 :UInt32;
  owner  @4 :Text;
}

# ---------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------

enum NetFamily {
  v4 @0;
  v6 @1;
}
