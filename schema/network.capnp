@0xe996a8e112b41e28;

using Common = import "common.capnp";

# ---------------------------------------------------------------------
# Info structs
# ---------------------------------------------------------------------

struct NetworkInfo {
  id           @0 :Int64;
  name         @1 :Text;
  subnet       @2 :Text;
  dhcp         @3 :Bool;
  nat          @4 :Bool;
  running      @5 :Bool;
  dnsmasqPid   @6 :Int32;   # 0 == not running
  createdAt    @7 :Int64;   # POSIX nanoseconds
  autostart    @8 :Bool;
  vni          @9 :Int32;   # 0 == none (single-node network)
  peerNodeIds  @10 :List(Int64);
  # DNS servers advertised to DHCP clients (option 6). Empty when
  # the network either has DHCP disabled or has no DNS configured.
  dnsServers   @11 :List(Text);
  # The DNS suffix the dnsmasq instance is (or would be) authoritative
  # for. Effective value: explicit `domain` if set, otherwise the
  # network's own name. Empty here ONLY for networks where DNS is
  # impossible (no DHCP, no subnet) — the daemon resolves the default
  # before returning it.
  domain       @12 :Text;
  # Whether the host installed a systemd-resolved drop-in routing
  # `~<domain>` to this network's bridge IP. False either by operator
  # choice or because the network has no domain to forward.
  hostDns      @13 :Bool;
}

# Inputs for attach-node / detach-node.
struct NetworkPeerParams {
  node @0 :Common.EntityRef;
}

# ---------------------------------------------------------------------
# Parameter structs
# ---------------------------------------------------------------------

struct NetworkCreateParams {
  # `name` and `subnet` are mandatory; the toggles default off to
  # match `crv network create`.
  name      @0 :Text;
  subnet    @1 :Text;
  dhcp      @2 :Bool = false;
  nat       @3 :Bool = false;
  autostart @4 :Bool = false;
  # Node this network is bound to (multi-node slice 1c).
  node      @5 :Common.EntityRef;
  # DNS servers to advertise via DHCP option 6 (empty = none).
  dnsServers @6 :List(Text);
  # DNS suffix dnsmasq is authoritative for (e.g. `corvus`). Empty
  # → daemon defaults it to the network's `name`.
  domain     @7 :Text;
  # Install a systemd-resolved drop-in on the network's owner so
  # `*.<domain>` resolves to the bridge IP. Default true; the
  # daemon flips it off if the network has no chance of serving DNS.
  hostDns    @8 :Bool = true;
}

struct NetworkEditParams {
  hasName       @0 :Bool;
  name          @1 :Text;
  hasSubnet     @2 :Bool;
  subnet        @3 :Text;
  hasDhcp       @4 :Bool;
  dhcp          @5 :Bool;
  hasNat        @6 :Bool;
  nat           @7 :Bool;
  hasAutostart  @8 :Bool;
  autostart     @9 :Bool;
  # Set hasDnsServers=true to overwrite the list with the value of
  # dnsServers (an empty list clears the option entirely).
  hasDnsServers @10 :Bool;
  dnsServers    @11 :List(Text);
  hasDomain     @12 :Bool;
  domain        @13 :Text;
  hasHostDns    @14 :Bool;
  hostDns       @15 :Bool;
}

# ---------------------------------------------------------------------
# Manager + resource capabilities
# ---------------------------------------------------------------------

interface NetworkManager {
  list    @0 () -> (networks :List(NetworkInfo));
  get     @1 (ref :Common.EntityRef) -> (network :Network);
  create  @2 (params :NetworkCreateParams) -> (network :Network);
}

interface Network {
  show         @0 () -> (info :NetworkInfo);
  start        @1 () -> ();
  # `force` defaults to false to match `crv network stop`.
  stop         @2 (force :Bool = false) -> ();
  edit         @3 (params :NetworkEditParams) -> ();
  delete       @4 () -> ();
  attachNode   @5 (params :NetworkPeerParams) -> ();
  detachNode   @6 (params :NetworkPeerParams) -> ();
}
