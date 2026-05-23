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
