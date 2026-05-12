@0xeb6a435f11477f84;

using Corvus = import "corvus.capnp";

# ---------------------------------------------------------------------
# Info struct
# ---------------------------------------------------------------------

struct CloudInitInfo {
  hasUserData       @0 :Bool;
  userData          @1 :Text;
  hasNetworkConfig  @2 :Bool;
  networkConfig     @3 :Text;
  injectSshKeys     @4 :Bool;
}

# ---------------------------------------------------------------------
# Parameter struct
# ---------------------------------------------------------------------

struct CloudInitSetParams {
  vmRef          @0 :Corvus.EntityRef;
  config         @1 :CloudInitInfo;
}

# ---------------------------------------------------------------------
# Manager capability (CloudInit configs are owned by their VM;
# no per-config resource cap is needed)
# ---------------------------------------------------------------------

interface CloudInitManager {
  set    @0 (params :CloudInitSetParams) -> ();
  get    @1 (vmRef :Corvus.EntityRef) -> (config :CloudInitInfo);
  delete @2 (vmRef :Corvus.EntityRef) -> ();
}
